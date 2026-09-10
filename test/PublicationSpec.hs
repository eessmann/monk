{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent qualified as Concurrent
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef qualified as IORef
import Data.List qualified as L
import Monk.Output.Publication
  ( MemberRole (..),
    ObservedEntry (..),
    PublicationFailure,
    PublicationFailureKind (..),
    PublicationHook,
    PublicationMember (..),
    PublicationPlan,
    PublicationReceipt,
    PublicationStage (..),
    generationRelativeDirectory,
    generationRelativeDirectoryMembers,
    mkPublicationHook,
    planManagedPublication,
    planManagedPublicationMembers,
    planSingleFilePublication,
    publicationFailureKind,
    publicationFailureMessage,
    publicationFailureObservedEntry,
    publicationReceiptDestination,
    publicationReceiptGeneration,
    publicationReceiptWarnings,
    publishPublication,
    publishPublicationWithHook,
  )
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment qualified as Environment
import System.Exit (ExitCode (..))
import System.FilePath qualified as FP
import System.IO qualified as IO
import System.Posix.Files (createSymbolicLink, setFileMode)
import System.Posix.Process (ProcessStatus (Exited), executeFile, exitImmediately, forkProcess, getProcessStatus)
import System.Posix.Types (ProcessID)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as H

main :: IO ()
main = do
  arguments <- Environment.getArgs
  case arguments of
    ["--publication-worker", destination, value, firstParent, ready, release] ->
      publicationWorker destination (BS8.pack value) firstParent ready release
    _ -> defaultMain publicationTests

publicationTests :: TestTree
publicationTests =
  testGroup
    "Publication"
    [ validationTests,
      managedPublicationTests,
      failureRecoveryTests,
      concurrencyTests,
      singleFileTests
    ]

validationTests :: TestTree
validationTests =
  testGroup
    "validation"
    [ H.testCase "generation layout is deterministic and member-order independent" $ do
        let destination = "/tmp/out/root.fish"
            members = [("root.fish", "root"), ("lib/child.fish", "child")]
        generationRelativeDirectory destination members
          @?= generationRelativeDirectory destination (reverse members),
      H.testCase "generation layout changes when complete member bytes change" $ do
        let destination = "/tmp/out/root.fish"
            before = generationRelativeDirectory destination [("root.fish", "before")]
            after = generationRelativeDirectory destination [("root.fish", "after")]
        H.assertBool "generation label ignored member bytes" (before /= after),
      H.testCase "generation identity includes artifact role and executable mode" $ do
        let script = generationRelativeDirectoryMembers "/tmp/root.fish" [PublicationMember "member" FishSource "same-bytes"]
            executable = generationRelativeDirectoryMembers "/tmp/root.fish" [PublicationMember "member" NativeExecutable "same-bytes"]
        H.assertBool "generation ignored member role and mode" (script /= executable),
      H.testCase "managed planning rejects duplicate member paths" $
        assertPlanFailure
          InvalidPublicationPlan
          (planManagedPublication "/tmp/root.fish" [("root.fish", "one"), ("root.fish", "two")] "loader"),
      H.testCase "managed planning rejects traversal" $
        assertPlanFailure
          InvalidPublicationPlan
          (planManagedPublication "/tmp/root.fish" [("../escape.fish", "bad")] "loader"),
      H.testCase "managed planning rejects non-normal member paths" $
        assertPlanFailure
          InvalidPublicationPlan
          (planManagedPublication "/tmp/root.fish" [("lib/../root.fish", "bad")] "loader"),
      H.testCase "managed planning rejects file-directory member collisions" $
        assertPlanFailure
          InvalidPublicationPlan
          (planManagedPublication "/tmp/root.fish" [("lib", "file"), ("lib/child.fish", "child")] "loader"),
      H.testCase "planning rejects a destination without an entry basename" $
        assertPlanFailure
          InvalidPublicationPlan
          (planSingleFilePublication "/" "bad")
    ]

managedPublicationTests :: TestTree
managedPublicationTests =
  testGroup
    "managed generations"
    [ H.testCase "publishes complete immutable generation before loader" $
        withTempDir "monk-publication" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root-v1"), ("lib/child.fish", "child-v1")]
          plan <- managedPlan destination members
          receipt <- successfulPublish plan
          generation <- requireGeneration receipt
          publicationReceiptDestination receipt @?= destination
          publicationReceiptWarnings receipt @?= []
          assertFileBytes destination (loaderFor generation)
          forM_ members $ \(path, contents) ->
            assertFileBytes (tmpDir FP.</> generation FP.</> path) contents,
      H.testCase "invalid staged runtime preserves the published entry" $
        withTempDir "monk-invalid-native" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          void (successfulPublish =<< managedPlan destination [("entry.fish", "old")])
          previous <- BS.readFile destination
          plan <- requirePlan (planManagedPublicationMembers destination [PublicationMember "entry.fish" FishSource "new", PublicationMember "bin/monk-runtime" NativeExecutable "invalid-executable"] "new-loader")
          failed <- publishPublication plan
          assertPublishFailure InvalidPublicationPlan failed
          assertFileBytes destination previous,
      H.testCase "reuses only a byte-for-byte matching generation" $
        withTempDir "monk-publication-reuse" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root"), ("lib/child.fish", "child")]
          plan <- managedPlan destination members
          firstReceipt <- successfulPublish plan
          secondReceipt <- successfulPublish plan
          publicationReceiptGeneration secondReceipt @?= publicationReceiptGeneration firstReceipt
          generation <- requireGeneration firstReceipt
          entries <- listDirectory (tmpDir FP.</> FP.takeDirectory generation)
          length (filter (not . L.isPrefixOf ".") entries) @?= 1,
      H.testCase "reports a digest collision when existing generation content differs" $
        withTempDir "monk-publication-collision" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root"), ("lib/child.fish", "child")]
          plan <- managedPlan destination members
          receipt <- successfulPublish plan
          generation <- requireGeneration receipt
          BS.writeFile (tmpDir FP.</> generation FP.</> "lib/child.fish") "tampered"
          failed <- publishPublication plan
          assertPublishFailure GenerationCollision failed,
      H.testCase "reports a digest collision when an existing generation has extra content" $
        withTempDir "monk-publication-extra-content" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root")]
          plan <- managedPlan destination members
          receipt <- successfulPublish plan
          generation <- requireGeneration receipt
          BS.writeFile (tmpDir FP.</> generation FP.</> "extra") "unexpected"
          result <- publishPublication plan
          assertPublishFailure GenerationCollision result,
      H.testCase "retains the old generation after publishing a replacement" $
        withTempDir "monk-publication-retain" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          firstReceipt <- successfulPublish =<< managedPlan destination [("root.fish", "one")]
          secondReceipt <- successfulPublish =<< managedPlan destination [("root.fish", "two")]
          firstGeneration <- requireGeneration firstReceipt
          secondGeneration <- requireGeneration secondReceipt
          H.assertBool "replacement reused changed generation" (firstGeneration /= secondGeneration)
          assertFileBytes (tmpDir FP.</> firstGeneration FP.</> "root.fish") "one"
          assertFileBytes (tmpDir FP.</> secondGeneration FP.</> "root.fish") "two"
          assertFileBytes destination (loaderFor secondGeneration),
      H.testCase "rejects an unmanaged directory instead of adopting it" $
        withTempDir "monk-publication-owner" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root")]
          relativeGeneration <- successfulLayout destination members
          createDirectory (managedRoot tmpDir relativeGeneration)
          result <- publishPublication =<< managedPlan destination members
          assertPublishFailure OwnershipMismatch result,
      H.testCase "rejects managed-directory symlinks" $
        withTempDir "monk-publication-symlink-root" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root")]
              elsewhere = tmpDir FP.</> "elsewhere"
          relativeGeneration <- successfulLayout destination members
          createDirectory elsewhere
          createSymbolicLink elsewhere (managedRoot tmpDir relativeGeneration)
          result <- publishPublication =<< managedPlan destination members
          assertPublishFailure SymlinkConflict result,
      H.testCase "rejects ownership-marker symlinks" $
        withTempDir "monk-publication-symlink-marker" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root")]
          plan <- managedPlan destination members
          receipt <- successfulPublish plan
          generation <- requireGeneration receipt
          let marker = managedRoot tmpDir generation FP.</> ".monk-owner"
              target = tmpDir FP.</> "marker-target"
          removeFile marker
          BS.writeFile target "monk-managed-output-v1\n"
          createSymbolicLink target marker
          result <- publishPublication plan
          assertPublishFailure SymlinkConflict result,
      H.testCase "rejects symlinks inside an existing generation" $
        withTempDir "monk-publication-symlink-member" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root"), ("lib/child.fish", "child")]
          plan <- managedPlan destination members
          receipt <- successfulPublish plan
          generation <- requireGeneration receipt
          let child = tmpDir FP.</> generation FP.</> "lib/child.fish"
              target = tmpDir FP.</> "child-target"
          removeFile child
          BS.writeFile target "child"
          createSymbolicLink target child
          result <- publishPublication plan
          assertPublishFailure SymlinkConflict result,
      H.testCase "rejects destination symlinks" $
        withTempDir "monk-publication-symlink-entry" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              target = tmpDir FP.</> "target.fish"
          BS.writeFile target "existing"
          createSymbolicLink target destination
          result <- publishPublication =<< managedPlan destination [("root.fish", "root")]
          assertPublishFailure SymlinkConflict result,
      H.testCase "rejects an ancestor directory symlink" $
        withTempDir "monk-publication-symlink-ancestor" $ \tmpDir -> do
          let actual = tmpDir FP.</> "actual"
              alias = tmpDir FP.</> "alias"
              destination = alias FP.</> "root.fish"
          createDirectory actual
          createSymbolicLink actual alias
          result <- publishPublication =<< managedPlan destination [("root.fish", "root")]
          assertPublishFailure SymlinkConflict result,
      H.testCase "rejects a symlink lock file" $
        withTempDir "monk-publication-symlink-lock" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              lockPath = tmpDir FP.</> ".root.fish.monk.lock"
              target = tmpDir FP.</> "lock-target"
          BS.writeFile target "lock"
          createSymbolicLink target lockPath
          result <- publishPublication =<< managedPlan destination [("root.fish", "root")]
          assertPublishFailure SymlinkConflict result,
      H.testCase "returns a typed failure for an unwritable lock file" $
        withTempDir "monk-publication-unwritable-lock" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              lockPath = tmpDir FP.</> ".root.fish.monk.lock"
          BS.writeFile lockPath "lock"
          setFileMode lockPath 0o000
          result <- publishPublication =<< managedPlan destination [("root.fish", "root")]
          assertPublishFailure PublicationIOFailure result,
      H.testCase "returns a typed failure when a destination parent is a file" $
        withTempDir "monk-publication-parent-file" $ \tmpDir -> do
          let parent = tmpDir FP.</> "not-a-directory"
              destination = parent FP.</> "root.fish"
          BS.writeFile parent "file"
          result <- publishPublication =<< managedPlan destination [("root.fish", "root")]
          assertPublishFailure InvalidPublicationPlan result,
      H.testCase "flushes each newly created destination parent before its child" $
        withTempDir "monk-publication-parent-order" $ \tmpDir -> do
          let firstParent = tmpDir FP.</> "new"
              secondParent = firstParent FP.</> "nested"
              destination = secondParent FP.</> "root.fish"
          observed <- IORef.newIORef []
          let hook =
                mkPublicationHook $ \stage -> do
                  IORef.modifyIORef' observed (<> [stage])
                  pure Nothing
          result <- publishPublicationWithHook hook =<< managedPlan destination [("root.fish", "root")]
          assertPublishSuccess result
          stages <- IORef.readIORef observed
          take 6 stages
            @?= [ StageBeforeDestinationDirectoryCreate firstParent,
                  StageAfterDestinationDirectoryCreate firstParent,
                  StageAfterDestinationDirectoryParentFlush tmpDir,
                  StageBeforeDestinationDirectoryCreate secondParent,
                  StageAfterDestinationDirectoryCreate secondParent,
                  StageAfterDestinationDirectoryParentFlush firstParent
                ]
    ]

failureRecoveryTests :: TestTree
failureRecoveryTests =
  testGroup
    "failure recovery"
    [ H.testCase "member-flush failure preserves the old entry" $
        withTempDir "monk-publication-member-failure" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          oldReceipt <- successfulPublish =<< managedPlan destination [("root.fish", "old")]
          oldLoader <- BS.readFile destination
          replacement <- managedPlan destination [("root.fish", "new")]
          result <- publishPublicationWithHook (failAt (StageAfterMemberFlush "root.fish")) replacement
          assertPublishFailure InjectedPublicationFailure result
          assertFileBytes destination oldLoader
          oldGeneration <- requireGeneration oldReceipt
          assertFileBytes (tmpDir FP.</> oldGeneration FP.</> "root.fish") "old",
      H.testCase "member-write failure leaves the old entry unchanged" $
        withTempDir "monk-publication-write-failure" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          void (successfulPublish =<< managedPlan destination [("root.fish", "old")])
          oldLoader <- BS.readFile destination
          replacement <- managedPlan destination [("root.fish", "new")]
          result <- publishPublicationWithHook (failAt (StageAfterMemberWrite "root.fish")) replacement
          assertPublishFailure InjectedPublicationFailure result
          assertFileBytes destination oldLoader,
      H.testCase "pre-staging failure creates no staging directory or entry" $
        withTempDir "monk-publication-before-staging" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root")]
          generation <- successfulLayout destination members
          result <- publishPublicationWithHook (failAt StageBeforeGenerationStaging) =<< managedPlan destination members
          assertPublishFailure InjectedPublicationFailure result
          exists <- doesFileExist destination
          H.assertBool "entry appeared before staging" (not exists)
          managedEntries <- listDirectory (managedRoot tmpDir generation)
          H.assertBool "staging directory was created before the hook" (not (any (L.isPrefixOf ".staging") managedEntries)),
      H.testCase "generation-flush failure leaves no new entry" $
        withTempDir "monk-publication-generation-failure" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          plan <- managedPlan destination [("root.fish", "root")]
          result <- publishPublicationWithHook (failAt StageAfterGenerationFlush) plan
          assertPublishFailure InjectedPublicationFailure result
          exists <- doesFileExist destination
          H.assertBool "entry appeared before generation was durable" (not exists),
      H.testCase "retry after generation-parent flush failure reflushes the verified generation before loader" $
        withTempDir "monk-publication-generation-parent-retry" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root"), ("lib/child.fish", "child")]
          generation <- successfulLayout destination members
          let generationsRoot = managedRoot tmpDir generation FP.</> "generations"
              parentFlush = StageBeforeGenerationParentFlush generationsRoot
          plan <- managedPlan destination members
          failed <- publishPublicationWithHook (failAt parentFlush) plan
          assertPublishFailure InjectedPublicationFailure failed
          H.assertBool "generation rename did not survive the failed parent flush" =<< doesDirectoryExist (tmpDir FP.</> generation)
          H.assertBool "loader appeared before the generation parent was durable" . not =<< doesFileExist destination
          observed <- IORef.newIORef []
          let retryHook stage = IORef.modifyIORef' observed (<> [stage]) >> pure Nothing
          assertPublishSuccess =<< publishPublicationWithHook (mkPublicationHook retryHook) plan
          stages <- IORef.readIORef observed
          let beforeReplacement = takeWhile (/= StageBeforeEntryReplace) stages
          H.assertBool "retry did not flush the verified root member" (StageAfterMemberFlush "root.fish" `elem` beforeReplacement)
          H.assertBool "retry did not flush the verified nested member" (StageAfterMemberFlush "lib/child.fish" `elem` beforeReplacement)
          H.assertBool "retry did not flush the verified generation tree" (StageAfterGenerationFlush `elem` beforeReplacement)
          H.assertBool "retry did not flush the generations parent" (parentFlush `elem` beforeReplacement)
          assertFileBytes destination (loaderFor generation),
      H.testCase "pre-replace failure keeps a newly published generation unreferenced" $
        withTempDir "monk-publication-before-replace" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          plan <- managedPlan destination [("root.fish", "root")]
          result <- publishPublicationWithHook (failAt StageBeforeEntryReplace) plan
          assertPublishFailure InjectedPublicationFailure result
          exists <- doesFileExist destination
          H.assertBool "entry was replaced despite pre-replace failure" (not exists),
      H.testCase "post-generation-publish failure retains an unreferenced generation" $
        withTempDir "monk-publication-after-generation" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
              members = [("root.fish", "root")]
          generation <- successfulLayout destination members
          plan <- managedPlan destination members
          result <- publishPublicationWithHook (failAt StageAfterGenerationPublish) plan
          assertPublishFailure InjectedPublicationFailure result
          exists <- doesFileExist destination
          H.assertBool "entry appeared before replacement" (not exists)
          H.assertBool "published generation was removed" =<< doesDirectoryExist (tmpDir FP.</> generation),
      H.testCase "post-replace failure reports the observed planned entry" $
        withTempDir "monk-publication-after-replace" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          plan <- managedPlan destination [("root.fish", "root")]
          result <- publishPublicationWithHook (failAt StageAfterEntryReplace) plan
          failure <- requireFailure result
          publicationFailureKind failure @?= InjectedPublicationFailure
          publicationFailureObservedEntry failure @?= Just ObservedEntryMatchesPlanned,
      H.testCase "post-parent-flush failure inspects the actual entry" $
        withTempDir "monk-publication-parent-failure" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          plan <- managedPlan destination [("root.fish", "root")]
          result <- publishPublicationWithHook (failAt StageAfterParentFlush) plan
          failure <- requireFailure result
          publicationFailureKind failure @?= InjectedPublicationFailure
          publicationFailureObservedEntry failure @?= Just ObservedEntryMatchesPlanned
    ]

concurrencyTests :: TestTree
concurrencyTests =
  testGroup
    "concurrency"
    [ H.testCase "same-destination writers serialize" $
        withTempDir "monk-publication-writers" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          firstPlan <- managedPlan destination [("root.fish", "one")]
          secondPlan <- managedPlan destination [("root.fish", "two")]
          firstEntered <- Concurrent.newEmptyMVar
          releaseFirst <- Concurrent.newEmptyMVar
          secondEntered <- Concurrent.newEmptyMVar
          firstDone <- Concurrent.newEmptyMVar
          secondDone <- Concurrent.newEmptyMVar
          let firstHook =
                mkPublicationHook $ \stage -> do
                  when (stage == StageBeforeEntryReplace) $ do
                    Concurrent.putMVar firstEntered ()
                    Concurrent.takeMVar releaseFirst
                  pure Nothing
              secondHook =
                mkPublicationHook $ \stage -> do
                  when (stage == StageBeforeEntryReplace) (Concurrent.putMVar secondEntered ())
                  pure Nothing
          void (Concurrent.forkIO (publishPublicationWithHook firstHook firstPlan >>= Concurrent.putMVar firstDone))
          Concurrent.takeMVar firstEntered
          void (Concurrent.forkIO (publishPublicationWithHook secondHook secondPlan >>= Concurrent.putMVar secondDone))
          Concurrent.threadDelay 50000
          blocked <- Concurrent.isEmptyMVar secondEntered
          H.assertBool "second writer entered publication while first held the lock" blocked
          Concurrent.putMVar releaseFirst ()
          assertPublishSuccess =<< Concurrent.takeMVar firstDone
          assertPublishSuccess =<< Concurrent.takeMVar secondDone
          void (Concurrent.takeMVar secondEntered),
      H.testCase "separate processes serialize first publication through missing parents" $
        withTempDir "monk-publication-process-writers" $ \tmpDir -> do
          let firstParent = tmpDir FP.</> "new"
              destination = firstParent FP.</> "nested" FP.</> "root.fish"
              firstReady = tmpDir FP.</> "first.ready"
              secondReady = tmpDir FP.</> "second.ready"
              release = tmpDir FP.</> "release"
          firstPid <- forkPublisher destination "one" firstParent firstReady release
          secondPid <- forkPublisher destination "two" firstParent secondReady release
          waitForFiles [firstReady, secondReady]
          BS.writeFile release "go"
          waitForSuccessfulProcess firstPid
          waitForSuccessfulProcess secondPid
          firstGeneration <- successfulLayout destination [("root.fish", "one")]
          secondGeneration <- successfulLayout destination [("root.fish", "two")]
          H.assertBool "first generation missing" =<< doesDirectoryExist (FP.takeDirectory destination FP.</> firstGeneration)
          H.assertBool "second generation missing" =<< doesDirectoryExist (FP.takeDirectory destination FP.</> secondGeneration)
          loader <- BS.readFile destination
          H.assertBool "entry does not name either complete generation" (loader `elem` [loaderFor firstGeneration, loaderFor secondGeneration]),
      H.testCase "concurrent readers see an old or new complete generation" $
        withTempDir "monk-publication-readers" $ \tmpDir -> do
          let destination = tmpDir FP.</> "root.fish"
          oldGenerationPath <- successfulLayout destination [("value", "old")]
          oldPlan <-
            requirePlan
              ( planManagedPublication
                  destination
                  [("value", "old")]
                  (BS8.pack (oldGenerationPath FP.</> "value"))
              )
          oldReceipt <- successfulPublish oldPlan
          oldGeneration <- requireGeneration oldReceipt
          newGeneration <- successfulLayout destination [("value", "new")]
          newPlan <-
            requirePlan
              ( planManagedPublication
                  destination
                  [("value", "new")]
                  (BS8.pack (newGeneration FP.</> "value"))
              )
          replaceEntered <- Concurrent.newEmptyMVar
          releaseReplace <- Concurrent.newEmptyMVar
          writerDone <- Concurrent.newEmptyMVar
          let hook =
                mkPublicationHook $ \stage -> do
                  when (stage == StageBeforeEntryReplace) $ do
                    Concurrent.putMVar replaceEntered ()
                    Concurrent.takeMVar releaseReplace
                  pure Nothing
          void (Concurrent.forkIO (publishPublicationWithHook hook newPlan >>= Concurrent.putMVar writerDone))
          Concurrent.takeMVar replaceEntered
          before <- replicateM 100 (readPinnedValue tmpDir destination)
          Concurrent.putMVar releaseReplace ()
          result <- Concurrent.takeMVar writerDone
          assertPublishSuccess result
          after <- replicateM 100 (readPinnedValue tmpDir destination)
          H.assertBool "reader observed incomplete generation" (all (`elem` ["old", "new"]) (before <> after))
          H.assertBool "reader did not observe the old generation" ("old" `elem` before)
          H.assertBool "reader did not observe the new generation" ("new" `elem` after)
          H.assertBool "old generation was not retained" =<< doesDirectoryExist (tmpDir FP.</> oldGeneration)
    ]

singleFileTests :: TestTree
singleFileTests =
  testGroup
    "single file"
    [ H.testCase "atomically replaces a single file" $
        withTempDir "monk-publication-single" $ \tmpDir -> do
          let destination = tmpDir FP.</> "output.fish"
          BS.writeFile destination "old"
          receipt <- successfulPublish =<< requirePlan (planSingleFilePublication destination "new")
          publicationReceiptDestination receipt @?= destination
          publicationReceiptGeneration receipt @?= Nothing
          assertFileBytes destination "new",
      H.testCase "single-file pre-replace failure preserves old bytes" $
        withTempDir "monk-publication-single-failure" $ \tmpDir -> do
          let destination = tmpDir FP.</> "output.fish"
          BS.writeFile destination "old"
          plan <- requirePlan (planSingleFilePublication destination "new")
          result <- publishPublicationWithHook (failAt StageBeforeEntryReplace) plan
          assertPublishFailure InjectedPublicationFailure result
          assertFileBytes destination "old"
    ]

managedPlan :: FilePath -> [(FilePath, BS.ByteString)] -> IO PublicationPlan
managedPlan destination members = do
  generation <- successfulLayout destination members
  requirePlan (planManagedPublication destination members (loaderFor generation))

successfulLayout :: FilePath -> [(FilePath, BS.ByteString)] -> IO FilePath
successfulLayout destination members =
  case generationRelativeDirectory destination members of
    Left failure -> H.assertFailure (showFailure failure) >> error "unreachable"
    Right generation -> pure generation

loaderFor :: FilePath -> BS.ByteString
loaderFor generation = BS8.pack (generation FP.</> "root.fish")

managedRoot :: FilePath -> FilePath -> FilePath
managedRoot parent relativeGeneration =
  parent FP.</> FP.takeDirectory (FP.takeDirectory relativeGeneration)

readPinnedValue :: FilePath -> FilePath -> IO BS.ByteString
readPinnedValue parent destination = do
  relativeMember <- BS8.unpack <$> BS.readFile destination
  BS.readFile (parent FP.</> relativeMember)

failAt :: PublicationStage -> PublicationHook
failAt expected =
  mkPublicationHook $ \actual ->
    pure
      ( if actual == expected
          then Just "injected test failure"
          else Nothing
      )

forkPublisher :: FilePath -> String -> FilePath -> FilePath -> FilePath -> IO ProcessID
forkPublisher destination value firstParent ready release = do
  executable <- Environment.getExecutablePath
  forkProcess
    ( executeFile
        executable
        False
        ["--publication-worker", destination, value, firstParent, ready, release]
        Nothing
    )

publicationWorker :: FilePath -> BS.ByteString -> FilePath -> FilePath -> FilePath -> IO ()
publicationWorker destination value firstParent ready release = do
  plan <- managedPlan destination [("root.fish", value)]
  let hook =
        mkPublicationHook $ \stage -> do
          when (stage == StageBeforeDestinationDirectoryCreate firstParent) $ do
            BS.writeFile ready "ready"
            waitForFiles [release]
          pure Nothing
  result <- publishPublicationWithHook hook plan
  exitImmediately (if isRight result then ExitSuccess else ExitFailure 1)

waitForFiles :: [FilePath] -> IO ()
waitForFiles paths = go (500 :: Int)
  where
    go remaining
      | remaining <= 0 = H.assertFailure ("timed out waiting for files: " <> show paths)
      | otherwise = do
          ready <- and <$> traverse doesFileExist paths
          unless ready (Concurrent.threadDelay 10000 >> go (remaining - 1))

waitForSuccessfulProcess :: ProcessID -> IO ()
waitForSuccessfulProcess processId = do
  status <- getProcessStatus True False processId
  status @?= Just (Exited ExitSuccess)

assertPlanFailure :: PublicationFailureKind -> Either PublicationFailure PublicationPlan -> H.Assertion
assertPlanFailure expected result =
  case result of
    Left failure -> publicationFailureKind failure @?= expected
    Right _ -> H.assertFailure "publication planning unexpectedly succeeded"

assertPublishFailure :: PublicationFailureKind -> Either PublicationFailure PublicationReceipt -> H.Assertion
assertPublishFailure expected result = do
  failure <- requireFailure result
  publicationFailureKind failure @?= expected

assertPublishSuccess :: Either PublicationFailure PublicationReceipt -> H.Assertion
assertPublishSuccess result = void (requireReceipt result)

successfulPublish :: PublicationPlan -> IO PublicationReceipt
successfulPublish plan = publishPublication plan >>= requireReceipt

requirePlan :: Either PublicationFailure PublicationPlan -> IO PublicationPlan
requirePlan = \case
  Left failure -> H.assertFailure (showFailure failure) >> error "unreachable"
  Right plan -> pure plan

requireReceipt :: Either PublicationFailure PublicationReceipt -> IO PublicationReceipt
requireReceipt = \case
  Left failure -> H.assertFailure (showFailure failure) >> error "unreachable"
  Right receipt -> pure receipt

requireFailure :: Either PublicationFailure PublicationReceipt -> IO PublicationFailure
requireFailure = \case
  Left failure -> pure failure
  Right _ -> H.assertFailure "publication unexpectedly succeeded" >> error "unreachable"

requireGeneration :: PublicationReceipt -> IO FilePath
requireGeneration receipt =
  case publicationReceiptGeneration receipt of
    Nothing -> H.assertFailure "managed publication returned no generation" >> error "unreachable"
    Just generation -> pure generation

showFailure :: PublicationFailure -> String
showFailure failure =
  show (publicationFailureKind failure)
    <> ": "
    <> toString (publicationFailureMessage failure)

assertFileBytes :: FilePath -> BS.ByteString -> H.Assertion
assertFileBytes path expected = do
  actual <- BS.readFile path
  actual @?= expected

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  systemTemp <- getTemporaryDirectory
  let create = do
        (path, handle) <- IO.openBinaryTempFile systemTemp prefix
        IO.hClose handle
        removeFile path
        createDirectory path
        pure path
  bracket create cleanup action
  where
    cleanup path = do
      exists <- doesDirectoryExist path
      when exists (removeDirectoryRecursive path)

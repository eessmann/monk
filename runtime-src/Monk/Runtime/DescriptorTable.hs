{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Virtual source descriptor numbers backed by separately owned POSIX open
-- file descriptions. Scope copies and dup preserve shared offsets; closing one
-- owned reference never closes its aliases or a caller's borrowed descriptor.
module Monk.Runtime.DescriptorTable
  ( DescriptorTable,
    newDescriptorTable,
    closeDescriptorTable,
    pushDescriptors,
    popDescriptors,
    resetDescriptors,
    descriptorBindings,
    resolveDescriptor,
    mergedDescriptors,
    openDescriptor,
    openDescriptorAt,
    duplicateDescriptor,
    closeDescriptor,
    dataDescriptor,
    installDescriptors,
  )
where

import Control.Exception (IOException, bracket, catch, mask_, onException)
import Control.Monad (forM_, unless, void)
import Data.ByteString qualified as B
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.Map.Strict qualified as M
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import GHC.Foreign qualified as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import Monk.Runtime.Descriptors (duplicatePrivate, duplicatePrivateAbove, privateCloseOnExec)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Posix.IO (FdOption (CloseOnExec), OpenFileFlags (append, creat, trunc), OpenMode (ReadOnly, ReadWrite, WriteOnly), closeFd, defaultFileFlags, dupTo, setFdOption)
import System.Posix.IO.ByteString (openFd)
import System.Posix.Types (Fd (..))

newtype DescriptorTable = DescriptorTable (IORef [M.Map Int (Maybe Fd)])

newDescriptorTable :: [(Int, Fd)] -> IO DescriptorTable
newDescriptorTable inherited = do
  owned <- copyBindings (M.fromList [(number, Just fd) | (number, fd) <- inherited])
  DescriptorTable <$> newIORef [owned]

closeDescriptorTable :: DescriptorTable -> IO ()
closeDescriptorTable (DescriptorTable reference) = readIORef reference >>= mapM_ closeBindings

pushDescriptors :: DescriptorTable -> IO ()
pushDescriptors table@(DescriptorTable reference) = mask_ $ do
  current <- descriptorBindings table
  copied <- copyBindings current
  atomicModifyIORef' reference (\stack -> (copied : stack, ()))

popDescriptors :: DescriptorTable -> Int -> IO ()
popDescriptors (DescriptorTable reference) count = mask_ $ do
  stack <- readIORef reference
  unless (count >= 0 && count < length stack) (ioError (userError "invalid descriptor scope pop"))
  removed <- atomicModifyIORef' reference (\current -> let (closed, kept) = splitAt count current in (kept, closed))
  mapM_ closeBindings removed

resetDescriptors :: DescriptorTable -> IO ()
resetDescriptors table@(DescriptorTable reference) = do
  stack <- readIORef reference
  popDescriptors table (length stack - 1)

descriptorBindings :: DescriptorTable -> IO (M.Map Int (Maybe Fd))
descriptorBindings (DescriptorTable reference) = do
  stack <- readIORef reference
  case stack of
    current : _ -> pure current
    [] -> ioError (userError "missing descriptor scope")

resolveDescriptor :: DescriptorTable -> M.Map Int Fd -> Int -> IO (Maybe Fd)
resolveDescriptor table inherited number = do
  bindings <- descriptorBindings table
  pure (M.findWithDefault (M.lookup number inherited) number bindings)

openDescriptor :: DescriptorTable -> Int -> B.ByteString -> B.ByteString -> IO ()
openDescriptor table number mode path = do
  validNumber number
  (access, flags) <- case mode of
    "read" -> pure (ReadOnly, defaultFileFlags)
    "write" -> pure (WriteOnly, defaultFileFlags {creat = Just 0o666, trunc = True})
    "append" -> pure (WriteOnly, defaultFileFlags {creat = Just 0o666, append = True})
    "read-write" -> pure (ReadWrite, defaultFileFlags {creat = Just 0o666})
    _ -> ioError (userError "invalid descriptor open mode")
  fd <- bracket (openFd path access flags) closeFd duplicatePrivate
  replaceDescriptor table number (Just fd)

foreign import ccall interruptible "monk_open_at" openAtNative :: CInt -> CString -> CInt -> IO CInt

-- | Relative lookup is anchored to the caller's directory identity, even when
-- that directory no longer has a pathname. The descriptor owns the actual open.
openDescriptorAt :: DescriptorTable -> Int -> B.ByteString -> B.ByteString -> Fd -> IO ()
openDescriptorAt table number mode path (Fd directory) = do
  validNumber number
  access <- case mode of
    "read" -> pure 0
    "write" -> pure 1
    "append" -> pure 2
    "read-write" -> pure 3
    _ -> ioError (userError "invalid descriptor open mode")
  fd <- B.useAsCString path (\value -> Fd <$> throwErrnoIfMinus1Retry "open relative descriptor" (openAtNative directory value access))
  replaceDescriptor table number (Just fd)

duplicateDescriptor :: DescriptorTable -> M.Map Int Fd -> Int -> Int -> IO ()
duplicateDescriptor table inherited target source = do
  validNumber target
  original <- resolveDescriptor table inherited source >>= maybe (ioError (userError "closed source descriptor")) pure
  owned <- duplicatePrivate original
  replaceDescriptor table target (Just owned)

closeDescriptor :: DescriptorTable -> Int -> IO ()
closeDescriptor table number = validNumber number >> replaceDescriptor table number Nothing

dataDescriptor :: DescriptorTable -> Int -> B.ByteString -> IO ()
dataDescriptor table number bytes = do
  validNumber number
  directory <- getTemporaryDirectory
  fd <- bracket (openBinaryTempFile directory "monk-input") (\(path, handle) -> (hClose handle `catch` ignore) >> (removeFile path `catch` ignore)) $ \(path, handle) -> do
    B.hPut handle bytes
    hClose handle
    encoded <- nativePath path
    bracket (openFd encoded ReadOnly defaultFileFlags) closeFd $ \reader -> do
      removeFile path
      duplicatePrivate reader
  replaceDescriptor table number (Just fd)

-- | Install in a freshly forked child. Duplicate every source above all target
-- numbers before dup2, so cycles and descriptor-number reuse cannot clobber an
-- input. Only semantic targets survive exec, including explicit fd>2.
mergedDescriptors :: DescriptorTable -> M.Map Int Fd -> IO (M.Map Int Fd)
mergedDescriptors table inherited = do
  bindings <- descriptorBindings table
  pure (M.mapMaybe id (M.union bindings (M.map Just inherited)))

installDescriptors :: M.Map Int Fd -> IO ()
installDescriptors desired = do
  let closed = [number | number <- [0, 1, 2], M.notMember number desired]
      minimumFd = maximum (10 : map (+ 1) (M.keys desired))
      protect [] result = pure (reverse result)
      protect ((number, original) : rest) result = do
        copied <- duplicatePrivateAbove original minimumFd `onException` mapM_ (closeQuiet . snd) result
        protect rest ((number, copied) : result)
  protected <- protect (M.toList desired) []
  ( do
      privateCloseOnExec
      forM_ closed (closeQuiet . Fd . fromIntegral)
      forM_ protected $ \(number, original) -> do
        let target = Fd (fromIntegral number)
        void (dupTo original target)
        setFdOption target CloseOnExec False
    )
    `onException` mapM_ (closeQuiet . snd) protected
    <* mapM_ (closeQuiet . snd) protected

replaceDescriptor :: DescriptorTable -> Int -> Maybe Fd -> IO ()
replaceDescriptor (DescriptorTable reference) number descriptor = mask_ $ do
  previous <- atomicModifyIORef' reference $ \case
    current : rest -> (M.insert number descriptor current : rest, M.lookup number current)
    [] -> ([M.singleton number descriptor], Nothing)
  mapM_ (mapM_ closeQuiet) previous

copyBindings :: M.Map Int (Maybe Fd) -> IO (M.Map Int (Maybe Fd))
copyBindings bindings = go (M.toList bindings) M.empty
  where
    go [] result = pure result
    go ((number, descriptor) : rest) result = do
      copied <- traverse duplicatePrivate descriptor `onException` closeBindings result
      go rest (M.insert number copied result)

closeBindings :: M.Map Int (Maybe Fd) -> IO ()
closeBindings = mapM_ (mapM_ closeQuiet) . M.elems

validNumber :: Int -> IO ()
validNumber number = unless (number >= 0 && number < fromIntegral (maxBound :: Int32)) (ioError (userError "invalid source descriptor"))

closeQuiet :: Fd -> IO ()
closeQuiet fd = closeFd fd `catch` ignore

ignore :: IOException -> IO ()
ignore _ = pure ()

nativePath :: FilePath -> IO B.ByteString
nativePath path = getFileSystemEncoding >>= \encoding -> Foreign.withCString encoding path B.packCString

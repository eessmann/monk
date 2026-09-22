{-# LANGUAGE OverloadedStrings #-}

-- | Source-visible process substitution paths are aliases of real pipes, not
-- an independent-offset mechanism for script or state files.
module Monk.Runtime.PipePaths (probePipePaths) where

import Control.Exception (IOException, bracket, catch, mask_, onException)
import Control.Monad (unless, void, (>=>))
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (castPtr)
import Monk.Runtime.Descriptors (duplicatePrivate)
import Monk.Runtime.Session.Transport (closeSession)
import System.IO (hClose)
import System.Posix.Files (getFdStatus, isNamedPipe)
import System.Posix.IO (OpenMode (ReadOnly, WriteOnly), createPipe, defaultFileFlags, fdToHandle, fdWriteBuf)
import System.Posix.IO.ByteString (openFd)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)

probePipePaths :: IO Bool
probePipePaths = (fromMaybe False <$> timeout 1000000 probe) `catch` (\(_ :: IOException) -> pure False)
  where
    probe = bracket (newIORef []) (readIORef >=> mapM_ closeSession) $ \owned -> do
      let remember descriptor = atomicModifyIORef' owned (\fds -> (descriptor : fds, ())) >> pure descriptor
          acquire action = mask_ (action >>= remember)
          release descriptor = mask_ $ do
            atomicModifyIORef' owned (\fds -> (filter (/= descriptor) fds, ()))
            closeSession descriptor
      (reader, writer) <- mask_ $ do
        (input, output) <- createPipe
        _ <- remember input
        _ <- remember output
        pure (input, output)
      readAlias <- acquire (openFd (path reader) ReadOnly defaultFileFlags)
      writeAlias <- acquire (openFd (path writer) WriteOnly defaultFileFlags)
      actualPipe <- isNamedPipe <$> getFdStatus readAlias
      unless actualPipe (ioError (userError "descriptor path is not a pipe"))
      bracket (do copied <- duplicatePrivate readAlias; fdToHandle copied `onException` closeSession copied) hClose $ \handle -> do
        B.useAsCString "M" (\bytes -> void (fdWriteBuf writeAlias (castPtr bytes) 1))
        first <- B.hGet handle 1
        release writer
        release writeAlias
        eof <- B.hGet handle 1
        pure (first == "M" && B.null eof)
    path (Fd descriptor) = "/dev/fd/" <> C.pack (show descriptor)

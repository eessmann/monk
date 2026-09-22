{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parent-prepared POSIX spawning. Unlike forkProcess, no Haskell action or
-- garbage collection executes in the child before exec.
module Monk.Runtime.Spawn (spawnProcess, spawnProcessAt, spawnProcessAtMode, initialSignalIgnored, execProcess, openWorkingDirectory, spawnStatus) where

import Control.Exception (bracket, onException)
import Data.Bits (setBit)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Foreign.C.Error (Errno (..), eACCES, eNOENT, eNOTDIR, errnoToIOError, throwErrnoIfMinus1)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray, withArray0)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Monk.Runtime.Descriptors (duplicatePrivateAbove, privateCloseOnExec)
import System.Posix.IO (closeFd)
import System.Posix.Signals (Signal)
import System.Posix.Types (Fd (..), ProcessID)

foreign import ccall unsafe "monk_spawn" spawnNative :: CString -> Ptr CString -> Ptr CString -> CString -> CInt -> Ptr CInt -> Ptr CInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

spawnProcess :: M.Map Int Fd -> Maybe B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO ProcessID
spawnProcess descriptors cwd = spawnProcessWith False descriptors cwd Nothing

-- | Directory identity survives unlink and rename; no pathname reconstruction.
spawnProcessAt :: M.Map Int Fd -> Maybe Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO ProcessID
spawnProcessAt = spawnProcessAtMode False

-- | Asynchronous source commands ignore INT/QUIT before exec.
spawnProcessAtMode :: Bool -> M.Map Int Fd -> Maybe Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO ProcessID
spawnProcessAtMode asynchronous descriptors = spawnProcessWith asynchronous descriptors Nothing

foreign import ccall unsafe "monk_initial_signal_ignored" initialIgnoredNative :: CInt -> IO CInt

initialSignalIgnored :: Signal -> IO Bool
initialSignalIgnored signal = (/= 0) <$> initialIgnoredNative signal

spawnProcessWith :: Bool -> M.Map Int Fd -> Maybe B.ByteString -> Maybe Fd -> [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO ProcessID
spawnProcessWith asynchronous descriptors cwd cwdDescriptor environment command arguments = do
  privateCloseOnExec
  let minimumFd = maximum (10 : map (+ 1) (M.keys descriptors))
      duplicate [] owned = pure (reverse owned)
      duplicate ((target, source) : rest) owned = do
        fd <- duplicatePrivateAbove source minimumFd `onException` mapM_ (closeFd . snd) owned
        duplicate rest ((target, fd) : owned)
      closed = foldl setBit (0 :: Int) [fd | fd <- [0 .. 2], M.notMember fd descriptors]
      candidates = candidatePaths environment command
  bracket (duplicate (M.toList descriptors) []) (mapM_ (closeFd . snd)) $ \owned ->
    withStrings (command : arguments) $ \argv ->
      withStrings [name <> "=" <> value | (name, value) <- environment] $ \env ->
        maybe ($ nullPtr) B.useAsCString cwd $ \directory ->
          withArray [fromIntegral target | (target, _) <- owned] $ \targets ->
            withArray [fd | (_, Fd fd) <- owned] $ \sources ->
              alloca $ \pid -> do
                let attempt [] previous = ioError (errnoToIOError "spawn executable" previous Nothing Nothing)
                    attempt (path : rest) previous = do
                      result <- B.useAsCString path $ \executable -> spawnNative executable argv env directory (maybe (-1) (\(Fd fd) -> fd) cwdDescriptor) targets sources (fromIntegral (length owned)) (fromIntegral closed) (if asynchronous then 1 else 0) pid
                      if result == 0
                        then fromIntegral <$> peek pid
                        else do
                          let failure = Errno result
                          if not (B.elem 47 command) && (failure == eNOENT || failure == eNOTDIR || failure == eACCES)
                            then attempt rest (if failure == eACCES then failure else previous)
                            else ioError (errnoToIOError "spawn executable" failure Nothing Nothing)
                attempt candidates eNOENT

-- | Replace this primitive with the user's executable. It returns only when
-- every candidate failed, preserving EACCES precedence over absent PATH entries.
-- The caller has already restored semantic stdin/stdout/stderr.
foreign import ccall unsafe "monk_exec" execNative :: CString -> Ptr CString -> Ptr CString -> IO CInt

execProcess :: [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString] -> IO Errno
execProcess environment command arguments = do
  privateCloseOnExec
  withStrings (command : arguments) $ \argv ->
    withStrings [name <> "=" <> value | (name, value) <- environment] $ \env -> do
      let attempt [] previous = pure previous
          attempt (path : rest) previous = do
            failure <- Errno <$> B.useAsCString path (\executable -> execNative executable argv env)
            if not (B.elem 47 command) && (failure == eNOENT || failure == eNOTDIR || failure == eACCES)
              then attempt rest (if failure == eACCES then failure else previous)
              else pure failure
      attempt (candidatePaths environment command) eNOENT

candidatePaths :: [(B.ByteString, B.ByteString)] -> B.ByteString -> [B.ByteString]
candidatePaths environment command
  | B.elem 47 command = [command]
  | otherwise = [if B.null path then command else path <> "/" <> command | path <- C.split ':' (fromMaybe "/bin:/usr/bin" (lookup "PATH" environment))]

withStrings :: [B.ByteString] -> (Ptr CString -> IO a) -> IO a
withStrings values action = collect values []
  where
    collect [] pointers = withArray0 nullPtr (reverse pointers) action
    collect (value : rest) pointers = B.useAsCString value (\pointer -> collect rest (pointer : pointers))

foreign import ccall unsafe "monk_open_working_directory" workingDirectoryNative :: IO CInt

-- | A search-only directory capability, private and close-on-exec.
openWorkingDirectory :: IO Fd
openWorkingDirectory = Fd <$> throwErrnoIfMinus1 "open working directory" workingDirectoryNative

foreign import ccall unsafe "monk_spawn_status" statusNative :: CInt -> IO CInt

spawnStatus :: Int -> IO ProcessID
spawnStatus code = fromIntegral <$> throwErrnoIfMinus1 "spawn failed executable status" (statusNative (fromIntegral code))

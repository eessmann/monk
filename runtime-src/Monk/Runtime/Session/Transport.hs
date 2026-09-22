{-# LANGUAGE ForeignFunctionInterface #-}

-- | Private Unix socket transport. SCM_RIGHTS preserves the caller's open-file
-- descriptions, including offsets, without exposing protocol on user streams.
module Monk.Runtime.Session.Transport
  ( listenSession,
    acceptSession,
    requestSession,
    receiveSession,
    replySession,
    closeSession,
    requestSessionLease,
    replySessionLease,
  )
where

import Control.Exception (IOException, bracket, catch, onException)
import Control.Monad (void)
import Data.ByteString qualified as B
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1RetryMayBlock, throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (Ptr)
import GHC.Conc (threadWaitRead)
import Monk.Runtime.Descriptors (duplicatePrivate)
import System.IO (Handle, hClose, hFlush)
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "monk_session_listen" listenNative :: CString -> IO CInt

foreign import ccall unsafe "monk_session_connect" connectNative :: CString -> IO CInt

foreign import ccall unsafe "monk_session_accept" acceptNative :: CInt -> IO CInt

foreign import ccall unsafe "monk_session_send_fds" sendNative :: CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall unsafe "monk_session_receive_fds" receiveNative :: CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "monk_session_shutdown_write" shutdownNative :: CInt -> IO CInt

ownSocket :: IO CInt -> IO Fd
ownSocket acquire = bracket (Fd <$> throwErrnoIfMinus1 "session socket" acquire) closeFd duplicatePrivate

listenSession :: B.ByteString -> IO Fd
listenSession path = B.useAsCString path (ownSocket . listenNative)

acceptSession :: Fd -> IO Fd
acceptSession (Fd listener) = ownSocket (throwErrnoIfMinus1RetryMayBlock "accept session" (acceptNative listener) (threadWaitRead (fromIntegral listener)))

requestSession :: B.ByteString -> [Fd] -> B.ByteString -> IO B.ByteString
requestSession path descriptors request = do
  socket <- B.useAsCString path (ownSocket . connectNative)
  bracket (fdToHandle socket `onException` closeFd socket) hClose $ \handle -> do
    let Fd fd = socket
    withArray [value | Fd value <- descriptors] $ \values -> throwErrnoIfMinus1_ "send session descriptors" (sendNative fd values (fromIntegral (length descriptors)))
    B.hPut handle request
    hFlush handle
    throwErrnoIfMinus1_ "finish session request" (shutdownNative fd)
    readAll handle

-- | Each accepted socket and transferred descriptor has exactly one owner.
receiveSession :: Fd -> IO (Handle, [Fd], B.ByteString)
receiveSession socket@(Fd fd) = do
  descriptors <- allocaArray 4 $ \values -> do
    count <- throwErrnoIfMinus1RetryMayBlock "receive session descriptors" (receiveNative fd values) (threadWaitRead (fromIntegral fd))
    raw <- map Fd <$> peekArray (fromIntegral count) values
    bracket (pure raw) (mapM_ closeSession) duplicateAll
  handle <- fdToHandle socket `onException` mapM_ closeSession descriptors
  request <- readAll handle `onException` (hClose handle >> mapM_ closeSession descriptors)
  pure (handle, descriptors, request)

replySession :: Handle -> B.ByteString -> IO ()
replySession handle reply = B.hPut handle reply >> hFlush handle

closeSession :: Fd -> IO ()
closeSession fd = void (closeFd fd) `catch` (\(_ :: IOException) -> pure ())

readAll :: Handle -> IO B.ByteString
readAll handle = loop []
  where
    loop chunks = do
      chunk <- B.hGetSome handle 65536
      if B.null chunk then pure (B.concat (reverse chunks)) else loop (chunk : chunks)

-- | Authenticate a bootstrap capsule and retain its lifetime lease. The lease
-- is close-on-exec, so evaluator/user children cannot extend owner lifetime.
requestSessionLease :: B.ByteString -> B.ByteString -> IO (Fd, B.ByteString)
requestSessionLease path request = do
  socket@(Fd fd) <- B.useAsCString path (ownSocket . connectNative)
  bracket (fdToHandle socket `onException` closeFd socket) hClose $ \handle -> do
    withArray [] $ \values -> throwErrnoIfMinus1_ "request capsule" (sendNative fd values 0)
    B.hPut handle request
    hFlush handle
    throwErrnoIfMinus1_ "finish capsule request" (shutdownNative fd)
    descriptors <- allocaArray 4 $ \values -> do
      count <- throwErrnoIfMinus1RetryMayBlock "receive capsule lease" (receiveNative fd values) (threadWaitRead (fromIntegral fd))
      raw <- map Fd <$> peekArray (fromIntegral count) values
      bracket (pure raw) (mapM_ closeSession) duplicateAll
    response <- readAll handle `onException` mapM_ closeSession descriptors
    case descriptors of
      [lease] -> pure (lease, response)
      _ -> mapM_ closeSession descriptors >> ioError (userError "missing capsule lease")

replySessionLease :: Fd -> Handle -> Fd -> B.ByteString -> IO ()
replySessionLease (Fd socket) handle (Fd lease) response = do
  withArray [lease] $ \values -> throwErrnoIfMinus1_ "transfer capsule lease" (sendNative socket values 1)
  replySession handle response

-- Partial duplication failures must release already acquired owned references.
duplicateAll :: [Fd] -> IO [Fd]
duplicateAll descriptors = go descriptors []
  where
    go [] owned = pure (reverse owned)
    go (descriptor : rest) owned = do
      copied <- duplicatePrivate descriptor `onException` mapM_ closeSession owned
      go rest (copied : owned)

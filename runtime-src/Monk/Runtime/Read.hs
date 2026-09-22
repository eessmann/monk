-- | C-locale byte reads with no read-ahead: the next command observes exactly
-- the remaining bytes in the shared open-file description.
module Monk.Runtime.Read (ReadConfig (..), ReadDestination (..), readDescriptor) where

import Data.ByteString qualified as B
import Data.Word (Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import System.Posix.IO (fdReadBuf)
import System.Posix.Types (Fd)

data ReadDestination = ReadReply | ReadScalars Int | ReadArray
  deriving stock (Eq, Show)

data ReadConfig = ReadConfig
  { readRaw :: Bool,
    readDelimiter :: Word8,
    readLimit :: Maybe Int,
    readIFS :: B.ByteString,
    readDestination :: ReadDestination
  }
  deriving stock (Eq, Show)

-- True marks an escaped byte; it remains literal during IFS splitting.
type ReadByte = (Word8, Bool)

readDescriptor :: ReadConfig -> Fd -> IO (Int, [B.ByteString])
readDescriptor config descriptor = do
  (status, bytes) <- gather False 0 []
  pure (status, fields config (reverse bytes))
  where
    gather escaped count accumulated
      | maybe False (count >=) (readLimit config) = pure (0, accumulated)
      | otherwise = do
          next <- readByte descriptor
          case next of
            Nothing -> pure (1, accumulated)
            Just byte
              | byte == 0 && readDelimiter config /= 0 -> gather escaped count accumulated
              | escaped && byte == 10 -> gather False count accumulated
              | escaped -> gather False (count + 1) ((byte, True) : accumulated)
              | byte == readDelimiter config -> pure (0, accumulated)
              | byte == 92 && not (readRaw config) -> gather True count accumulated
              | otherwise -> gather False (count + 1) ((byte, False) : accumulated)

readByte :: Fd -> IO (Maybe Word8)
readByte descriptor = alloca $ \pointer -> do
  count <- fdReadBuf descriptor (castPtr pointer) 1
  if count == 0 then pure Nothing else Just <$> peek pointer

fields :: ReadConfig -> [ReadByte] -> [B.ByteString]
fields config bytes = case readDestination config of
  ReadReply -> [value bytes]
  ReadArray -> map value (split bytes)
  ReadScalars count -> scalars count bytes
  where
    separator (byte, escaped) = not escaped && B.elem byte (readIFS config)
    whitespace item@(byte, _) = separator item && byte `elem` [32, 9, 10]
    trimStart = dropWhile whitespace
    trimEnd = reverse . trimStart . reverse
    value = B.pack . map fst
    split input = case nextField input of
      Nothing -> []
      Just (first, rest) -> first : split rest
    nextField input = case trimStart input of
      [] -> Nothing
      remaining ->
        let (first, rest) = break separator remaining
            after = case rest of
              [] -> []
              item : following
                | whitespace item -> case trimStart following of
                    next : suffix | separator next -> trimStart suffix
                    suffix -> suffix
                | otherwise -> trimStart following
         in Just (first, after)
    scalars count input
      | count <= 0 = []
      | count == 1 = case split input of
          [] -> [B.empty]
          [first] -> [value first]
          _ -> [value (trimEnd (trimStart input))]
      | otherwise = case nextField input of
          Nothing -> replicate count B.empty
          Just (first, rest) -> value first : scalars (count - 1) rest

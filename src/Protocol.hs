{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Protocol
  ( Cfg
  , parsePacket
  ) where

import           Data.Binary.Get
import           Data.Bits            (complement, xor, (.&.))
import qualified Data.ByteString.Lazy as BL
import           Data.List            (intercalate)
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Vector          as V
import           Data.Word

import           Control.Applicative  (many)

import           Text.Printf          (printf)

import           Util

type Cfg = (V.Vector String, V.Vector String)

data Header = Header
  { _count  :: Word8
  , _uid    :: Word8
  , _tmstmp :: Word32
  }

instance Show Header where
  show (Header c u t) =
    printf "%03d" c ++ ", " ++ printf "%010d" t ++ ", " ++ printf "%03d" u

newtype InitEv = InitEv
  { _id :: String
  } deriving (Show)

data ReceiveEv = ReceiveEv
  { _id    :: String
  , _sigId :: String
  , _state :: String
  } deriving (Show)

data TransitionEv = TransitionEv
  { _id        :: String
  , _state     :: String
  , _sigId     :: String
  , _nextState :: String
  } deriving (Show)

data EvType
  = IE InitEv
  | RE ReceiveEv
  | TE TransitionEv
  | BD [BL.ByteString]

instance Show EvType where
  show a =
    case a of
      BD a' -> "BinaryData {" ++ intercalate ", " (fmap toHex a') ++ "}"
      IE a' -> show a'
      RE a' -> show a'
      TE a' -> show a'

data Payload = Payload
  { _events :: Maybe EvType
  , _data   :: BL.ByteString
  }

instance Show Payload where
  show (Payload e d) =
    case e of
      Just et -> show et
      Nothing -> "??? {" ++ toHex d ++ "}"

data Packet = Packet
  { _header   :: Header
  , _len      :: Word8
  , _payload  :: Payload
  , _crc      :: Word8
  , _crcValid :: Bool
  }

instance Show Packet where
  show (Packet h l p c c') =
    show h ++
    ", " ++
    printf "%03d" l ++
    ", " ++
    "crc=" ++
    printf "%03d" c ++
    " (" ++
    (if c'
       then "Ok"
       else "Err") ++
    ")" ++ ", " ++ show p

cfgGet :: V.Vector String -> Word8 -> String
cfgGet ids i = fromMaybe "???" $ ids V.!? fromIntegral i

getHeader :: Get Header
getHeader = Header <$> getWord8 <*> getWord8 <*> getWord32le

getInitEv :: Cfg -> Get InitEv
getInitEv (_, ids) = InitEv . cfgGet ids <$> getWord8

getReceiveEv :: Cfg -> Get ReceiveEv
getReceiveEv (sigIds, ids) = do
  i <- getWord8
  s <- getWord8
  st <- getNData 1
  return $
    ReceiveEv
      (cfgGet ids i)
      (cfgGet sigIds s)
      (read . show $ fromMaybe "???" $ listToMaybe st)

getTransitionEv :: Cfg -> Get TransitionEv
getTransitionEv (sigIds, ids) = do
  i <- getWord8
  s <- getWord8
  (st:ns:_) <- read . show <$> getNData 2
  return $ TransitionEv (cfgGet ids i) st (cfgGet sigIds s) ns

getEvType :: Cfg -> Word8 -> Get (Maybe EvType)
getEvType cfg u
  | u == 0 = Just . IE <$> getInitEv cfg
  | u == 1 = Just . TE <$> getTransitionEv cfg
  | u == 2 = Just . RE <$> getReceiveEv cfg
  | u == 3 = Just . BD <$> getData
  | otherwise = return Nothing

getPacket :: Cfg -> Word8 -> Get Packet
getPacket cfg crc = do
  h <- getHeader
  len <- getWord8
  b <- getLazyByteString $ fromIntegral (len - 1)
  let et =
        case runGetOrFail (getEvType cfg (_uid h)) b of
          Right (_, _, me) -> me
          _                -> Nothing
  crc' <- getWord8
  return $ Packet h len (Payload et b) crc' (crc == crc')

getData :: Get [BL.ByteString]
getData =
  many $ do
    len <- getWord8
    getLazyByteString $ fromIntegral len

getNData :: Int -> Get [BL.ByteString]
getNData n = fmap (take n) getData

unescape :: BL.ByteString -> BL.ByteString
unescape bl = fst $ BL.foldl' go (BL.empty, False) bl
  where
    go (bs, s) w
      | s = (BL.snoc bs (xor w 0x20), False)
      | w == 0x7D = (bs, True)
      | otherwise = (BL.snoc bs w, s)

checksum :: BL.ByteString -> Word8
checksum bs = 0xFF .&. complement (BL.foldl1' (+) bs)

parsePacket :: Cfg -> BL.ByteString -> String
parsePacket cfg b =
  let b' = unescape b
      go = getPacket cfg (checksum (BL.init b'))
   in case runGetOrFail go b' of
        Right (_, _, p) -> show p
        Left (bs, o, err) ->
          "Error: " ++ err ++ " in " ++ toHex bs ++ " at " ++ show o

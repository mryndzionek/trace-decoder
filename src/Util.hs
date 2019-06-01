{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Util
  ( toHex
  , log'
  , unique
  , serialOpen
  ) where

import qualified Data.ByteString.Lazy       as BL

import           Data.List                  (foldl')
import qualified Data.Set                   as S
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Vector                as V
import           Data.Word

import           System.Hardware.Serialport
import           System.Process

import           Control.Monad.IO.Class

import           Numeric                    (showHex)

hex :: Word8 -> String
hex = format . flip showHex ""
  where
    format c
      | length c == 2 = c
      | otherwise = "0" ++ c

toHex :: BL.ByteString -> String
toHex bs = unwords $ map hex $ BL.unpack bs

log' :: MonadIO m => String -> m ()
log' msg =
  liftIO $ do
    now <- getCurrentTime
    let ds = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S.%3q" now
    putStrLn $ ds ++ " :: " ++ msg

unique :: Ord a => [a] -> V.Vector a
unique xs =
  snd $
  foldl'
    (\(s, a) b ->
       if S.member b s
         then (s, a)
         else (S.insert b s, V.snoc a b))
    (S.empty, V.empty)
    xs

serialOpen :: String -> Integer -> IO SerialPort
serialOpen port baud = do
  sp <- openSerial port defaultSerialSettings {timeout = 10}
  (e, _, _) <- readProcessWithExitCode "stty" ["-F", port, show baud] ""
  log' $ "Setting baudrate: " ++ show e
  return sp

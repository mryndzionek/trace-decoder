{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Util
  ( toHex
  , log'
  , serialOpen
  ) where

import qualified Data.ByteString.Lazy       as BL

import           Data.Time.Clock
import           Data.Time.Format

import           System.Hardware.Serialport
import           System.Process

import           Control.Monad.IO.Class

import           Numeric                    (showHex)

toHex :: BL.ByteString -> String
toHex bs =
  unwords $
  map
    ((\c ->
        if length c == 2
          then c
          else "0" ++ c) .
     flip showHex "") $
  BL.unpack bs

log' :: MonadIO m => String -> m ()
log' msg =
  liftIO $ do
    now <- getCurrentTime
    let ds = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S.%3q" now
    putStrLn $ ds ++ " :: " ++ msg

serialOpen :: String -> Integer -> IO SerialPort
serialOpen port baud = do
  sp <- openSerial port defaultSerialSettings {timeout = 10}
  (e, _, _) <- readProcessWithExitCode "stty" ["-F", port, show baud] ""
  log' $ "Setting baudrate: " ++ show e
  return sp

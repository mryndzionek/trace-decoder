{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isSpace)
import           Data.Function              ((&))
import           Data.List.Split            (splitWhen)
import           Data.Maybe                 (fromJust, isJust)
import           Data.Semigroup             (cycle1)

import qualified Data.Vector                as V

import           Control.Monad              (when, (>=>))
import           Control.Monad.IO.Class

import           System.Environment
import           System.Hardware.Serialport

import           Streamly
import qualified Streamly.Prelude           as S

import           Text.Regex

import           Opts
import           Protocol
import           Util

serialStream :: MonadIO m => SerialPort -> SerialT m BL.ByteString
serialStream sp =
  S.filter (not . BL.null) $
  cycle1 $ liftIO $ BL.fromStrict <$> recv sp (2 * 32 + 2)

fileStream :: MonadIO m => FilePath -> SerialT m BL.ByteString
fileStream fp = S.filter (not . BL.null) $ liftIO $ BL.readFile fp

splitComms :: BL.ByteString -> ([BL.ByteString], BL.ByteString)
splitComms xs
  | BL.null xs = ([], BL.empty)
  | otherwise =
    let a = BL.split 0x7E xs
     in (init a, last a)

commsSplitter :: MonadIO m => SerialT m BL.ByteString -> SerialT m BL.ByteString
commsSplitter bs = bss >>= (mconcat . map S.yield)
  where
    bss = S.filter (not . null) $ S.map fst $ S.scanl' cut ([], BL.empty) bs
    cut (_, leftover) bs' = splitComms (BL.append leftover bs')

packetStream ::
     MonadAsync m => Bool -> Cfg -> SerialT m BL.ByteString -> SerialT m String
packetStream d cfg s =
  S.mapM printPacket $
  S.map (parsePacket cfg) (s & commsSplitter & S.mapM debugBuffer)
  where
    printPacket p = log' (tail . init $ show p) >> return p
    debugBuffer b = when d (log' ("Raw: " ++ toHex b)) >> return b

getSerialParams :: FilePath -> Maybe (String, Integer)
getSerialParams fp =
  let spRe = mkRegex "([^:]+):([0-9]+):([5678])([NOE])([12])"
      match = matchRegex spRe fp
   in (\s -> (head s, read $ s !! 1)) <$> match

getSigMap :: [String] -> Cfg
getSigMap ls =
  let sigRe = mkRegex "ESM_SIGNAL\\((\\w+)\\)"
      idRe = mkRegex "ESM_ID\\((\\w+)\\)"
      find re =
        unique . fmap (head . fromJust) . filter isJust . fmap (matchRegex re)
      sigs = find sigRe ls
      ids = find idRe ls
   in (V.cons "alarms" sigs, V.fromList ["tick", "trace"] V.++ ids)

main :: IO ()
main = do
  (debug, isSerial, mapFp, path) <- getArgs >>= getOpts
  cfg <- getSigMap . (splitWhen isSpace >=> lines) <$> readFile mapFp
  let run = runStream . packetStream debug cfg
  if isSerial
    then case getSerialParams path of
           Just sp -> do
             sp' <- uncurry serialOpen sp
             run $ serialStream sp'
             closeSerial sp'
           Nothing -> putStrLn $ "Wrong serial port format: " ++ path
    else run $ fileStream path

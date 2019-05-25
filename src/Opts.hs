module Opts
  ( getOpts
  ) where

import           Data.List             (foldl')
import           System.Console.GetOpt

data Flag
  = Debug
  | IsSerial Bool
  | Map FilePath
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['d'] ["debug"] (NoArg Debug) "debug output on stderr"
  , Option ['i'] ["input"] (ReqArg inp "serial | file") "input type"
  , Option ['m'] ["map"] (ReqArg Map "FILE") "path to map file (signals.h)"
  ]

inp :: String -> Flag
inp = IsSerial . (== "serial")

process :: [String] -> [Flag] -> (Bool, FilePath, String)
process ps fs =
  ( x
  , y
  , if null ps
      then "/dev/ttyUSB0:576000:8N1"
      else head ps)
  where
    (x, y) = foldl' go (True, "") fs
    go (i, m) b =
      case b of
        IsSerial a -> (a, m)
        Map fp     -> (i, fp)
        _          -> (i, m)

getOpts :: [String] -> IO (Bool, FilePath, String)
getOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return $ process n o
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where
    header =
      "Usage: proto-decoder [OPTION...] path to file or serial port identifier"

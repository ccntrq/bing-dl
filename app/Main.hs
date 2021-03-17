module Main where

import           Control.Conditional            ( cond )
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.Text                     as T
import qualified System.Console.GetOpt         as GetOpt
import           System.Directory.Text          ( createDirectoryIfMissing )
import           System.Environment             ( getProgName )
import           System.Posix.Untildify

import           BingDl                         ( fetchWallpapers )

version :: Text
version = "bing-dl v0.1.0"

main :: IO ()
main = do
  args      <- getArgs
  (opts, _) <- bingDlOpts (T.pack <$> args)
  dispatch opts

data Options = Opts
  { optHelp        :: Bool
  , optVersion     :: Bool
  , optFetch       :: Bool
  , optLibraryPath :: Text
  }
  deriving Show

defaultOptions :: Options
defaultOptions = Opts { optHelp        = False
                      , optVersion     = False
                      , optFetch       = False
                      , optLibraryPath = "~/wallpaper/"
                      }

options :: [GetOpt.OptDescr (Options -> Options)]
options =
  [ GetOpt.Option
    ['l']
    ["library"]
    (GetOpt.ReqArg
      (\libraryDir opts -> opts { optLibraryPath = T.pack libraryDir })
      "[DIR]"
    )
    "Provide a custom library path"
  , GetOpt.Option ['v']
                  ["version"]
                  (GetOpt.NoArg (\opts -> opts { optVersion = True }))
                  "Print version"
  , GetOpt.Option ['h', 'H']
                  ["help"]
                  (GetOpt.NoArg (\opts -> opts { optHelp = True }))
                  "Print this help message"
  ]

bingDlOpts :: [Text] -> IO (Options, [FilePath])
bingDlOpts argv =
  case GetOpt.getOpt GetOpt.Permute options (T.unpack <$> argv) of
    (o, n, []  ) -> return (foldl (&) defaultOptions o, n)
    (_, _, errs) -> mapM_ putStr errs >> printUsage >> exitFailure

createUsage :: BS.ByteString -> BS.ByteString
createUsage progName =
  let header = concat ["Usage: ", BS.unpack progName, " [Options]"]
  in  BS.pack $ GetOpt.usageInfo header options

dispatch :: Options -> IO ()
dispatch opts = do
  cond
    [ (optHelp opts   , printUsage)
    , (optVersion opts, printVersion)
    , (otherwise      , runFetch opts)
    ]
  exitSuccess

printUsage :: IO ()
printUsage = do
  progName <- getProgName
  putStrLn $ createUsage (BS.pack progName)

printVersion :: IO ()
printVersion = putStrLn (version :: Text)

runFetch :: Options -> IO ()
runFetch opts = do
  realLib <- untildify $ optLibraryPath opts
  forM_ [createDirectoryIfMissing True, fetchWallpapers] (realLib &)
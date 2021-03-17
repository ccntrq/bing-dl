module System.Directory.Text
    ( createDirectoryIfMissing
    , getDirectoryContents
    , doesFileExist
    ) where

import qualified System.Directory              as Dir

import           Data.Text

-- | Text wrapper arround System.Directory.getDirectoryContents
getDirectoryContents :: Text -> IO [Text]
getDirectoryContents path = pack <<$>> Dir.getDirectoryContents (unpack path)

-- | Text wrapper arround System.Directory.createDirectoryIfMissing
createDirectoryIfMissing :: Bool -> Text -> IO ()
createDirectoryIfMissing b = Dir.createDirectoryIfMissing b . unpack

-- | Text wrapper arround System.Directory.doesFileExist
doesFileExist :: Text -> IO Bool
doesFileExist = Dir.doesFileExist . unpack

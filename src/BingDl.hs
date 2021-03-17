module BingDl where

import           Data.List                      ( nub )

import           Control.Lens.Getter            ( (^.) )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text                     as T
import qualified Network.Wreq                  as Wreq
import           Text.XML                       ( def
                                                , parseLBS_
                                                )
import           Text.XML.Cursor                ( child
                                                , content
                                                , descendant
                                                , element
                                                , fromDocument
                                                )

import           System.Directory.Text          ( doesFileExist )
import           System.MyProgressbar           ( incProgress
                                                , newProgressbar
                                                , updateWorkTodo
                                                )

fetchWallpapers :: Text -> IO ()
fetchWallpapers libraryDir = do
  putStrLn ("fetching new wallpaper from bing..." :: Text)
  pb      <- newProgressbar
  imgs    <- nub . concat <$> mapM fetchWallpapersForRegion regions
  newImgs <- filterM (\(name, _) -> not <$> doesFileExist (fullName name)) imgs
  updateWorkTodo pb (length newImgs)
  mapM_
    (\(name, url) -> do
      downloadFile (T.unpack $ fullName name) url
      incProgress pb
    )
    newImgs
  where fullName name = T.concat [libraryDir, T.pack name]

fetchWallpapersForRegion :: BS.ByteString -> IO [(FilePath, BS.ByteString)]
fetchWallpapersForRegion region = do
  -- 15 images seems to be the maximum we can fetch
  -- a maximum of 8 images per fetch is allowed
  page1 <- fetchPage 0 7
  page2 <- fetchPage 8 7
  return $ page1 ++ page2
 where
  fetchPage init count = do
    let url = genURL init count region
    res <- Wreq.get (BSC.unpack url)
    let doc    = parseLBS_ def (res ^. Wreq.responseBody)
    let cursor = fromDocument doc
    let urls =
          child cursor
            >>= element "image"
            >>= child
            >>= element "urlBase"
            >>= descendant
            >>= content
    let namesWithUrls =
          (\url' ->
              ( T.unpack $ T.concat [extractImageName url', resolution, extension]
              , BSC.pack $ T.unpack $ T.concat
                [bingUrl, url', resolution, extension]
              )
            )
            <$> urls
    return namesWithUrls

downloadFile :: FilePath -> BS.ByteString -> IO ()
downloadFile name url = do
  res <- Wreq.get (BSC.unpack url)
  liftIO $ BL.writeFile name (res ^. Wreq.responseBody)

extractImageName :: Text -> Text
extractImageName baseUrl =
  T.drop 1 $ T.dropEnd 1 $ T.dropWhile (/= '.') $ T.dropWhileEnd (/= '_')
                                                                 baseUrl

bingUrl :: IsString a => a
bingUrl = "https://www.bing.com"

resolution :: IsString a => a
resolution = "_1920x1080"

extension :: IsString a => a
extension = ".jpg"

regions :: IsString a => [a]
regions =
  ["de-DE", "zh-CN", "ja-JP", "en-US", "en-AU", "en-UK", "en-NZ", "en-CA"]

genURL :: Int -> Int -> BS.ByteString -> BS.ByteString
genURL pictureIndex pictureCountPerFetch region = BS.concat
  [ bingUrl
  , apiBase
  , pictureIndexBase
  , BSC.pack $ show pictureIndex
  , pictureCountBase
  , BSC.pack $ show pictureCountPerFetch
  , pictureRegionBase
  , region
  ]
 where
  apiBase :: BS.ByteString
  apiBase = "/HPImageArchive.aspx?format=xml"
  pictureIndexBase :: BS.ByteString
  pictureIndexBase = "&idx="
  pictureCountBase :: BS.ByteString
  pictureCountBase = "&n="
  pictureRegionBase :: BS.ByteString
  pictureRegionBase = "&mkt="

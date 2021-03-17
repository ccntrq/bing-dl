module System.Posix.Untildify
  ( untildify
  ) where

import qualified Data.Text                     as T
import           System.Directory               ( getHomeDirectory )

untildify :: T.Text -> IO T.Text
untildify path = case T.uncons path of
  Just ('~', rest) -> do
    userHome <- T.pack <$> getHomeDirectory
    return $ T.concat [userHome, rest]
  _ -> return path

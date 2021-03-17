module System.MyProgressbar where

import qualified System.ProgressBar            as PB

newProgressbar :: IO (PB.ProgressBar ())
newProgressbar = PB.newProgressBar PB.defStyle 10 (PB.Progress 0 100 ())

updateWorkTodo :: PB.ProgressBar () -> Int -> IO ()
updateWorkTodo pb workTodo =
  PB.updateProgress pb (const $ PB.Progress 0 workTodo ())

incProgress :: PB.ProgressBar s -> IO ()
incProgress = flip PB.incProgress 1


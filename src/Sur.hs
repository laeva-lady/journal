module Sur
  ( getTodayEntry,
    startVIMquestionMark,
    getDate,
    date2string,
    getListOfEntries,
  )
where

import           Control.Monad
import           Data.Time
import           Lib
import           System.Directory
import           System.FilePath
import           System.Process
import           Text.Printf      (printf)
import           Utils

startVIMquestionMark :: String -> IO ()
startVIMquestionMark file = do
  _ <- rawSystem "nvim" [file]
  return ()

getDate :: IO Date -- (year, month, day)
getDate = do
  toGregorian . localDay . zonedTimeToLocalTime <$> getZonedTime

date2string :: Date -> String
date2string (y, m, d) = Utils.unwords '-' [show y, printf "%02d" m, printf "%02d" d]

getTodayEntry :: IO FilePath
getTodayEntry = do
  d <- getDate
  journalpath <- journalPath
  let entry = journalpath ++ date2string d ++ ".md"
  return entry

getListOfEntries :: IO [FilePath]
getListOfEntries = do
  dir <- journalPath
  contents <- listDirectory dir
  filtered_contents <- filterM (doesFileExist . (dir </>)) contents

  mapM (return . (dir </>)) filtered_contents



module Sur
  ( getTodayEntry,
    startVIMquestionMark,
    getDate,
    date2string,
  )
where

import Data.Time
import Lib
import System.Process
import Text.Printf (printf)
import Utils

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

-- entry == journal file
-- get entry for current date
-- open it in vim
-- TUI to view all the entries

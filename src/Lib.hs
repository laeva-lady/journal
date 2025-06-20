module Lib
  ( journalPath,
  )
where

import           System.Directory
import           System.Environment (getEnv)

journalPath :: IO FilePath
journalPath = do
    let jpath = (++ "/personal/journal/") <$> getEnv "HOME" :: IO FilePath
    jpath >>= createDirectoryIfMissing True
    jpath

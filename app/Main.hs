module Main (main) where
import Application (startTheApp)

main :: IO ()
main = startTheApp

-- mainLoop :: Int -> IO ()
-- mainLoop count = do
--   getTodayEntry >>= startVIMquestionMark

--   -- avoid infinite loop for now
--   _ <- getLine
--   mainLoop $ count + 1

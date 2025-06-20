module Utils
  ( Utils.unwords,
    Date,
  )
where

type Date = (Integer, Int, Int)

unwords :: Char -> [String] -> String
unwords _ [] = ""
unwords p (w : ws) = w ++ go ws
  where
    go [] = ""
    go (v : vs) = p : (v ++ go vs)

import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)


-- Parses till blank line
-- Returns list of Integers
readLines :: IO [Int]
readLines = do
  line <- getLine
  done <- isEOF
  let number = read line :: Int
  if done
    then return [number]
    else
      (number :) <$> readLines

countIncrease :: Ord a => [a] -> Int
countIncrease [] = 0
countIncrease [_] = 0
countIncrease (a:b:tail)
  | a < b = (1 +) $ countIncrease (b:tail)
  | otherwise = countIncrease (b:tail)


main = do
  lines <- readLines
  print $ countIncrease lines

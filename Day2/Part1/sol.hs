import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)

data Arrow = Forward Integer | Up Integer | Down Integer deriving (Read, Show)

parseInput :: [String] -> [Arrow]
parseInput [] = []
parseInput (x:xs)
  | first == "forward" = (Forward secondInt) : parseInput xs
  | first == "up"      = (Up secondInt) : parseInput xs
  | first == "down"    = (Down secondInt) : parseInput xs
    where (first:second:[]) = words x
          secondInt = read second :: Integer

readLines :: IO [String]
readLines = do
  line <- getLine
  done <- isEOF
  if done
     then return [line]
     else (line :) <$> readLines

depthProduct :: [Arrow] -> Integer
depthProduct = depthProductAcum 0 0
  where 
    depthProductAcum x y [] = x * y
    depthProductAcum x y ((Forward u):us) = depthProductAcum (x + u) y us
    depthProductAcum x y ((Up u):us) = depthProductAcum x (y - u) us
    depthProductAcum x y ((Down u):us) = depthProductAcum x (y + u) us

main = do
  lines <- readLines
  print $ (depthProduct . parseInput) lines

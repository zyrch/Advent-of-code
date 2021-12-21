import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)

-- 'Arrow' is not a good name for part 2
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
depthProduct = depthProductAcum 0 0 0 
  where 
    depthProductAcum x y _ [] = x * y
    depthProductAcum x y z ((Forward u):us) = depthProductAcum (x + u) (y + z * u) z us
    depthProductAcum x y z ((Up u):us)      = depthProductAcum x y (z - u) us
    depthProductAcum x y z ((Down u):us)    = depthProductAcum x y (z + u) us

main = do
  lines <- readLines
  print $ (depthProduct . parseInput) lines

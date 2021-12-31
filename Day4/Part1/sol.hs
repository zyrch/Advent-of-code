import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)
import Data.Array

readLines :: IO [String]
readLines = do
  line <- getLine
  done <- isEOF
  if done
    then return [line]
    else (line :) <$> readLines

parseInput :: [String] -> ([Int], [Array (Int, Int) Int])
parseInput (x:_:xs) = (bingoNumbers, matrices)
  where
    bingoNumbers = read ('[' : (x ++ "]")) :: [Int] 
    matrices = parseMatrices $ splitBy (=="") xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = case dropWhile p s of 
                [] -> []
                s' -> w : splitBy p s''
                      where (w, s'') = break p s'

parseMatrices :: [[String]] -> [Array (Int, Int) Int]
parseMatrices = map parseMatrix

parseMatrix :: [String] -> Array (Int, Int) Int
parseMatrix x = (matrixFromList n m) $ zipWith2dIndex lx
  where lx = parseLists x
        n = length lx
        m = length $ head lx -- fails if empty list

matrixFromList :: Int -> Int -> [((Int, Int), Int)] -> Array (Int, Int) Int
matrixFromList n m matrix = array ((1, 1),(n, m)) matrix

zipWith2dIndex :: [[Int]] -> [((Int, Int), Int)]
zipWith2dIndex x = [((i + 1, j + 1), a) | (i, b) <- zip [0..] x, 
                                          (j, a) <- zip [0..] b]

parseLists :: [String] -> [[Int]]
parseLists = map ((map read) . words)


main = do
  lines <- readLines
  let content = parseInput lines
  print content


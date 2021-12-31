import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)
import Data.Array

import Debug.Trace

readLines :: IO [String]
readLines = do
  line <- getLine
  done <- isEOF
  if done
    then return [line]
    else (line :) <$> readLines

parseInput :: [String] -> ([Int], [Array (Int, Int) (Int, Int)])
parseInput (x:_:xs) = (bingoNumbers, matrices)
  where
    bingoNumbers = read ('[' : (x ++ "]")) :: [Int] 
    matrices = parseMatrices $ splitBy (=="") xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = case dropWhile p s of 
                [] -> []
                s' -> w : splitBy p s''
                      where (w, s'') = break p s'

parseMatrices :: [[String]] -> [Array (Int, Int) (Int, Int)]
parseMatrices = map parseMatrix

parseMatrix :: [String] -> Array (Int, Int) (Int, Int)
parseMatrix x = (matrixFromList n m) $ zipWith2dIndex lx
  where lx = parseLists x
        n = length lx
        m = length $ head lx -- fails if empty list

matrixFromList :: Int -> Int -> [((Int, Int), (Int, Int))] -> Array (Int, Int) (Int, Int)
matrixFromList n m matrix = array ((1, 1),(n, m)) matrix

zipWith2dIndex :: [[Int]] -> [((Int, Int), (Int, Int))]
zipWith2dIndex x = [((i + 1, j + 1), (a, 0)) | (i, b) <- zip [0..] x, 
                                               (j, a) <- zip [0..] b]

parseLists :: [String] -> [[Int]]
parseLists = map ((map read) . words)

markMatrix :: Int -> Array (Int, Int) (Int, Int) -> Array (Int, Int) (Int, Int)
markMatrix value matrix = matrix // [((i, j), (fst (matrix!(i,j)), 1)) | i <- [1..n], j <- [1..m], fst (matrix!(i,j)) == value]
  where ((_,_), (n, m)) = bounds matrix

checkAllMatrices :: [Array (Int, Int) (Int, Int)] -> Bool
checkAllMatrices x = any (==True) $ map checkMatrix x

checkMatrix :: Array (Int, Int) (Int, Int) -> Bool
checkMatrix x = (checkCols x n m) || (checkRows x n m)
  where ((_,_), (n, m)) = bounds x

checkCols :: Array (Int, Int) (Int, Int) -> Int -> Int -> Bool
checkCols x n m = any (==True) $ map (checkCol x 1 n) [1..m]

checkCol :: Array (Int, Int) (Int, Int) -> Int -> Int -> Int -> Bool
checkCol x y n p
  | y == (n + 1) = True
  | snd (x ! (y, p)) == 1 = checkCol x (y + 1) n p
  | otherwise = False

checkRows :: Array (Int, Int) (Int, Int) -> Int -> Int -> Bool
checkRows x n m = any (==True) $ map (checkRow x 1 m) [1..n]

checkRow :: Array (Int, Int) (Int, Int) -> Int -> Int -> Int -> Bool
checkRow x y n p
  | y == (n + 1) = True
  | snd (x ! (p, y)) == 1 = checkRow x (y + 1) n p
  | otherwise = False

getUnmarkedSum :: Array (Int, Int) (Int, Int) -> Int
getUnmarkedSum matrix = sum [fst $ matrix ! (i, j) | i <- [1..n], j <- [1..m], (snd $ matrix ! (i, j)) == 0]
  where ((_,_), (n, m)) = bounds matrix

solve :: [Int] -> [Array (Int, Int) (Int, Int)] -> Int
solve [] _ = 0
solve (x:xs) matrices = if checkAllMatrices newMatrices then x * (getUnmarkedSum $ head $ filter checkMatrix newMatrices) else solve xs newMatrices
  where newMatrices = map (markMatrix x) matrices

main :: IO ()
main = do
  lines <- readLines
  let (bingoNumbers, matrices) = parseInput lines
  print $ solve bingoNumbers matrices


import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)
import Debug.Trace

parseInput :: [String] -> [String]
parseInput = id

readLines :: IO [String]
readLines = do
  line <- getLine
  done <- isEOF
  if done
    then return [line]
    else (line :) <$> readLines

mostCommonBit :: [String] -> Int -> Char
mostCommonBit x k = if count >= (((length x) + 1) `div` 2) then '1' else '0'
  where 
    count = fst $ foldl fun (0, k) x
    fun (currentCount, k) x
      | x !! k == '1' = (currentCount + 1, k)
      | otherwise = (currentCount, k)

filterWithMcb :: [String] -> Int -> Char -> [String]
filterWithMcb [] _ _ = []
filterWithMcb (x:xs) k mcb
  | (x !! k) == mcb = (x:) $ filterWithMcb xs k mcb
  | otherwise = filterWithMcb xs k mcb

filterBits :: [String] -> Int -> Int -> String
filterBits x k n  = if (n <= k || (length x) == 1) then head x else filterBits (filterWithMcb x k mcb) (k + 1) n
  where
    mcb = mostCommonBit x k

filterBits' :: [String] -> Int -> Int -> String
filterBits' x k n  = if (n <= k || (length x) == 1) then head x else filterBits' (filterWithMcb x k mcb) (k + 1) n
  where
    mcb = if mostCommonBit x k == '1' then '0' else '1'

binToDec :: [Int] -> Int
binToDec = foldl fun 0
  where
    fun n x = x + (2 * n)

charToInt :: String -> [Int]
charToInt = map (\a -> if a == '1' then 1 else 0)

complement :: [Int] -> [Int]
complement = map (\a -> if a == 1 then 0 else 1) 

main = do
  lines <- readLines
  let gamma = charToInt $ filterBits (parseInput lines) 0 (length $ head lines)
      epsilon = charToInt $ filterBits' (parseInput lines) 0 (length $ head lines)
  print (binToDec gamma * binToDec epsilon)  

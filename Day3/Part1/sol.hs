import Prelude hiding (readList)
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO (isEOF)

parseInput :: [String] -> [String]
parseInput = id

readLines :: IO [String]
readLines = do
  line <- getLine
  done <- isEOF
  if done
    then return [line]
    else (line :) <$> readLines

addBinaryString :: [Int] -> String -> [Int]
addBinaryString = zipWith fun
  where 
    fun x c
      | c == '1' = (x + 1)
      | otherwise = x

binarySum :: [String] -> [Int]
binarySum a = foldl addBinaryString [0 | _ <- [1..(length a)]] a

gammaRate :: [String] -> [Int]
gammaRate x = sumToBin sum n
  where
    n = length x
    sum = binarySum x
    sumToBin [] n = []
    sumToBin (x:xs) n 
      | x >= ((n + 1) `div` 2) = 1 : sumToBin xs n
      | otherwise              = 0 : sumToBin xs n

binToDec :: [Int] -> Int
binToDec = foldl fun 0
  where
    fun n x = x + (2 * n)

complement :: [Int] -> [Int]
complement = map (\a -> if a == 1 then 0 else 1) 

main = do
  lines <- readLines
  let gamma = gammaRate $ parseInput lines
      epsilon = complement gamma
  print (binToDec gamma * binToDec epsilon)  

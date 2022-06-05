{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Main where

import Lib
import Control.Parallel
import System.Environment (getArgs)
import Data.Time
import Data.List
import Data.Matrix hiding (toList)
import Data.Maybe (maybeToList, fromJust)
import Data.Vector (toList)
import Control.Monad

listToMatrix :: [a] -> Int -> Maybe [[a]]
listToMatrix xs b
    | length xs `mod` b /= 0 = Nothing
    | otherwise = Just (go xs)
    where go [] = []
          go xs = row : go remaining
              where (row, remaining) = splitAt b xs


relaxation :: [[Double]] -> [Double] -> [Double] -> [Double] -> Int -> Double -> Double -> [Double]
relaxation a b next prev n omega eps
    | normI (zipWith (-) next prev) < (omega * eps) = next
    | otherwise = relaxation a b (calc2 a b next next 0 omega) next n omega eps


calc :: [[Double]] -> [Double] -> [Double] -> [Double] -> Int -> Double -> Double
calc a b next prev i omega = do
    let n1 = firstStep (prev!!i) omega
    let s1 = getFirstSum a next i (i-1)
    let n2 = n1 - secondStep s1 omega
    let s2 = getSecondSum a prev i (i+1)
    let n3 = n2 - secondStep s2 omega
    let n4 = n3 + thirdStep (a!!i!!i) (b!!i) omega
    n4

calc2 :: [[Double]] -> [Double] -> [Double] -> [Double] -> Int -> Double -> [Double]
calc2 a b next prev i omega 
    | i > ((length next) - 1) = next
    | otherwise = calc2 a b (replaceAtIndex i (calc a b next prev i omega) next) prev (i+1) omega


replaceAtIndex :: Int -> Double -> [Double] -> [Double]
replaceAtIndex i item ls = a ++ (item:b) where (a, (_:b)) = splitAt i ls


getFirstSum :: [[Double]] -> [Double] -> Int -> Int -> Double
getFirstSum _ _ 0 _= 0
getFirstSum _ _ _ (-1) = 0
getFirstSum a next i j = getSum (a!!i!!j) (a!!i!!i) (next!!j) + getFirstSum a next i (j-1)


getSecondSum :: [[Double]] -> [Double] -> Int -> Int -> Double
getSecondSum a prev i j
    | j < length a = getSum (a!!i!!j) (a!!i!!i) (prev!!j) + getSecondSum a prev i (j+1)
    | otherwise = 0

firstStep :: Double -> Double -> Double
firstStep i omega = (1 - omega) * i

getSum :: Double -> Double -> Double -> Double
getSum aij aii nextj = (aij / aii) * nextj

secondStep :: Double -> Double -> Double
secondStep sum omega = omega * sum

thirdStep :: Double -> Double -> Double -> Double
thirdStep aii bi omega = omega * (bi / aii)

append :: [Double] -> Double -> [Double]
append [] a = [a]
append (x:xs) a = x : append xs a


normI :: [Double] -> Double
normI [x] = abs x
normI (x:xs)
    | abs x > maxTail = abs x
    | otherwise = maxTail
    where maxTail = normI xs


main :: IO ()
main = do
    let omega = 1.1
    let eps = 0.000001

    start_time <- getCurrentTime
    a_text <- readFile "A.txt"
    b_text <- readFile "b.txt"
    let b = map(read::String->Double) (words b_text)
    let n = length b
    let a_vector = map(read::String->Double) (words a_text) --читаем матрицу из файла как список
    let a_maybe = listToMatrix a_vector n --преобразуем список в список списков
    let a = fromJust a_maybe
    let matrix = fromLists a --из списка списков получаем матрицу размера n*n
    let diag_v = getDiag matrix --найдем главную диагональ матрицы
    let diagonal = toList diag_v --преобразуем главную диагональ в список (Data.Vector -> List)
    let c = zipWith (/) b diagonal --получим начальное приближение b[i]/A[i,i]
    let new_a = toLists matrix
    let c2 = calc2 new_a b c c 0 omega
    let res = relaxation new_a b c2 c n omega eps
    print res
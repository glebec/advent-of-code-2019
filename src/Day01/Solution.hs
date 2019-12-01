module Day01.Solution (result) where

import Day01.Input (raw)

masses :: [Int]
masses = read <$> lines raw

getFuel :: Int -> Int
getFuel f = f `quot` 3 - 2

result :: Int
result = sum $ getFuel <$> masses

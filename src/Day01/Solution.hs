module Day01.Solution (result1, result2) where

import Day01.Input (raw)

masses :: [Int]
masses = read <$> lines raw

getFuel :: Int -> Int
getFuel mass = mass `quot` 3 - 2

getTotalFuel :: Int -> Int
getTotalFuel = sum . takeWhile (> 0) . iterate getFuel . getFuel

result1, result2 :: Int
result1 = sum $ getFuel <$> masses
result2 = sum $ getTotalFuel <$> masses

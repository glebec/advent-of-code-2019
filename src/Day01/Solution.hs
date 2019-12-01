module Day01.Solution (result) where

import Day01.Input (raw)

masses :: [Int]
masses = read <$> lines raw

getFuel :: Int -> Int
getFuel mass = max 0 (mass `quot` 3 - 2)

getTotalFuel :: Int -> Int
getTotalFuel mass
    | mass <= 0 = mass
    | otherwise =
        let thisFuel = getFuel mass
        in  thisFuel + getTotalFuel thisFuel

result :: Int
result = sum $ getTotalFuel <$> masses

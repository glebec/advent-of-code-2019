module Main where

import qualified Day01.Solution as D01
import qualified Day02.Solution as D02
import qualified Day03.Solution as D03
import qualified Day04.Solution as D04
import qualified Day06.Solution as D06

main :: IO ()
main = do
    putStrLn $ "Day 01: " ++ show D01.result1 ++ " " ++ show D01.result2
    putStrLn $ "Day 02: " ++ show D02.result1 ++ " " ++ show D02.result2
    putStrLn $ "Day 03: " ++ show D03.result1 ++ " " ++ show D03.result2
    putStrLn $ "Day 04: " ++ show D04.result1 ++ " " ++ show D04.result2
    putStrLn "Day 05: TODO"
    putStrLn $ "Day 06: " ++ show D06.result1

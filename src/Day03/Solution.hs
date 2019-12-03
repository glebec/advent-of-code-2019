{-# LANGUAGE LambdaCase #-}

module Day03.Solution (result1, result2) where

import Prelude hiding (Left, Right)

import Data.List.Split (splitOn)

import Day03.Input (raw)

-- parse

data Direction = Up | Down | Left | Right deriving Eq
data Move = Move Direction Int

parseMove :: String -> Move
parseMove (d:x) =
    let distance = read x
    in  case d of
        'U' -> Move Up distance
        'D' -> Move Down distance
        'L' -> Move Left distance
        'R' -> Move Right distance
parseMove move = error $ "Cannot parse move: " ++ move

-- programList :: [Instruction]
-- programList = read <$> splitOn "," raw

-- solve

result1, result2 :: Int
result1 = undefined
result2 = undefined

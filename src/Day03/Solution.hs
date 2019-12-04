module Day03.Solution (result1, result2) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map as Map

import Day03.Input (raw)

-- parse

data Direction = U | D | L | R deriving Read
data Move = Move Direction Int

parseMove :: String -> Move
parseMove (d:x) = Move (read [d]) (read x)
parseMove move = error $ "Cannot parse move: " ++ move

parseMoves :: String -> [Move]
parseMoves = fmap parseMove . splitOn ","

moves :: ([Move], [Move])
moves = case parseMoves <$> lines raw of
    [a, b] -> (a, b)
    _ -> error "Cannot parse input lines into moves."

-- solve

type Coords = (Int, Int)

toSteps :: [Move] -> [Direction]
toSteps ms = ms >>= \(Move dir dist) -> replicate dist dir

trace :: [Move] -> [Coords]
trace ms = scanl go (0, 0) (toSteps ms) where
    go (x, y) step = case step of
        U -> (x, y + 1)
        D -> (x, y - 1)
        L -> (x + 1, y)
        R -> (x - 1, y)

toGridOfMinSteps :: [Move] -> Map Coords Int
toGridOfMinSteps = Map.fromListWith const . tail . flip zip [0..] . trace

wire1, wire2 :: Map Coords Int
wire1 = toGridOfMinSteps $ fst moves
wire2 = toGridOfMinSteps $ snd moves

crossings :: Map Coords Int
crossings = Map.intersectionWith (+) wire1 wire2

manhattanDistances :: Map Coords Int
manhattanDistances = Map.mapWithKey (\(x, y) _ -> abs x + abs y) crossings

result1 :: Int
result1 = snd . head . Map.toAscList $ manhattanDistances

result2 :: Int
result2 = minimum . fmap snd . Map.toList $ crossings

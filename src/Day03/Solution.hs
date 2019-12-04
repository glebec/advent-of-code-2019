module Day03.Solution (result1, result2) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

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

wire1, wire2 :: Set Coords
wire1 = Set.fromList . tail . trace $ fst moves
wire2 = Set.fromList . tail . trace $ snd moves

crossings :: Set Coords
crossings = Set.intersection wire1 wire2

manhattanDistances :: Set Int
manhattanDistances = Set.map (\(x, y) -> abs x + abs y) crossings

result1 :: Int
result1 = head . Set.toAscList $ manhattanDistances

result2 :: Int
result2 = undefined

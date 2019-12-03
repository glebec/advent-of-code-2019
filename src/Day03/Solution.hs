module Day03.Solution (result1, result2) where

import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

import Day03.Input (raw)

-- parse

data Direction = U | D | L | R deriving (Eq, Read)
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

trace :: [Move] -> [Coords]
trace = go (0, 0) where
    go xy [] = [xy]
    go xy@(x, y) (move:ms') = case move of
        Move _ 0 -> go xy ms'
        Move U d -> xy : go (x, y + 1) (Move U (d - 1) : ms')
        Move D d -> xy : go (x, y - 1) (Move D (d - 1) : ms')
        Move L d -> xy : go (x - 1, y) (Move L (d - 1) : ms')
        Move R d -> xy : go (x + 1, y) (Move R (d - 1) : ms')

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

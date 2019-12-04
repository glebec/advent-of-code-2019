module Day04.Solution (result1, result2) where

import Data.List (group)
import Day04.Input (low, high)

isSixDigits :: Int -> Bool
isSixDigits = (== 6) . length . show

twoAdjacentSame :: Int -> Bool
twoAdjacentSame = check . show where
    check s = or $ zipWith (==) s (tail s)

onlyTwoAdjacentSame :: Int -> Bool
onlyTwoAdjacentSame = check . show where
    check = elem 2 . fmap length . group

monotonicUp :: Int -> Bool
monotonicUp = check . show where
    check s = and $ zipWith (<=) s (tail s)

infixl 3 <&&>
(<&&>) :: Applicative a => a Bool -> a Bool -> a Bool
(<&&>) p q = (&&) <$> p <*> q

result1 :: Int
result1 = length . filter (isSixDigits <&&> twoAdjacentSame <&&> monotonicUp) $ [low..high]

result2 :: Int
result2 = length . filter (isSixDigits <&&> onlyTwoAdjacentSame <&&> monotonicUp) $ [low..high]

-- alternative ways to deal with multiple predicates, credit to github.com/mstksg

-- altSolutionA, altSolutionB :: Int
-- altSolutionA = length . filter (and . sequence [isSixDigits, twoAdjacentSame, monotonicUp]) $ [low..high]
-- altSolutionB = length . filter (\x -> all ($x) [isSixDigits, twoAdjacentSame, monotonicUp]) $ [low..high]

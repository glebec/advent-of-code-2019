module Day04.Solution (result1, result2) where

import Day04.Input (low, high)

isSixDigits :: Int -> Bool
isSixDigits = (== 6) . length . show

twoAdjacentSame :: Int -> Bool
twoAdjacentSame = check . show where
    check s = or $ zipWith (==) s (tail s)

monotonicUp :: Int -> Bool
monotonicUp = check . show where
    check s = and $ zipWith (<=) s (tail s)

infixl 3 &&&
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) p q = (&&) <$> p <*> q

result1 :: Int
result1 = length . filter (isSixDigits &&& twoAdjacentSame &&& monotonicUp) $ [low..high]

result2 :: Int
result2 = undefined

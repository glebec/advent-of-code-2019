{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Day02.Solution (result1, result2) where

import Data.Function ((&))
import Data.Maybe (catMaybes, fromJust)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List.Split (splitOn)

import Day02.Input (raw)

-- aliases

type Instruction = Int
type Noun = Int
type Verb = Int
type Address = Int
type Program = IntMap Instruction

-- parse

programList :: [Instruction]
programList = read <$> splitOn "," raw

program :: Program
program = IM.fromDistinctAscList (zip [0..] programList)
    & IM.insert 1 12
    & IM.insert 2 2

-- solve part 1

data Opcode = Add | Mult | Halt | Err deriving Show

opcode :: Instruction -> Opcode
opcode = \case
    1  -> Add
    2  -> Mult
    99 -> Halt
    _  -> Err

next :: Address -> Address
next = (+4)

run :: Program -> Maybe Int
run = go 0 where
    go i prog = case opcode (IM.findWithDefault 0 i prog) of
        Add -> runAdd i prog & go (next i)
        Mult -> runMult i prog & go (next i)
        Halt -> prog IM.!? 0
        Err -> Nothing

runAdd :: Address -> Program -> Program
runAdd i prog =
    let (a, b, dest) = getVals i prog
    in  IM.insert dest (a + b) prog

runMult :: Address -> Program -> Program
runMult i prog =
    let (a, b, dest) = getVals i prog
    in  IM.insert dest (a * b) prog

getVals :: Address -> Program -> (Int, Int, Address)
getVals i prog =
    let loc1 = prog IM.! (i + 1)
        loc2 = prog IM.! (i + 2)
        loc3 = prog IM.! (i + 3)
        a = prog IM.! loc1
        b = prog IM.! loc2
    in  (a, b, loc3)

result1 :: Int
result1 = fromJust $ run program

-- solve part 2

goal :: Int
goal = 19690720

halted :: [((Noun, Verb), Int)]
halted = catMaybes $ do
    noun <- [0..99]
    verb <- [0..99]
    let candidate = program & IM.insert 1 noun & IM.insert 2 verb
    pure $ sequenceA ((noun, verb), run candidate)

nounAndVerb :: (Noun, Verb)
nounAndVerb = fst . head . filter (\(_, r) -> r == goal) $ halted

result2 :: Int
result2 = 100 * fst nounAndVerb + snd nounAndVerb

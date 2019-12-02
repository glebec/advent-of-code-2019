{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Day02.Solution (result1, result2) where

import Data.Function ((&))

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List.Split (splitOn)

import Day02.Input (raw)

-- aliases

type Instruction = Int
type Address = Int
type Program = IntMap Instruction

-- parse

programList :: [Instruction]
programList = read <$> splitOn "," raw

program :: Program
program = IM.fromDistinctAscList (zip [0..] programList)
    & IM.insert 1 12
    & IM.insert 2 2

-- solve

data Opcode = Add | Mult | Halt | Err Instruction deriving Show

opcode :: Instruction -> Opcode
opcode = \case
    1  -> Add
    2  -> Mult
    99 -> Halt
    i  -> Err i

next :: Address -> Address
next = (+4)

run :: Program -> Int
run = go 0 where
    go i prog = case opcode (IM.findWithDefault 99 i prog) of
        Add -> runAdd i prog & go (next i)
        Mult -> runMult i prog & go (next i)
        Halt -> prog IM.! 0
        e -> error $ "Unrecognized opcode. " ++ show e

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

result1, result2 :: Int
result1 = run program
result2 = 0

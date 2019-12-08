module Day06.Solution (result1, result2) where

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map as Map

import Day06.Input (raw)

-- parse

newtype Satellite = Satellite String deriving (Eq, Ord, Show)
newtype Center = Center String deriving (Eq, Ord, Show)
newtype Orbit = Orbit {unOrbit :: (Satellite, Center)} deriving (Eq, Ord, Show)

parseOrbit :: String -> Orbit
parseOrbit str = case splitOn ")" str of
    (ctr:sat:_) -> Orbit (Satellite sat, Center ctr)
    _ -> error $ "Could not parse orbit notation " ++ str

orbitList :: [Orbit]
orbitList = parseOrbit <$> lines raw

-- solve

getTree :: [Orbit] -> Map Satellite Center
getTree = Map.fromList . fmap unOrbit

-- tie the knot (memoization via mutual recursion)
getRootPaths :: Map Satellite Center -> Map Satellite [String]
getRootPaths orbits =
    let rootPaths = fmap pathToRoot orbits
        pathToRoot :: Center -> [String]
        pathToRoot (Center x) = (x:) $ case Map.lookup (Satellite x) rootPaths of
            Just xs -> xs
            Nothing -> maybe [] pathToRoot (Map.lookup (Satellite x) orbits)
    in  rootPaths

result1 :: Int
result1 = sum . fmap length . getRootPaths . getTree $ orbitList

result2 :: String
result2 = undefined

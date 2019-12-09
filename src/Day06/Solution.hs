module Day06.Solution
    ( result1
    , result2
    ) where

import Data.List (elemIndex, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Day06.Input (raw)

-- parse

newtype Satellite =
    Satellite String
    deriving (Eq, Ord, Show)

newtype Center =
    Center String
    deriving (Eq, Ord, Show)

newtype Orbit =
    Orbit
        { unOrbit :: (Satellite, Center)
        }
    deriving (Eq, Ord, Show)

parseOrbit :: String -> Orbit
parseOrbit str =
    case splitOn ")" str of
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
        pathToRoot (Center x) =
            (x :) $
            case Map.lookup (Satellite x) rootPaths of
                Just xs -> xs
                Nothing -> maybe [] pathToRoot (Map.lookup (Satellite x) orbits)
     in rootPaths

result1 :: Int
result1 = sum . fmap length . getRootPaths . getTree $ orbitList

-- part 2

findCommonOrbit :: Map Satellite [String] -> String
findCommonOrbit m =
    let santaOrbits = Set.fromList (Map.findWithDefault [] (Satellite "SAN") m)
    in  fromMaybe "" $
        find
            (`Set.member` santaOrbits)
            (Map.findWithDefault [] (Satellite "YOU") m)

result2 :: Int
result2 =
    let paths :: Map Satellite [String]
        paths = getRootPaths . getTree $ orbitList
        common :: String
        common = findCommonOrbit paths
        maybeSantaDist :: Maybe Int
        maybeSantaDist =
            elemIndex common (Map.findWithDefault [] (Satellite "SAN") paths)
        maybeYouDist :: Maybe Int
        maybeYouDist =
            elemIndex common (Map.findWithDefault [] (Satellite "YOU") paths)
    in  fromMaybe (-1) $ do
            santaDist <- maybeSantaDist
            youDist <- maybeYouDist
            pure $ case common of
                "SAN" -> youDist
                "YOU" -> santaDist
                _ -> youDist + santaDist

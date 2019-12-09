{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day08.Solution
    ( result1
    , result2
    ) where

import Data.Char (digitToInt)
import Data.List (intercalate)

import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Day08.Input (raw)

-- parse
type Width = Int

type Height = Int

type Coords = (Height, Width)

newtype Layer a =
    Layer
        { unLayer :: Map Coords a
        }
    deriving (Functor, Show)

instance Semigroup a => Semigroup (Layer a) where
    (<>) (Layer l) (Layer l') = Layer $ M.unionWith (<>) l l'

instance Monoid a => Monoid (Layer a) where
    mempty = Layer mempty

newtype Image a =
    Image
        { unImage :: [Layer a]
        }
    deriving (Functor, Show)

parseImage :: Width -> Height -> String -> Image Int
parseImage w h s =
    let digits = digitToInt <$> s
        coords = do
            line <- [0 .. h - 1]
            col <- [0 .. w - 1]
            pure (line, col)
     in Image $ do
            layerDigits <- chunksOf (w * h) digits
            pure . Layer . M.fromDistinctAscList $ zip coords layerDigits

width :: Width
width = 25

height :: Height
height = 6

image :: Image Int
image = parseImage width height raw

-- solve
countInLayer :: Int -> Layer Int -> Int
countInLayer x (Layer m) = length $ M.filter (== x) m

layerWithFewest0s :: Image Int -> Layer Int
layerWithFewest0s (Image layers) = foldl1 go layers
  where
    go l1 l2 =
        if (0 `countInLayer` l1) < (0 `countInLayer` l2)
            then l1
            else l2

result1 :: Int
result1 =
    let lowest0s = layerWithFewest0s image
     in (1 `countInLayer` lowest0s) * (2 `countInLayer` lowest0s)

-- part 2
data Px
    = Black
    | White
    | Clear
    deriving (Enum, Show)

instance Semigroup Px where
    Clear <> x = x
    x <> _ = x

instance Monoid Px where
    mempty = Clear

intToPx :: Int -> Px
intToPx = toEnum

pxToChar :: Px -> Char
pxToChar =
    \case
        Black -> 'X'
        White -> '.'
        Clear -> '_'

flattenImage :: Monoid a => Image a -> Layer a
flattenImage = mconcat . unImage

renderLayer :: Width -> Height -> Layer Px -> String
renderLayer w h (Layer m) = intercalate "\n" (fmap toRow [0 .. h - 1])
  where
    toRow :: Height -> String
    toRow line = do
        col <- [0 .. w - 1]
        pure . pxToChar . M.findWithDefault Clear (line, col) $ m

result2 :: String
result2 = renderLayer width height . flattenImage . fmap intToPx $ image

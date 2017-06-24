-- | See http://joelgrus.com/2015/06/12/on-the-mathematics-of-spot-it/
--   for the theory behind this
module PuxIt.Math where

import Prelude
import Data.Array ((..), length, zip, cons)
import Data.Maybe (fromJust)
import Data.Map as Map
import Partial.Unsafe (unsafePartial)

-- | We define three types of points:
-- * an ordinary point (x, y)
-- * a point at infinity corresponding to a slope m
-- * the point at infinity corresponding to infinite slope
data Point = OrdinaryPoint Int Int
           | PointAtInfinity Int
           | VerticalInfinity

-- We need Eq and Ord instances to use as keys in a Map.
derive instance eqPoint :: Eq Point
derive instance ordPoint :: Ord Point

-- It's also handy to have a Show instance for debugging.
instance showPoint :: Show Point where
  show (OrdinaryPoint x y) = "Point " <> show x <> "," <> show y
  show (PointAtInfinity m) = "Infinity " <> show m
  show VerticalInfinity    = "Infinity Infinity"

-- Generate all the points corresponding to a given `n`
allPoints :: Int -> Array Point
allPoints n = ordinaryPoints <> infinitePoints
  where
    ordinaryPoints = do
      x <- 0 .. (n - 1)
      y <- 0 .. (n - 1)
      pure $ OrdinaryPoint x y
    infinitePoints = cons VerticalInfinity $ do
      m <- 0 .. (n - 1)
      pure $ PointAtInfinity m

-- | Similarly, we define three types of lines:
-- * an ordinary line with slope m and intercept b
-- * a vertical line through (x, 0)
-- * the line at infinity
data Line = OrdinaryLine Int Int
          | VerticalLine Int
          | LineAtInfinity

-- Generate all the lines corresponding to a size `n`
allLines :: Int -> Array Line
allLines n = ordinaryLines <> verticalLines <> [LineAtInfinity]
  where
    ordinaryLines = do
      m <- 0 .. (n - 1)
      b <- 0 .. (n - 1)
      pure $ OrdinaryLine m b
    verticalLines = do
      x <- 0 .. (n - 1)
      pure $ VerticalLine x

-- Generate all the points on a given line.
pointsOnLine :: Int -> Line -> Array Point
pointsOnLine n line = case line of
  -- an ordinary line y = mx + b consists of all points (x, mx+b `mod` n)
  -- for x in [0 .. n-1], and also the point "Infinity m"
  OrdinaryLine m b -> cons (PointAtInfinity m) $ do
    x <- 0 .. (n - 1)
    let y = (m * x + b) `mod` n
    pure $ OrdinaryPoint x y
  -- a vertical line at x consists of all (x, y) for y in [0 .. n-1]
  -- and also the point "VerticalInfinity"
  VerticalLine x -> cons VerticalInfinity $ do
    y <- 0 .. (n - 1)
    pure $ OrdinaryPoint x y
  -- the "line at infinity" consists of all the points "Infinity m"
  -- for m in [0 .. n-1] and also the point "VerticalInfinity"
  LineAtInfinity -> cons VerticalInfinity $ do
    m <- 0 .. (n - 1)
    pure $ PointAtInfinity m

type Card = Array Int

-- Creates the deck corresponding to the prime number `n`. The deck will have
-- n^2 + n + 1 "cards" (lines), each containing a subset of size n+1 of
-- n^2 + n + 1 "images" (points). We number the points from 0 to n^2 + n
-- and represent each "card" as simply an Array Int
createDeck :: Int -> Array Card
createDeck n = map (toIndexes <<< pointsOnLine n) (allLines n)
  where
    points = allPoints n
    numPoints = length points
    -- a Map : Point -> Int that gives each point's index in the `points` array
    encoding = Map.fromFoldable $ zip points (0 .. (numPoints - 1))
    -- replace an array of Points with their indexes from `encoding`
    toIndexes = map (\p -> unsafePartial $ fromJust $ Map.lookup p encoding)

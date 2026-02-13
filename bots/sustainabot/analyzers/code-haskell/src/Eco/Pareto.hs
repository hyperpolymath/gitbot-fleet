-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2024-2025 hyperpolymath

{-# LANGUAGE OverloadedStrings #-}

-- | Pareto optimality analysis for multi-objective optimization
--
-- Implements Pareto frontier calculation and dominance checking
-- for balancing competing software objectives (performance, memory,
-- energy, maintainability, etc.)
module Eco.Pareto
  ( -- * Pareto Analysis
    calculateParetoFrontier
  , isDominated
  , paretoDistance
  , findDominatingPoints

    -- * Objectives
  , Objective(..)
  , ObjectiveDirection(..)
  , standardObjectives

    -- * Trade-off Analysis
  , analyzeTradeoffs
  , suggestImprovements
  ) where

import Types.Metrics
import Data.List (sortBy, nubBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- | Direction of optimization for an objective
data ObjectiveDirection
  = Minimize  -- ^ Lower is better (e.g., latency, memory)
  | Maximize  -- ^ Higher is better (e.g., throughput, coverage)
  deriving (Show, Eq)

-- | Definition of an optimization objective
data Objective = Objective
  { objName      :: !Text
  , objDirection :: !ObjectiveDirection
  , objWeight    :: !Double  -- ^ Importance weight (0-1)
  } deriving (Show, Eq)

-- | Standard objectives for Oikos Bot analysis
standardObjectives :: [Objective]
standardObjectives =
  [ Objective "carbon_intensity" Minimize 0.20
  , Objective "energy_consumption" Minimize 0.15
  , Objective "execution_time" Minimize 0.15
  , Objective "memory_usage" Minimize 0.10
  , Objective "maintainability" Maximize 0.15
  , Objective "test_coverage" Maximize 0.10
  , Objective "technical_debt" Minimize 0.15
  ]

-- | Calculate the Pareto frontier from a set of solutions
--
-- A solution is Pareto-optimal if no other solution dominates it
-- (i.e., no other solution is better in all objectives)
calculateParetoFrontier :: [Objective] -> [[Double]] -> ParetoFrontier
calculateParetoFrontier objectives solutions = ParetoFrontier
  { pfPoints = map toPoint indexed
  , pfObjectives = map objName objectives
  , pfCurrentPos = head $ map toPoint indexed  -- First point as current
  , pfDistance = 0  -- Will be calculated relative to frontier
  }
  where
    indexed = zip solutions [0..]

    toPoint (values, _idx) = ParetoPoint
      { ppDimensions = values
      , ppLabels = map objName objectives
      , ppDominated = isDominated objectives values solutions
      }

-- | Check if a solution is dominated by any other solution
isDominated :: [Objective] -> [Double] -> [[Double]] -> Bool
isDominated objectives point allPoints =
  any (dominates objectives point) (filter (/= point) allPoints)

-- | Check if point A dominates point B
-- A dominates B if A is at least as good in all objectives
-- and strictly better in at least one
dominates :: [Objective] -> [Double] -> [Double] -> Bool
dominates objectives pointA pointB =
  allAtLeastAsGood && anyStrictlyBetter
  where
    comparisons = zipWith3 compareObjective objectives pointA pointB

    allAtLeastAsGood = all (>= EQ) comparisons
    anyStrictlyBetter = any (== GT) comparisons

    compareObjective obj a b = case objDirection obj of
      Minimize -> compare b a  -- Lower is better, so flip
      Maximize -> compare a b  -- Higher is better

-- | Calculate distance from a point to the Pareto frontier
paretoDistance :: [Objective] -> [Double] -> [[Double]] -> Double
paretoDistance objectives point frontier =
  minimum $ map (euclideanDistance point) nonDominatedPoints
  where
    nonDominatedPoints = filter (not . flip (isDominated objectives) frontier) frontier

    euclideanDistance a b = sqrt $ sum $ zipWith (\x y -> (x - y) ^ (2 :: Int)) a b

-- | Find all points that dominate the given point
findDominatingPoints :: [Objective] -> [Double] -> [[Double]] -> [[Double]]
findDominatingPoints objectives point allPoints =
  filter (\p -> dominates objectives p point) (filter (/= point) allPoints)

-- | Analyze trade-offs between objectives
analyzeTradeoffs :: [Objective] -> [Double] -> [(Text, Text, Double)]
analyzeTradeoffs objectives values =
  [ (objName o1, objName o2, correlation v1 v2)
  | (o1, v1) <- zip objectives values
  , (o2, v2) <- zip objectives values
  , objName o1 < objName o2  -- Avoid duplicates
  ]
  where
    -- Simplified correlation (would use actual correlation in practice)
    correlation a b = (a * b) / (abs a + abs b + 0.001)

-- | Suggest improvements to move toward Pareto frontier
suggestImprovements :: [Objective] -> [Double] -> [[Double]] -> [Text]
suggestImprovements objectives current frontier =
  concatMap suggest $ zip objectives current
  where
    frontierMeans = map mean $ transpose frontier

    suggest (obj, val) =
      let targetIdx = findObjIndex (objName obj) objectives
          target = frontierMeans !! targetIdx
          diff = case objDirection obj of
            Minimize -> val - target
            Maximize -> target - val
      in if diff > 0.1  -- Threshold for suggesting
         then [T.concat [objName obj, ": improve by ", T.pack (show (round (diff * 100) :: Int)), "%"]]
         else []

    findObjIndex name objs =
      length $ takeWhile ((/= name) . objName) objs

    mean xs = sum xs / fromIntegral (length xs)

    transpose [] = []
    transpose ([] : _) = []
    transpose xs = map head xs : transpose (map tail xs)

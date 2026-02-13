{-# LANGUAGE OverloadedStrings #-}

-- | Coupling and cohesion analysis
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Implements Robert C. Martin's instability/abstractness metrics
-- and LCOM (Lack of Cohesion of Methods).
module Quality.Coupling
  ( analyzeCoupling
  , calculateInstability
  , calculateAbstractness
  , calculateDistance
  , CouplingConfig(..)
  , defaultCouplingConfig
  , ModuleDependency(..)
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub)

-- | Configuration for coupling analysis
data CouplingConfig = CouplingConfig
  { cpMaxAfferent     :: !Int      -- ^ Max incoming dependencies
  , cpMaxEfferent     :: !Int      -- ^ Max outgoing dependencies
  , cpMaxInstability  :: !Double   -- ^ Max instability (0-1)
  , cpMaxDistance     :: !Double   -- ^ Max distance from main sequence
  } deriving (Show, Eq)

-- | Default coupling thresholds
defaultCouplingConfig :: CouplingConfig
defaultCouplingConfig = CouplingConfig
  { cpMaxAfferent = 20
  , cpMaxEfferent = 10
  , cpMaxInstability = 0.8
  , cpMaxDistance = 0.3
  }

-- | Represents a module's dependency information
data ModuleDependency = ModuleDependency
  { mdName        :: !Text
  , mdImports     :: ![Text]       -- ^ Modules this module imports
  , mdExports     :: ![Text]       -- ^ Public interface items
  , mdAbstracts   :: !Int          -- ^ Abstract types/interfaces
  , mdConcretes   :: !Int          -- ^ Concrete implementations
  , mdClasses     :: !Int          -- ^ Number of classes/modules
  } deriving (Show, Eq)

-- | Calculate coupling metrics for a set of modules
analyzeCoupling :: CouplingConfig -> [ModuleDependency] -> CouplingScore
analyzeCoupling config modules = CouplingScore
  { csAfferent = totalAfferent
  , csEfferent = totalEfferent
  , csInstability = avgInstability
  , csAbstractness = avgAbstractness
  , csDistance = avgDistance
  }
  where
    -- Calculate afferent coupling (incoming) for each module
    afferentCounts = map (countAfferent modules) modules
    totalAfferent = sum afferentCounts

    -- Calculate efferent coupling (outgoing) for each module
    efferentCounts = map (length . mdImports) modules
    totalEfferent = sum efferentCounts

    -- Calculate metrics per module
    instabilities = zipWith (\a e -> calculateInstability a e) afferentCounts efferentCounts
    abstractnesses = map calculateAbstractnessForModule modules
    distances = zipWith calculateDistance instabilities abstractnesses

    -- Averages
    avgInstability = safeAverage instabilities
    avgAbstractness = safeAverage abstractnesses
    avgDistance = safeAverage distances

    safeAverage xs = if null xs then 0 else sum xs / fromIntegral (length xs)

-- | Count how many modules import this module
countAfferent :: [ModuleDependency] -> ModuleDependency -> Int
countAfferent allModules targetModule =
  length $ filter importsTarget allModules
  where
    importsTarget m = mdName targetModule `elem` mdImports m

-- | Calculate instability: I = Ce / (Ca + Ce)
-- 0 = stable (many dependents, few dependencies)
-- 1 = unstable (few dependents, many dependencies)
calculateInstability :: Int -> Int -> Double
calculateInstability afferent efferent
  | afferent + efferent == 0 = 0
  | otherwise = fromIntegral efferent / fromIntegral (afferent + efferent)

-- | Calculate abstractness for a module: A = abstracts / (abstracts + concretes)
calculateAbstractnessForModule :: ModuleDependency -> Double
calculateAbstractnessForModule m
  | total == 0 = 0
  | otherwise = fromIntegral (mdAbstracts m) / fromIntegral total
  where
    total = mdAbstracts m + mdConcretes m

-- | Calculate abstractness: A = abstract_classes / total_classes
calculateAbstractness :: Int -> Int -> Double
calculateAbstractness abstractCount totalCount
  | totalCount == 0 = 0
  | otherwise = fromIntegral abstractCount / fromIntegral totalCount

-- | Calculate distance from main sequence: D = |A + I - 1|
-- 0 = on the main sequence (ideal)
-- Closer to 1 = either "zone of pain" or "zone of uselessness"
calculateDistance :: Double -> Double -> Double
calculateDistance instability abstractness =
  abs (abstractness + instability - 1)

-- | Analyze LCOM (Lack of Cohesion of Methods)
-- LCOM = 1 - (sum of method intersections / (m * a))
-- where m = methods, a = attributes
analyzeLCOM :: ModuleDependency -> Double
analyzeLCOM m
  | mdClasses m == 0 = 0
  | otherwise = 1.0 - cohesionEstimate
  where
    -- Simplified estimate based on exports vs total
    exportCount = length $ mdExports m
    totalItems = mdAbstracts m + mdConcretes m
    cohesionEstimate = if totalItems == 0
                       then 1.0
                       else fromIntegral exportCount / fromIntegral totalItems

-- | Suggest coupling improvements
suggestCouplingImprovements :: CouplingConfig -> [ModuleDependency] -> [Text]
suggestCouplingImprovements config modules = concatMap suggest modules
  where
    suggest m = concat
      [ [ T.concat ["Module '", mdName m, "': Too many dependencies (", T.pack (show deps), ")"]
        | deps > cpMaxEfferent config
        ]
      , [ T.concat ["Module '", mdName m, "': Consider introducing abstractions"]
        | calculateAbstractnessForModule m < 0.1 && mdClasses m > 5
        ]
      , [ T.concat ["Module '", mdName m, "': High instability - add abstractions or reduce dependencies"]
        | calculateInstability afferent (length $ mdImports m) > cpMaxInstability config
        ]
      ]
      where
        deps = length $ mdImports m
        afferent = countAfferent modules m

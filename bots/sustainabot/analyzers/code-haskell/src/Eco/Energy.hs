{-# LANGUAGE OverloadedStrings #-}

-- | Energy efficiency pattern analysis
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Detects energy-inefficient code patterns and suggests improvements
-- based on software energy consumption research.
module Eco.Energy
  ( analyzeEnergyPatterns
  , detectBusyWaiting
  , detectIneffientLoops
  , detectBlockingIO
  , detectRedundantComputation
  , EnergyConfig(..)
  , defaultEnergyConfig
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for energy analysis
data EnergyConfig = EnergyConfig
  { ecBusyWaitThreshold    :: !Int     -- ^ Spin iterations before flagging
  , ecLoopUnrollThreshold  :: !Int     -- ^ Loop count before suggesting unroll
  , ecIOBatchThreshold     :: !Int     -- ^ IO calls before suggesting batch
  , ecCacheableThreshold   :: !Int     -- ^ Repeated computations to flag
  } deriving (Show, Eq)

-- | Default energy analysis configuration
defaultEnergyConfig :: EnergyConfig
defaultEnergyConfig = EnergyConfig
  { ecBusyWaitThreshold = 100
  , ecLoopUnrollThreshold = 1000
  , ecIOBatchThreshold = 10
  , ecCacheableThreshold = 3
  }

-- | Source code analysis input (simplified AST representation)
data SourceAnalysis = SourceAnalysis
  { saLoops           :: ![LoopInfo]
  , saIOCalls         :: ![IOCallInfo]
  , saFunctionCalls   :: ![FunctionCallInfo]
  , saSpinLocks       :: ![SpinLockInfo]
  } deriving (Show, Eq)

data LoopInfo = LoopInfo
  { liLocation   :: !CodeLocation
  , liIterations :: !(Maybe Int)    -- ^ Estimated iterations if known
  , liBody       :: !Text           -- ^ Loop body snippet
  , liNested     :: !Int            -- ^ Nesting depth
  } deriving (Show, Eq)

data IOCallInfo = IOCallInfo
  { ioLocation :: !CodeLocation
  , ioType     :: !Text             -- ^ file, network, database
  , ioBlocking :: !Bool
  } deriving (Show, Eq)

data FunctionCallInfo = FunctionCallInfo
  { fcLocation  :: !CodeLocation
  , fcName      :: !Text
  , fcCallCount :: !Int             -- ^ Number of times called
  , fcPure      :: !Bool            -- ^ Is it a pure function?
  } deriving (Show, Eq)

data SpinLockInfo = SpinLockInfo
  { slLocation   :: !CodeLocation
  , slIterations :: !Int
  } deriving (Show, Eq)

-- | Analyze source for energy patterns
analyzeEnergyPatterns :: EnergyConfig -> SourceAnalysis -> EnergyScore
analyzeEnergyPatterns config source = EnergyScore
  { energyPatterns = patterns
  , energyNormalized = normalizeEnergyScore patterns
  , energyHotspots = map patternLocation patterns
  }
  where
    patterns = concat
      [ detectBusyWaiting config (saSpinLocks source)
      , detectIneffientLoops config (saLoops source)
      , detectBlockingIO config (saIOCalls source)
      , detectRedundantComputation config (saFunctionCalls source)
      ]

    patternLocation (BusyWaiting loc) = loc
    patternLocation (IneffientLoop loc) = loc
    patternLocation (BlockingIO loc) = loc
    patternLocation (RedundantComputation loc) = loc
    patternLocation (EfficientPattern loc) = loc

-- | Detect busy waiting / spin locks
detectBusyWaiting :: EnergyConfig -> [SpinLockInfo] -> [EnergyPattern]
detectBusyWaiting config = map toBusyWait . filter isExcessive
  where
    isExcessive sl = slIterations sl > ecBusyWaitThreshold config
    toBusyWait sl = BusyWaiting (slLocation sl)

-- | Detect inefficient loops (too many iterations, deep nesting)
detectIneffientLoops :: EnergyConfig -> [LoopInfo] -> [EnergyPattern]
detectIneffientLoops config = map toInefficient . filter isInefficient
  where
    isInefficient li =
      maybe False (> ecLoopUnrollThreshold config) (liIterations li)
      || liNested li > 3

    toInefficient li = IneffientLoop (liLocation li)

-- | Detect blocking I/O that could be async
detectBlockingIO :: EnergyConfig -> [IOCallInfo] -> [EnergyPattern]
detectBlockingIO _config = map toBlocking . filter ioBlocking
  where
    toBlocking io = BlockingIO (ioLocation io)

-- | Detect redundant computations that could be cached/memoized
detectRedundantComputation :: EnergyConfig -> [FunctionCallInfo] -> [EnergyPattern]
detectRedundantComputation config = map toRedundant . filter isRedundant
  where
    isRedundant fc =
      fcPure fc && fcCallCount fc >= ecCacheableThreshold config

    toRedundant fc = RedundantComputation (fcLocation fc)

-- | Normalize energy patterns to 0-100 score (100 = best)
normalizeEnergyScore :: [EnergyPattern] -> Double
normalizeEnergyScore patterns
  | null patterns = 100  -- No issues found
  | otherwise = max 0 $ 100 - fromIntegral (length patterns) * 10

-- | Placeholder for AST parsing (would use tree-sitter in real impl)
parseSourceToAnalysis :: Text -> SourceAnalysis
parseSourceToAnalysis _source = SourceAnalysis
  { saLoops = []
  , saIOCalls = []
  , saFunctionCalls = []
  , saSpinLocks = []
  }

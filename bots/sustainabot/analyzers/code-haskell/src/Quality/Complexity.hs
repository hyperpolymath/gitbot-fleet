{-# LANGUAGE OverloadedStrings #-}

-- | Cyclomatic and cognitive complexity analysis
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Implements complexity metrics based on:
-- - McCabe's Cyclomatic Complexity
-- - Cognitive Complexity (SonarSource)
-- - Halstead complexity measures
module Quality.Complexity
  ( analyzeComplexity
  , calculateCyclomatic
  , calculateCognitive
  , calculateMaintainability
  , ComplexityConfig(..)
  , defaultComplexityConfig
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for complexity analysis
data ComplexityConfig = ComplexityConfig
  { ccMaxCyclomatic      :: !Int     -- ^ Maximum acceptable cyclomatic complexity
  , ccMaxCognitive       :: !Int     -- ^ Maximum acceptable cognitive complexity
  , ccMinMaintainability :: !Double  -- ^ Minimum maintainability index
  , ccMaxFunctionLines   :: !Int     -- ^ Maximum lines per function
  } deriving (Show, Eq)

-- | Default complexity thresholds
defaultComplexityConfig :: ComplexityConfig
defaultComplexityConfig = ComplexityConfig
  { ccMaxCyclomatic = 10        -- Industry standard threshold
  , ccMaxCognitive = 15         -- SonarSource recommendation
  , ccMinMaintainability = 20   -- Below this is very difficult to maintain
  , ccMaxFunctionLines = 50     -- Functions should be concise
  }

-- | Simplified AST for complexity analysis
data FunctionAST = FunctionAST
  { astName        :: !Text
  , astLocation    :: !CodeLocation
  , astBranches    :: !Int         -- ^ if, switch, ternary
  , astLoops       :: !Int         -- ^ for, while, do-while
  , astNesting     :: !Int         -- ^ Maximum nesting depth
  , astOperators   :: !Int         -- ^ Distinct operators (Halstead)
  , astOperands    :: !Int         -- ^ Distinct operands (Halstead)
  , astTotalOps    :: !Int         -- ^ Total operator occurrences
  , astTotalOpnds  :: !Int         -- ^ Total operand occurrences
  , astLines       :: !Int         -- ^ Lines of code
  } deriving (Show, Eq)

-- | Analyze complexity of code
analyzeComplexity :: ComplexityConfig -> [FunctionAST] -> ComplexityMetrics
analyzeComplexity config functions = ComplexityMetrics
  { cmCyclomatic = maxCyclomatic
  , cmCognitive = maxCognitive
  , cmLinesOfCode = totalLines
  , cmMaintainability = avgMaintainability
  , cmHotspots = hotspots
  }
  where
    cyclomatics = map (calculateCyclomatic config) functions
    cognitives = map (calculateCognitive config) functions
    maintainabilities = map (calculateMaintainability config) functions

    maxCyclomatic = if null cyclomatics then 0 else maximum cyclomatics
    maxCognitive = if null cognitives then 0 else maximum cognitives
    totalLines = sum $ map astLines functions
    avgMaintainability = if null maintainabilities
                         then 100
                         else sum maintainabilities / fromIntegral (length maintainabilities)

    -- Hotspots are functions exceeding thresholds
    hotspots = map astLocation $ filter isHotspot functions

    isHotspot fn =
      calculateCyclomatic config fn > ccMaxCyclomatic config ||
      calculateCognitive config fn > ccMaxCognitive config ||
      astLines fn > ccMaxFunctionLines config

-- | Calculate McCabe's Cyclomatic Complexity
-- CC = E - N + 2P
-- Simplified: CC = branches + loops + 1
calculateCyclomatic :: ComplexityConfig -> FunctionAST -> Int
calculateCyclomatic _config fn =
  astBranches fn + astLoops fn + 1

-- | Calculate Cognitive Complexity
-- Based on SonarSource's metric:
-- - +1 for each control flow break
-- - Additional +1 for each level of nesting
calculateCognitive :: ComplexityConfig -> FunctionAST -> Int
calculateCognitive _config fn =
  baseComplexity + nestingPenalty
  where
    baseComplexity = astBranches fn + astLoops fn
    nestingPenalty = if astNesting fn > 2
                     then (astNesting fn - 2) * (astBranches fn + astLoops fn)
                     else 0

-- | Calculate Maintainability Index
-- MI = 171 - 5.2 * ln(V) - 0.23 * CC - 16.2 * ln(LOC)
-- Where V = Halstead Volume
calculateMaintainability :: ComplexityConfig -> FunctionAST -> Double
calculateMaintainability config fn =
  max 0 $ min 100 $ scaled
  where
    cc = fromIntegral $ calculateCyclomatic config fn
    loc = fromIntegral $ max 1 $ astLines fn

    -- Halstead Volume = N * log2(n)
    -- N = total operators + operands
    -- n = distinct operators + operands
    totalN = fromIntegral $ astTotalOps fn + astTotalOpnds fn
    distinctN = fromIntegral $ max 1 $ astOperators fn + astOperands fn
    volume = if totalN > 0 && distinctN > 1
             then totalN * logBase 2 distinctN
             else 1

    -- Original Maintainability Index formula
    mi = 171 - 5.2 * log volume - 0.23 * cc - 16.2 * log loc

    -- Scale to 0-100
    scaled = mi * 100 / 171

-- | Suggest complexity improvements
suggestComplexityImprovements :: ComplexityConfig -> [FunctionAST] -> [Text]
suggestComplexityImprovements config functions = concatMap suggest functions
  where
    suggest fn = concat
      [ [ T.concat ["Function '", astName fn, "': Extract smaller functions (CC=", T.pack (show cc), ")"]
        | cc > ccMaxCyclomatic config
        ]
      , [ T.concat ["Function '", astName fn, "': Reduce nesting depth (", T.pack (show $ astNesting fn), " levels)"]
        | astNesting fn > 3
        ]
      , [ T.concat ["Function '", astName fn, "': Split function (", T.pack (show $ astLines fn), " lines)"]
        | astLines fn > ccMaxFunctionLines config
        ]
      ]
      where
        cc = calculateCyclomatic config fn

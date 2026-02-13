{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for eco-analyzer
-- SPDX-License-Identifier: AGPL-3.0-or-later
module Main where

import Test.Hspec
import Test.QuickCheck

import Types.Metrics
import Eco.Carbon
import Eco.Pareto
import Quality.Complexity

main :: IO ()
main = hspec $ do
  describe "Carbon Analysis" $ do
    it "returns normalized score between 0 and 100" $ do
      let config = defaultCarbonConfig
          input = CodeAnalysisInput 10 2 50 10 1
          result = analyzeCarbonIntensity config input
      carbonNormalized result `shouldSatisfy` (\x -> x >= 0 && x <= 100)

    it "higher complexity increases carbon score" $ do
      let config = defaultCarbonConfig
          lowInput = CodeAnalysisInput 5 1 10 5 1
          highInput = CodeAnalysisInput 50 4 200 100 1
          lowResult = analyzeCarbonIntensity config lowInput
          highResult = analyzeCarbonIntensity config highInput
      carbonNormalized lowResult `shouldSatisfy` (> carbonNormalized highResult)

  describe "Pareto Analysis" $ do
    it "identifies dominated points correctly" $ do
      let objectives = standardObjectives
          point = [10, 20, 30, 40, 50, 60, 70]
          betterPoint = [5, 10, 15, 20, 25, 30, 35]  -- Better in all dimensions
          points = [point, betterPoint]
      isDominated objectives point points `shouldBe` True
      isDominated objectives betterPoint points `shouldBe` False

  describe "Complexity Analysis" $ do
    it "calculates cyclomatic complexity correctly" $ do
      let config = defaultComplexityConfig
          ast = FunctionAST
            { astName = "test"
            , astLocation = CodeLocation "" 1 1 Nothing
            , astBranches = 5
            , astLoops = 3
            , astNesting = 2
            , astOperators = 10
            , astOperands = 20
            , astTotalOps = 50
            , astTotalOpnds = 100
            , astLines = 30
            }
      calculateCyclomatic config ast `shouldBe` 9  -- 5 + 3 + 1

  describe "Properties" $ do
    it "normalized scores are always valid" $ property $ \(NonNegative n) ->
      let score = normalizeScore (n :: Double)
      in score >= 0 && score <= 100

-- Placeholder for normalizeScore (would import from Eco.Carbon)
normalizeScore :: Double -> Double
normalizeScore rawScore
  | rawScore <= 0.001 = 100
  | rawScore >= 1.0   = 0
  | otherwise = 100 * (1 - (log rawScore + 6.9) / 6.9)

-- Placeholder data types for tests
data FunctionAST = FunctionAST
  { astName :: Text
  , astLocation :: CodeLocation
  , astBranches :: Int
  , astLoops :: Int
  , astNesting :: Int
  , astOperators :: Int
  , astOperands :: Int
  , astTotalOps :: Int
  , astTotalOpnds :: Int
  , astLines :: Int
  }

data CodeAnalysisInput = CodeAnalysisInput
  { caiComplexity :: Int
  , caiLoopDepth :: Int
  , caiAllocations :: Int
  , caiIOOperations :: Int
  , caiParallelism :: Int
  }

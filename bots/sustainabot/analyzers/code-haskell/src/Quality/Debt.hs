{-# LANGUAGE OverloadedStrings #-}

-- | Technical debt estimation
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Estimates technical debt using SQALE methodology.
module Quality.Debt
  ( analyzeDebt
  , estimateDebtPrincipal
  , estimateDebtInterest
  , calculateDebtRatio
  , DebtConfig(..)
  , defaultDebtConfig
  , DebtType(..)
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for debt analysis
data DebtConfig = DebtConfig
  { dcHoursPerComplexityPoint :: !Double   -- ^ Hours to fix per complexity point
  , dcHoursPerDuplication     :: !Double   -- ^ Hours to fix per duplicated block
  , dcHoursPerCodeSmell       :: !Double   -- ^ Hours to fix per code smell
  , dcHoursPerSecurityIssue   :: !Double   -- ^ Hours to fix per security issue
  , dcInterestRate            :: !Double   -- ^ Annual interest rate on debt
  } deriving (Show, Eq)

-- | Default debt estimation configuration
defaultDebtConfig :: DebtConfig
defaultDebtConfig = DebtConfig
  { dcHoursPerComplexityPoint = 0.5
  , dcHoursPerDuplication = 2.0
  , dcHoursPerCodeSmell = 1.0
  , dcHoursPerSecurityIssue = 4.0
  , dcInterestRate = 0.15          -- 15% annual "interest"
  }

-- | Types of technical debt
data DebtType
  = DesignDebt       -- ^ Architectural issues
  | CodeDebt         -- ^ Code smells, complexity
  | TestDebt         -- ^ Missing or inadequate tests
  | DocDebt          -- ^ Missing documentation
  | InfraDebt        -- ^ Build/deploy issues
  | SecurityDebt     -- ^ Security vulnerabilities
  deriving (Show, Eq)

-- | Debt indicator from code analysis
data DebtIndicator = DebtIndicator
  { diType        :: !DebtType
  , diLocation    :: !CodeLocation
  , diSeverity    :: !Double         -- ^ 1-10 severity
  , diDescription :: !Text
  , diEffort      :: !Double         -- ^ Estimated hours to fix
  } deriving (Show, Eq)

-- | Code analysis results for debt estimation
data CodeAnalysisForDebt = CodeAnalysisForDebt
  { cadComplexityScore  :: !Int          -- ^ Total cyclomatic complexity
  , cadDuplicateBlocks  :: !Int          -- ^ Number of duplicate code blocks
  , cadCodeSmells       :: ![DebtIndicator]
  , cadSecurityIssues   :: ![DebtIndicator]
  , cadMissingTests     :: ![Text]       -- ^ Untested public functions
  , cadMissingDocs      :: ![Text]       -- ^ Undocumented public items
  , cadTotalLines       :: !Int          -- ^ Total lines of code
  } deriving (Show, Eq)

-- | Analyze technical debt
analyzeDebt :: DebtConfig -> CodeAnalysisForDebt -> DebtEstimate
analyzeDebt config analysis = DebtEstimate
  { debtPrincipal = principal
  , debtInterest = interest
  , debtRatio = ratio
  , debtItems = items
  }
  where
    principal = estimateDebtPrincipal config analysis
    interest = estimateDebtInterest config principal
    ratio = calculateDebtRatio config analysis principal

    items = map toDebtItem (cadCodeSmells analysis ++ cadSecurityIssues analysis)

    toDebtItem di = DebtItem
      { diLocation = diLocation di
      , diType = T.pack $ show $ diType di
      , diSeverity = diSeverity di
      , diDescription = diDescription di
      }

-- | Estimate debt principal (total hours to fix all debt)
estimateDebtPrincipal :: DebtConfig -> CodeAnalysisForDebt -> Double
estimateDebtPrincipal config analysis =
  complexityDebt + duplicationDebt + smellDebt + securityDebt + testDebt + docDebt
  where
    -- Complexity debt: excess complexity over threshold
    excessComplexity = max 0 $ cadComplexityScore analysis - 100
    complexityDebt = fromIntegral excessComplexity * dcHoursPerComplexityPoint config

    -- Duplication debt
    duplicationDebt = fromIntegral (cadDuplicateBlocks analysis) * dcHoursPerDuplication config

    -- Code smell debt
    smellDebt = sum $ map diEffort $ cadCodeSmells analysis

    -- Security debt
    securityDebt = sum $ map diEffort $ cadSecurityIssues analysis

    -- Test debt (assume 2 hours per missing test)
    testDebt = fromIntegral (length $ cadMissingTests analysis) * 2.0

    -- Documentation debt (assume 0.5 hours per missing doc)
    docDebt = fromIntegral (length $ cadMissingDocs analysis) * 0.5

-- | Estimate debt interest (ongoing cost of not fixing debt)
estimateDebtInterest :: DebtConfig -> Double -> Double
estimateDebtInterest config principal =
  principal * dcInterestRate config

-- | Calculate debt ratio (debt / development effort)
calculateDebtRatio :: DebtConfig -> CodeAnalysisForDebt -> Double -> Double
calculateDebtRatio _config analysis principal
  | developmentEffort == 0 = 0
  | otherwise = principal / developmentEffort
  where
    -- Estimate development effort from lines of code
    -- Assume 10 LOC per hour average productivity
    developmentEffort = fromIntegral (cadTotalLines analysis) / 10

-- | Categorize debt by severity
categorizeDebt :: [DebtIndicator] -> [(Text, [DebtIndicator])]
categorizeDebt items =
  [ ("Critical (>8)", filter (\i -> diSeverity i > 8) items)
  , ("High (6-8)", filter (\i -> diSeverity i > 6 && diSeverity i <= 8) items)
  , ("Medium (4-6)", filter (\i -> diSeverity i > 4 && diSeverity i <= 6) items)
  , ("Low (<=4)", filter (\i -> diSeverity i <= 4) items)
  ]

-- | Suggest debt remediation priorities
suggestDebtRemediation :: DebtConfig -> CodeAnalysisForDebt -> [Text]
suggestDebtRemediation config analysis = concat
  [ securityFirst
  , highRoi
  , quickWins
  ]
  where
    securityFirst =
      [ "PRIORITY: Fix security issues first"
      | not $ null $ cadSecurityIssues analysis
      ]

    -- High ROI items (high severity, low effort)
    highRoiItems = filter isHighRoi (cadCodeSmells analysis)
    isHighRoi di = diSeverity di > 6 && diEffort di < 2

    highRoi =
      [ T.concat ["High ROI fix: ", diDescription item]
      | item <- take 5 highRoiItems
      ]

    -- Quick wins (< 1 hour)
    quickWinItems = filter (\di -> diEffort di < 1) (cadCodeSmells analysis)
    quickWins =
      [ T.concat ["Quick win: ", diDescription item]
      | item <- take 3 quickWinItems
      ]

{-# LANGUAGE OverloadedStrings #-}

-- | Resource utilization analysis
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Analyzes memory, CPU, and I/O efficiency patterns in code.
module Eco.Resource
  ( analyzeResourceUsage
  , estimateMemoryEfficiency
  , estimateCPUEfficiency
  , estimateIOEfficiency
  , ResourceConfig(..)
  , defaultResourceConfig
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for resource analysis
data ResourceConfig = ResourceConfig
  { rcMaxAllocationsPerFunction :: !Int    -- ^ Threshold for allocation warnings
  , rcMaxCopyOperations         :: !Int    -- ^ Threshold for copy warnings
  , rcMaxFileHandles            :: !Int    -- ^ Max concurrent file handles
  , rcPoolingThreshold          :: !Int    -- ^ When to suggest connection pooling
  } deriving (Show, Eq)

-- | Default resource configuration
defaultResourceConfig :: ResourceConfig
defaultResourceConfig = ResourceConfig
  { rcMaxAllocationsPerFunction = 50
  , rcMaxCopyOperations = 20
  , rcMaxFileHandles = 100
  , rcPoolingThreshold = 5
  }

-- | Resource usage data from static analysis
data ResourceAnalysis = ResourceAnalysis
  { raAllocations     :: ![AllocationInfo]
  , raCopyOperations  :: ![CopyInfo]
  , raFileOperations  :: ![FileOpInfo]
  , raNetworkCalls    :: ![NetworkInfo]
  } deriving (Show, Eq)

data AllocationInfo = AllocationInfo
  { allocLocation :: !CodeLocation
  , allocSize     :: !(Maybe Int)      -- ^ Estimated size in bytes
  , allocType     :: !Text             -- ^ heap, stack, pool
  , allocFreed    :: !Bool             -- ^ Is it properly freed?
  } deriving (Show, Eq)

data CopyInfo = CopyInfo
  { copyLocation  :: !CodeLocation
  , copySize      :: !(Maybe Int)
  , copyAvoidable :: !Bool             -- ^ Could use reference instead?
  } deriving (Show, Eq)

data FileOpInfo = FileOpInfo
  { fileLocation :: !CodeLocation
  , fileOpType   :: !Text              -- ^ read, write, open, close
  , fileClosed   :: !Bool              -- ^ Is handle properly closed?
  } deriving (Show, Eq)

data NetworkInfo = NetworkInfo
  { netLocation   :: !CodeLocation
  , netPooled     :: !Bool             -- ^ Uses connection pooling?
  , netKeptAlive  :: !Bool             -- ^ Uses keep-alive?
  } deriving (Show, Eq)

-- | Analyze code for resource efficiency
analyzeResourceUsage :: ResourceConfig -> ResourceAnalysis -> ResourceScore
analyzeResourceUsage config analysis = ResourceScore
  { memoryEfficiency = estimateMemoryEfficiency config analysis
  , cpuEfficiency = estimateCPUEfficiency config analysis
  , ioEfficiency = estimateIOEfficiency config analysis
  , resourceNormalized = overallScore
  }
  where
    mem = estimateMemoryEfficiency config analysis
    cpu = estimateCPUEfficiency config analysis
    io = estimateIOEfficiency config analysis
    overallScore = (mem + cpu + io) / 3

-- | Estimate memory efficiency (0-100)
estimateMemoryEfficiency :: ResourceConfig -> ResourceAnalysis -> Double
estimateMemoryEfficiency config analysis = max 0 $ 100 - penalties
  where
    allocs = raAllocations analysis

    -- Penalty for excessive allocations
    allocPenalty = if length allocs > rcMaxAllocationsPerFunction config
                   then fromIntegral (length allocs - rcMaxAllocationsPerFunction config)
                   else 0

    -- Penalty for unfreed allocations (memory leaks)
    leakPenalty = fromIntegral $ length $ filter (not . allocFreed) allocs

    -- Penalty for avoidable copies
    copyPenalty = fromIntegral $ length $ filter copyAvoidable $ raCopyOperations analysis

    penalties = allocPenalty * 0.5 + leakPenalty * 10 + copyPenalty * 2

-- | Estimate CPU efficiency (0-100)
estimateCPUEfficiency :: ResourceConfig -> ResourceAnalysis -> Double
estimateCPUEfficiency config analysis = max 0 $ 100 - penalties
  where
    -- Excessive copies waste CPU
    copyCount = length $ raCopyOperations analysis
    copyPenalty = if copyCount > rcMaxCopyOperations config
                  then fromIntegral (copyCount - rcMaxCopyOperations config) * 2
                  else 0

    -- Large allocations cause memory pressure
    largeAllocs = filter (maybe False (> 1024 * 1024) . allocSize) $ raAllocations analysis
    allocPenalty = fromIntegral (length largeAllocs) * 5

    penalties = copyPenalty + allocPenalty

-- | Estimate I/O efficiency (0-100)
estimateIOEfficiency :: ResourceConfig -> ResourceAnalysis -> Double
estimateIOEfficiency config analysis = max 0 $ 100 - penalties
  where
    fileOps = raFileOperations analysis
    netOps = raNetworkCalls analysis

    -- Penalty for unclosed file handles
    unclosedPenalty = fromIntegral $ length $ filter (not . fileClosed) fileOps

    -- Penalty for non-pooled connections
    unpooledConnections = filter (not . netPooled) netOps
    poolPenalty = if length unpooledConnections > rcPoolingThreshold config
                  then fromIntegral (length unpooledConnections) * 5
                  else 0

    -- Penalty for connections without keep-alive
    noKeepalivePenalty = fromIntegral $ length $ filter (not . netKeptAlive) netOps

    penalties = unclosedPenalty * 10 + poolPenalty + noKeepalivePenalty * 2

-- | Analyze resource issues and generate suggestions
suggestResourceImprovements :: ResourceConfig -> ResourceAnalysis -> [Text]
suggestResourceImprovements config analysis = concat
  [ memoryIssues
  , ioIssues
  , networkIssues
  ]
  where
    memoryIssues =
      [ "Consider using object pooling to reduce allocations"
      | length (raAllocations analysis) > rcMaxAllocationsPerFunction config
      ] ++
      [ "Memory leak detected - ensure all allocations are freed"
      | any (not . allocFreed) (raAllocations analysis)
      ] ++
      [ "Avoid unnecessary copies - use references where possible"
      | any copyAvoidable (raCopyOperations analysis)
      ]

    ioIssues =
      [ "File handle leak detected - ensure all handles are closed"
      | any (not . fileClosed) (raFileOperations analysis)
      ]

    networkIssues =
      [ "Consider using connection pooling for better performance"
      | length (filter (not . netPooled) (raNetworkCalls analysis)) > rcPoolingThreshold config
      ] ++
      [ "Enable HTTP keep-alive for reduced connection overhead"
      | any (not . netKeptAlive) (raNetworkCalls analysis)
      ]

-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Rules
  ( allRules
  , rulesFor
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashSet as HashSet

import Duckling.Dimensions
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Rules.Common as CommonRules
import qualified Duckling.Rules.EN as ENRules
import qualified Duckling.Rules.ID as IDRules

-- | Returns the minimal set of rules required for `targets`.
rulesFor :: Locale -> HashSet (Seal Dimension) -> [Rule]
rulesFor locale targets
  | HashSet.null targets = allRules locale
  | otherwise = [ rules | dims <- HashSet.toList $ explicitDimensions targets
                        , rules <- rulesFor' locale dims ]

-- | Returns all the rules for the provided locale.
-- We can't really use `allDimensions` as-is, since `TimeGrain` is not present.
allRules :: Locale -> [Rule]
allRules locale@(Locale lang _) = concatMap (rulesFor' locale) . HashSet.toList
  . explicitDimensions . HashSet.fromList $ allDimensions lang

rulesFor' :: Locale -> Seal Dimension -> [Rule]
rulesFor' (Locale lang (Just region)) dim =
  CommonRules.rules dim ++ langRules lang dim ++ localeRules lang region dim
rulesFor' (Locale lang Nothing) dim =
  CommonRules.rules dim ++ defaultRules lang dim

-- | Default rules when no locale, for backward compatibility.
defaultRules :: Lang -> Seal Dimension -> [Rule]
defaultRules EN = ENRules.defaultRules
defaultRules ID = IDRules.defaultRules
defaultRules _ = const []  -- Unsupported languages return empty rules

localeRules :: Lang -> Region -> Seal Dimension -> [Rule]
localeRules EN = ENRules.localeRules
localeRules ID = IDRules.localeRules
localeRules _ = const (const [])  -- Unsupported languages return empty rules

langRules :: Lang -> Seal Dimension -> [Rule]
langRules EN = ENRules.langRules
langRules ID = IDRules.langRules
langRules _ = const []  -- Unsupported languages return empty rules

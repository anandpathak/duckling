-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Dimensions
  ( allDimensions
  , explicitDimensions
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashSet as HashSet

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Dimensions.Common as CommonDimensions
import qualified Duckling.Dimensions.EN as ENDimensions
import qualified Duckling.Dimensions.ID as IDDimensions


allDimensions :: Lang -> [Seal Dimension]
allDimensions lang = CommonDimensions.allDimensions ++ langDimensions lang

-- | Augments `targets` with all dependent dimensions.
explicitDimensions :: HashSet (Seal Dimension) -> HashSet (Seal Dimension)
explicitDimensions targets = HashSet.union targets deps
  where
    deps = HashSet.unions . map dependents $ HashSet.toList targets

-- | Ordinal depends on Numeral for JA, KO, and ZH.
dependents :: Seal Dimension -> HashSet (Seal Dimension)
dependents (Seal CreditCardNumber) = HashSet.empty
dependents (Seal Distance) = HashSet.singleton (Seal Numeral)
dependents (Seal Duration) = HashSet.fromList [Seal Numeral, Seal TimeGrain]
dependents (Seal Numeral) = HashSet.empty
dependents (Seal Email) = HashSet.empty
dependents (Seal AmountOfMoney) = HashSet.singleton (Seal Numeral)
dependents (Seal Ordinal) = HashSet.singleton (Seal Numeral)
dependents (Seal PhoneNumber) = HashSet.empty
dependents (Seal Quantity) = HashSet.singleton (Seal Numeral)
dependents (Seal RegexMatch) = HashSet.empty
dependents (Seal Temperature) = HashSet.singleton (Seal Numeral)
dependents (Seal Time) =
  HashSet.fromList [Seal Numeral, Seal Duration, Seal Ordinal, Seal TimeGrain]
dependents (Seal TimeGrain) = HashSet.empty
dependents (Seal Url) = HashSet.empty
dependents (Seal Volume) = HashSet.singleton (Seal Numeral)
dependents (Seal (CustomDimension dim)) = dimDependents dim

langDimensions :: Lang -> [Seal Dimension]
langDimensions EN = ENDimensions.allDimensions
langDimensions ID = IDDimensions.allDimensions
langDimensions _ = []  -- Unsupported languages return empty list

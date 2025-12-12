-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.ID.Rules
  ( rules
  ) where

import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import qualified Duckling.TimeGrain.Types as TG

grains :: [(Text, String, TG.Grain)]
grains = [ ("detik (grain) ", "detik?",      TG.Second)
         , ("menit (grain)" , "menit?",   TG.Minute)
         , ("jam (grain)"   , "jam?", TG.Hour)
         , ("hari (grain)"    , "hari?",            TG.Day)
         , ("minggu (grain)"   , "minggu(?!\\s+(lalu|depan|kemaren|kemarin|kemudian))",           TG.Week)
         , ("bulan (grain)"  , "bulan?",          TG.Month)
         , ("kuarter (grain)", "(kuarter|qtr)s?",  TG.Quarter)
         , ("tahun (grain)"   , "tahun?",        TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }

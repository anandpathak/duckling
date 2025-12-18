-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Duckling.Time.ID.Tests
  ( tests
  ) where

import Data.Aeson
import Data.Aeson.Types ((.:), parseMaybe, withObject)
import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.ID.Corpus
import Duckling.TimeGrain.Types (Grain(..))
import Duckling.Types (Range(..))

tests :: TestTree
tests = testGroup "ID Tests"
  [ makeCorpusTest [Seal Time] corpus
  , makeNegativeCorpusTest [Seal Time] negativeCorpus
  , dateRangeTests
  , timeExpressionTests
  , dateTimeCombinationTests
  , relativeDatesWithNumbersTests
  , specificDateTests
  , intervalExpressionTests
  , durationExpressionTests
  , periodExpressionTests
  ]

-- Test date ranges including the new "hari terakhir" rules
dateRangeTests :: TestTree
dateRangeTests = testCase "Date Range Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetimeInterval ((2013, 2, 5, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
                 [ "7 hari terakhir"
                 , "dalam 7 hari terakhir"
                 ]
      , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
                 [ "2 hari terakhir"
                 , "dalam 2 hari terakhir"
                 ]
      , examples (datetimeInterval ((2013, 1, 29, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
                 [ "14 hari terakhir"
                 , "dalam 14 hari terakhir"
                 ]
      ]

-- Test time expressions (hours and minutes)
timeExpressionTests :: TestTree
timeExpressionTests = testCase "Time Expression Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetime (2013, 2, 12, 14, 30, 0) Minute)
                 [ "pukul 14:30"
                 , "pukul 14.30"
                 , "jam 14:30"
                 ]
      , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
                 [ "jam 14"
                 , "pukul 14"
                 ]
      , examples (datetime (2013, 2, 12, 2, 0, 0) Hour)
                 [ "jam 2 pagi"
                 , "pukul 2 pagi"
                 ]
      , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
                 [ "jam 2 sore"
                 , "pukul 2 sore"
                 ]
      , examples (datetime (2013, 2, 12, 22, 0, 0) Hour)
                 [ "jam 10 malam"
                 , "pukul 10 malam"
                 ]
      , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
                 [ "pukul 3 siang"
                 ]
      ]

-- Test date + time combinations
dateTimeCombinationTests :: TestTree
dateTimeCombinationTests = testCase "Date + Time Combination Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetime (2013, 2, 13, 6, 0, 0) Hour)
                 [ "besok pagi"
                 ]
      , examples (datetime (2013, 2, 13, 12, 0, 0) Hour)
                 [ "besok siang"
                 ]
      , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
                 [ "besok sore"
                 ]
      , examples (datetime (2013, 2, 13, 18, 0, 0) Hour)
                 [ "besok malam"
                 ]
      , examples (datetime (2013, 2, 11, 6, 0, 0) Hour)
                 [ "kemarin pagi"
                 ]
      , examples (datetime (2013, 2, 11, 18, 0, 0) Hour)
                 [ "kemarin malam"
                 , "tadi malam"
                 ]
      , examples (datetime (2013, 2, 12, 3, 0, 0) Hour)
                 [ "hari ini jam 3"
                 ]
      , examples (datetime (2013, 2, 13, 14, 30, 0) Minute)
                 [ "besok jam 14:30"
                 ]
      , examples (datetime (2013, 2, 11, 10, 0, 0) Hour)
                 [ "kemarin pukul 10 pagi"
                 ]
      , examples (datetime (2013, 12, 13, 15, 0, 0) Hour)
                 [ "13 desember jam 15:00"
                 ]
      , examples (datetime (2013, 2, 13, 14, 0, 0) Hour)
                 [ "besok sore jam 2"
                 ]
      ]

-- Test relative dates with numbers
relativeDatesWithNumbersTests :: TestTree
relativeDatesWithNumbersTests = testCase "Relative Dates with Numbers Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetime (2013, 2, 14, 0, 0, 0) Day)
                 [ "2 hari lagi"
                 ]
      , examples (datetime (2013, 2, 9, 0, 0, 0) Day)
                 [ "3 hari lalu"
                 ]
      , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
                 [ "7 hari lalu"
                 ]
      , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
                 [ "2 minggu lalu"
                 ]
      , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
                 [ "3 minggu lalu"
                 ]
      , examples (datetime (2013, 1, 12, 0, 0, 0) Day)
                 [ "1 bulan lalu"
                 ]
      , examples (datetime (2012, 12, 12, 0, 0, 0) Day)
                 [ "2 bulan lalu"
                 ]
      ]

-- Test specific date formats
specificDateTests :: TestTree
specificDateTests = testCase "Specific Date Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetime (2013, 12, 13, 0, 0, 0) Day)
                 [ "13 desember"
                 ]
      , examples (datetime (2025, 2, 14, 0, 0, 0) Day)
                 [ "14 februari 2025"
                 ]
      , examples (datetime (2024, 1, 1, 0, 0, 0) Day)
                 [ "1 jan 2024"
                 ]
      , examples (datetime (2025, 12, 25, 0, 0, 0) Day)
                 [ "25 desember 2025"
                 ]
      , examples (datetime (2025, 11, 12, 0, 0, 0) Day)
                 [ "2025-11-12"
                 , "20251112"
                 ]
      , examples (datetime (2024, 12, 25, 0, 0, 0) Day)
                 [ "25/12/2024"
                 ]
      , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
                 [ "25/12"
                 ]
      , examples (datetime (2025, 1, 12, 0, 0, 0) Day)
                 [ "12 Jan 2025"
                 ]
      , examples (datetime (2025, 1, 1, 0, 0, 0) Day)
                 [ "Jan 2025"
                 ]
      , examples (datetime (2025, 1, 15, 0, 0, 0) Day)
                 [ "15/01/2025"
                 , "15-01-2025"
                 , "15.01.2025"
                 ]
      ]

-- Test interval expressions like "dari X sampai Y"
intervalExpressionTests :: TestTree
intervalExpressionTests = testCase "Interval Expression Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Day)
                 [ "dari 1 januari sampai 31 januari"
                 , "dari 1 jan sampai 31 jan"
                 ]
      , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 1, 16, 0, 0, 0)) Day)
                 [ "dari 1 jan sampai 15 jan"
                 ]
      , examples (datetimeInterval ((2013, 12, 1, 0, 0, 0), (2013, 12, 8, 0, 0, 0)) Day)
                 [ "1 sampai 7 desember"
                 ]
      ]

-- Test duration expressions
durationExpressionTests :: TestTree
durationExpressionTests = testCase "Duration Expression Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetime (2013, 2, 12, 16, 30, 0) Minute)
                 [ "dalam 2 jam"
                 ]
      , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
                 [ "dalam 30 menit"
                 ]
      , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
                 [ "dalam 1 hari"
                 ]
      , examples (datetime (2013, 3, 5, 4, 30, 0) Second)
                 [ "dalam 3 minggu"
                 ]
      , examples (datetime (2013, 2, 12, 6, 30, 0) Minute)
                 [ "2 jam kemudian"
                 ]
      , examples (datetime (2013, 2, 12, 15, 0, 0) Minute)
                 [ "30 menit kemudian"
                 ]
      , examples (datetime (2013, 2, 13, 4, 30, 0) Second)
                 [ "1 hari kemudian"
                 ]
      , examples (datetime (2013, 2, 12, 2, 30, 0) Minute)
                 [ "2 jam yang lalu"
                 ]
      , examples (datetime (2013, 2, 12, 14, 0, 0) Minute)
                 [ "30 menit yang lalu"
                 ]
      , examples (datetime (2013, 2, 11, 4, 30, 0) Second)
                 [ "1 hari yang lalu"
                 ]
      ]

-- Test period expressions like "minggu ini", "bulan ini", "akhir minggu", "awal bulan"
periodExpressionTests :: TestTree
periodExpressionTests = testCase "Period Expression Tests" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = concat
      [ examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
                 [ "minggu ini"
                 ]
      , examples (datetimeInterval ((2013, 2, 1, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
                 [ "bulan ini"
                 ]
      , examples (datetimeInterval ((2013, 2, 1, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
                 [ "awal bulan"
                 ]
      -- Note: "akhir minggu" (weekend) returns an Open interval from Friday 18:00 to Monday 00:00
      -- This is tested via the corpus which includes weekend examples
      ]

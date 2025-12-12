-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Ranking.Classifiers
  ( classifiers
  ) where

import Data.Maybe
import qualified Data.HashMap.Strict as HashMap

import Duckling.Locale
import Duckling.Ranking.Types
import qualified Duckling.Ranking.Classifiers.EN_GB as EN_GBClassifiers
import qualified Duckling.Ranking.Classifiers.EN_US as EN_USClassifiers
import qualified Duckling.Ranking.Classifiers.EN_XX as EN_XXClassifiers
import qualified Duckling.Ranking.Classifiers.ID_XX as ID_XXClassifiers

classifiers :: Locale -> Classifiers
classifiers (Locale EN (Just GB)) = EN_GBClassifiers.classifiers
classifiers (Locale EN (Just US)) = EN_USClassifiers.classifiers
classifiers (Locale EN _) = EN_XXClassifiers.classifiers
classifiers (Locale ID _) = ID_XXClassifiers.classifiers
classifiers _ = HashMap.empty  -- Unsupported languages return empty classifiers

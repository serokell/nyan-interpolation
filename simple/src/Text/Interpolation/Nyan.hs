-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{- | Flexible interpolation quoter.

This works exactly like the quoter in `nyan-interpolation` library, with the
only exception that arbitrary expressions cannot be used in placeholder position,
only variables.

>>> let val1 = 1 :: Int
>>>     val2 = 2
>>> in [int||Values are #{val1} and #d{val2}]
"Value is 5"

-}
module Text.Interpolation.Nyan
  ( int
  , module RModes
  ) where

import Text.Interpolation.Nyan.Core
import Text.Interpolation.Nyan.RModes.Buildable as RModes
import Text.Interpolation.Nyan.RModes.CommonExtra as RModes

int :: QuasiQuoter
int = mkInt defaultInterpolatorOptions
  { defaultSwitchesOptions = recommendedDefaultSwitchesOptions
  }

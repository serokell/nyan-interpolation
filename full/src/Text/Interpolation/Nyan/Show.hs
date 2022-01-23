{- | Exports the default interpolator + Show-oriented rendering modes

>>> data Ty = Ty { val :: Int }
>>>    deriving stock Show
>>>
>>> [int||Value is #{Ty 5}]
"Value is Ty {val = 5}"

-}
module Text.Interpolation.Nyan.Show
  ( int
  , module RModes
  ) where

import Text.Interpolation.Nyan (int)
import Text.Interpolation.Nyan.RModes.CommonExtra as RModes
import Text.Interpolation.Nyan.RModes.Show as RModes

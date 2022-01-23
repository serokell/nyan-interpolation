-- | Set of basic rendering modes, where 'Show' is treated as
-- the primary way to render data.
module Text.Interpolation.Nyan.RModes.Show where

import Fmt (build)

import Text.Interpolation.Nyan.Core

-- | Default render mode.
rmode' :: Show a => RMode a
rmode' = RMode $ build . show

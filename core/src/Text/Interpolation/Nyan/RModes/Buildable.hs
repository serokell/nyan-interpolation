-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Set of basic rendering modes, where 'Buildable' is treated as
-- the primary way to render data.
module Text.Interpolation.Nyan.RModes.Buildable where

import Fmt (Buildable (..), Builder)

import Text.Interpolation.Nyan.Core

-- | Default render mode.
rmode' :: Buildable a => RMode a
rmode' = RMode build

-- | Render via 'Show'.
rmode's :: Show a => RMode a
rmode's = RMode $ build . show

-- | Rendering mode for 'Builder'.
rmode'b :: RMode Builder
rmode'b = RMode id

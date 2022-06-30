-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{- | Core of the @nyan-interpolation@ library.

Use it to define your own variation of the interpolator,
customizing the default switches and other parameters to your preferences.

@
int :: QuasiQuoter
int = mkInt defaultInterpolatorOptions
  { defaultSwitchesOptions = recommendedDefaultSwitchesOptions
    { defSpacesTrimming = Just True
    }
  }
@

-}
module Text.Interpolation.Nyan.Core
  ( -- * Interpolator
    mkInt
    -- * Interpolator options
  , InterpolatorOptions
  , defaultInterpolatorOptions
    -- ** Field accessors for interpolator options
  , defaultSwitchesOptions
  , valueInterpolator
  , invisibleCharsPreview
    -- * Default switches options
  , DefaultSwitchesOptions
  , basicDefaultSwitchesOptions
  , recommendedDefaultSwitchesOptions
    -- ** Field accessors for default switches options
  , defSpacesTrimming
  , defCommenting
  , defIndentationStripping
  , defLeadingNewlineStripping
  , defTrailingSpacesStripping
  , defReducedNewlines
  , defReturnType
  , defMonadic

    -- * Value interpolators
  , ValueInterpolator (..)
  , simpleValueInterpolator
  , tickedValueInterpolator

    -- * Adjusting preview
  , InvisibleCharsPreview (..)
  , simpleInvisibleCharsPreview

    -- * Rendering modes
  , RMode (..)

    -- * Re-exports
  , TH.QuasiQuoter
  ) where

import qualified Data.Text as T
import qualified Language.Haskell.TH.Quote as TH

import Text.Interpolation.Nyan.Core.Internal.Base
import Text.Interpolation.Nyan.Core.Internal.Parser
import Text.Interpolation.Nyan.Core.Internal.Processor
import Text.Interpolation.Nyan.Core.Internal.RMode
import Text.Interpolation.Nyan.Core.Internal.Splice

-- | Construct an interpolator.
--
-- Usually you pass some options here that you consider canonical and use
-- the resulting interolator throughout your project.
mkInt :: InterpolatorOptions -> TH.QuasiQuoter
mkInt iopts = TH.QuasiQuoter
  { TH.quoteExp = \s -> do
      (sopts, sint) <-
        either fail pure $
        parseIntString (defaultSwitchesOptions iopts) (T.pack s)
      let sint' = processIntString sopts sint
      intSplice iopts (sopts, sint')
  , TH.quotePat = \_ ->
      fail "Cannot interpolate at pattern position"
  , TH.quoteType = \_ ->
      fail "Cannot interpolate at type position"
  , TH.quoteDec = \_ ->
      fail "Cannot interpolate at declaration position"
  }

-- | The most interpolator options.
--
-- * Tries to keep the text as much unchanged as possible.
-- * Interpolates only variables.
defaultInterpolatorOptions :: InterpolatorOptions
defaultInterpolatorOptions = InterpolatorOptions
  { defaultSwitchesOptions = basicDefaultSwitchesOptions
  , valueInterpolator = simpleValueInterpolator
  , invisibleCharsPreview = simpleInvisibleCharsPreview
  }

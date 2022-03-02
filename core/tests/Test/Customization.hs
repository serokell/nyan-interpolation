-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Customization () where

import Text.Interpolation.Nyan.Core
import qualified Text.Interpolation.Nyan.Core.Internal.Base as Internal

-- | We have to make sure that all available options can be tuned, i.e.
-- all the record field selectors are exported (while the constructor is not).
_AllFieldsAreExported :: QuasiQuoter
_AllFieldsAreExported =
  let _ioptsExhaustive :: InterpolatorOptions = Internal.InterpolatorOptions
        (error "")
        (error "")
        (error "")
        -- ↑ if you change this, also add a field to the record below
  in mkInt defaultInterpolatorOptions
    { valueInterpolator = ValueInterpolator $ error ""
    , invisibleCharsPreview = InvisibleCharsPreview $ error ""
    , defaultSwitchesOptions =
      let _dsopts :: DefaultSwitchesOptions = Internal.DefaultSwitchesOptions
            (error "")
            (error "")
            (error "")
            (error "")
            (error "")
            (error "")
            (error "")
            -- ↑ if you change this, also add a field to the record below
      in basicDefaultSwitchesOptions
        { defIndentationStripping = Nothing
        , defSpacesTrimming = Nothing
        , defLeadingNewlineStripping = Nothing
        , defTrailingSpacesStripping = Nothing
        , defReducedNewlines = Nothing
        , defReturnType = Nothing
        , defMonadic = Nothing
        }
    }

-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Exports basic lens for config-related types.
module Text.Interpolation.Nyan.Core.Internal.Base.Lens where

import Text.Interpolation.Nyan.Core.Internal.Base
import Text.Interpolation.Nyan.Lens

---------------------------------------------------------------------
-- Basic lens for 'SwitchesOptions'
---------------------------------------------------------------------

spacesTrimmingL, indentationStrippingL, leadingNewlineStrippingL,
  trailingSpacesStrippingL, reducedNewlinesL, monadicL :: Lens' SwitchesOptions Bool

spacesTrimmingL = lens spacesTrimming (\s a -> s { spacesTrimming = a })
indentationStrippingL = lens indentationStripping (\s a -> s { indentationStripping = a })
leadingNewlineStrippingL = lens leadingNewlineStripping (\s a -> s { leadingNewlineStripping = a })
trailingSpacesStrippingL = lens trailingSpacesStripping (\s a -> s { trailingSpacesStripping = a })
reducedNewlinesL = lens reducedNewlines (\s a -> s { reducedNewlines = a })
monadicL = lens monadic (\s a -> s { monadic = a })

returnTypeL :: Lens' SwitchesOptions ReturnType
returnTypeL = lens returnType (\s a -> s { returnType = a })

previewLevelL :: Lens' SwitchesOptions PreviewLevel
previewLevelL = lens previewLevel (\s a -> s { previewLevel = a })

---------------------------------------------------------------------
-- Basic lens for 'DefaultSwitchesOptions'
---------------------------------------------------------------------

defSpacesTrimmingL, defIndentationStrippingL, defLeadingNewlineStrippingL,
  defTrailingSpacesStrippingL, defReducedNewlinesL, defMonadicL
    :: Lens' DefaultSwitchesOptions (Maybe Bool)

defSpacesTrimmingL = lens defSpacesTrimming (\s a -> s { defSpacesTrimming = a })
defIndentationStrippingL = lens defIndentationStripping (\s a -> s { defIndentationStripping = a })
defLeadingNewlineStrippingL =
  lens defLeadingNewlineStripping (\s a -> s { defLeadingNewlineStripping = a })
defTrailingSpacesStrippingL =
  lens defTrailingSpacesStripping (\s a -> s { defTrailingSpacesStripping = a })
defReducedNewlinesL = lens defReducedNewlines (\s a -> s { defReducedNewlines = a })
defMonadicL = lens defMonadic (\s a -> s { defMonadic = a })

defReturnTypeL :: Lens' DefaultSwitchesOptions (Maybe ReturnType)
defReturnTypeL = lens defReturnType (\s a -> s { defReturnType = a })

---------------------------------------------------------------------
-- Basic lens for 'InterpolatorOptions'
---------------------------------------------------------------------

defaultSwitchesOptionsL :: Lens' InterpolatorOptions DefaultSwitchesOptions
defaultSwitchesOptionsL = lens defaultSwitchesOptions (\s a -> s { defaultSwitchesOptions = a })

valueInterpolatorL :: Lens' InterpolatorOptions ValueInterpolator
valueInterpolatorL = lens valueInterpolator (\s a -> s { valueInterpolator = a })

invisibleCharsPreviewL :: Lens' InterpolatorOptions InvisibleCharsPreview
invisibleCharsPreviewL = lens invisibleCharsPreview (\s a -> s { invisibleCharsPreview = a })

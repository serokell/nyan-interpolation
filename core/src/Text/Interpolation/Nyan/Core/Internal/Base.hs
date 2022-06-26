-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DerivingVia #-}

module Text.Interpolation.Nyan.Core.Internal.Base where

import Data.Monoid (Endo (..))
import Data.Text (Text)
import Language.Haskell.TH (ExpQ)

import Text.Interpolation.Nyan.Lens (makeLenses)

{- $setup

> import Data.Text
> import Data.Text.Lazy
-}


-- * Interpolation data

-- | Information about single piece that is to be interpolated.
data IntData = IntData
  { idMode :: Text
    -- ^ How to use the value in the braces.
    -- This is some text before the brances, usually one letter or nothing.
  , idCode :: Text
    -- ^ The inserted code. This is what appears in braces.
  } deriving stock (Show, Eq)

-- | Piece of interpolation string.
data ParsedIntPiece
  = PipString Text
    -- ^ Mere text.
  | PipNewline Text
    -- ^ Some line feed.
    -- This must be preferred over 'PipString'.
  | PipLeadingWs Word
    -- ^ Whitespaces at the beginning of the line.
    -- This must be preferred over 'PipString'.
  | PipEmptyLine
    -- ^ Line without any text. The line feed is not included here.
    -- This must be preferred over 'PipLeadingWs'.
  | PipInt IntData
    -- ^ Interpolator piece.
  deriving stock (Show, Eq)

type ParsedInterpolatedString = [ParsedIntPiece]

-- | Finalized piece of interpolation string.
data IntPiece
  = IpString Text
    -- ^ Mere text.
  | IpInt IntData
    -- ^ Interpolator piece.
  deriving stock (Show, Eq)

type InterpolatedString = [IntPiece]

-- * Switches

-- | Return type of the interpolator.
data ReturnType
  = AnyFromBuilder
    -- ^ @FromBuilder a => a@
  | ConcreteText
    -- ^ 'Data.Text.Text'
  | ConcreteLText
    -- ^ 'Data.Text.Lazy.Text'
  | ConcreteBuilder
    -- ^ 'Data.Text.Lazy.Builder.Builder'
  deriving stock (Show, Eq)

-- | Requested preview level.
data PreviewLevel
  = PreviewNone
    -- ^ Do nothing special.
  | PreviewExact
    -- ^ Print the resulting text as-is (without substitutions).
  | PreviewInvisible
    -- ^ Print the text, replacing invisible characters with special symbols.
  deriving stock (Show, Eq, Enum, Bounded)

-- | All switches options.
data SwitchesOptions = SwitchesOptions
  { spacesTrimming          :: Bool
  , indentationStripping    :: Bool
  , leadingNewlineStripping :: Bool
  , trailingSpacesStripping :: Bool
  , returnType              :: ReturnType
  , reducedNewlines         :: Bool
  , monadic                 :: Bool
  , previewLevel            :: PreviewLevel
  } deriving stock (Show, Eq)

makeLenses ''SwitchesOptions

-- | Default switches options set in the interpolator, those that are used
-- in @[int||...|]@.
--
-- When no default value for a switch is specified, this switch is left
-- mandatory for specifying in the interpolator.
data DefaultSwitchesOptions = DefaultSwitchesOptions
  { defSpacesTrimming          :: Maybe Bool
  , defIndentationStripping    :: Maybe Bool
  , defLeadingNewlineStripping :: Maybe Bool
  , defTrailingSpacesStripping :: Maybe Bool
  , defReducedNewlines         :: Maybe Bool
  , defReturnType              :: Maybe ReturnType
  , defMonadic                 :: Maybe Bool
  } deriving stock (Show)

makeLenses ''DefaultSwitchesOptions

-- | Default 'DefaultSwitchesOptions'.
--
-- This set of switches tries to leave the text as much unmodified as possible.
--
-- It does __not__ define default switches used by @Text.Interpolation.Nyan@
-- module, and you will likely want to enable at least some options here.
basicDefaultSwitchesOptions :: DefaultSwitchesOptions
basicDefaultSwitchesOptions = DefaultSwitchesOptions
  { defSpacesTrimming = Just False
  , defIndentationStripping = Just False
  , defLeadingNewlineStripping = Just False
  , defTrailingSpacesStripping = Just False
  , defReturnType = Just AnyFromBuilder
  , defReducedNewlines = Just False
  , defMonadic = Just False
  }

-- | 'DefaultSwitchesOptions' used in the @Text.Interpolation.Nyan@ module
-- in the default interpolator.
recommendedDefaultSwitchesOptions :: DefaultSwitchesOptions
recommendedDefaultSwitchesOptions = DefaultSwitchesOptions
  { defSpacesTrimming = Just False
  , defIndentationStripping = Just True
  , defLeadingNewlineStripping = Just True
  , defTrailingSpacesStripping = Just True
  , defReturnType = Just AnyFromBuilder
  , defReducedNewlines = Just False
  , defMonadic = Just False
  }

-- | How to expand values in @#{}@ into Haskell AST.
newtype ValueInterpolator = ValueInterpolator
  { runValueInterpolator :: Text -> ExpQ
  }

-- | Transformation that describes how to mark the invisible characters.
--
-- Use 'Monoid' instance to sequence multiple such transformations.
newtype InvisibleCharsPreview = InvisibleCharsPreview
  { replaceInvisibleChars :: String -> String
  } deriving Semigroup via (Endo String)
    deriving Monoid via (Endo String)

-- | Options set when creating an interpolator.
data InterpolatorOptions = InterpolatorOptions
  { defaultSwitchesOptions :: DefaultSwitchesOptions
    -- ^ Default switches options.

  , valueInterpolator      :: ValueInterpolator
    -- ^ Expands text in @#{}@ into AST.
    --
    -- We have to leave this changeable because there is no "perfect" expander.
    -- Using the most appropriate one would require relying on @haskell-src-exts@
    -- package which is quite a heavy-weight dependency.
    -- Some users would prefer a simpler solution which would only allow
    -- variables in @#{}@.
    --
    -- Interpreting the passed text in tricky ways is valid.
    -- For instance, for specialized interpolators @#{var}@
    -- could be expanded to @local'var@, @view varLens@, or more complex
    -- Haskell code.

  , invisibleCharsPreview  :: InvisibleCharsPreview
    -- ^ In case, when the switches are set to preview the resulting text
    -- with invisibles being marked specially (@!!@), how to update the pieces
    -- of text.
  }

makeLenses ''InterpolatorOptions

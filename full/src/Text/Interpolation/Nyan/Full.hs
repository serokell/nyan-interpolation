-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE CPP #-}

-- | Contains full-fledged interpolator.
module Text.Interpolation.Nyan.Full
  ( fullHaskellValueInterpolator
  ) where

import Control.Monad (when)
import Data.Char (isSpace)
import qualified Data.Text as T
import Language.Haskell.Exts.Parser (ParseMode (extensions, parseFilename),
                                     ParseResult (ParseFailed, ParseOk), defaultParseMode,
                                     parseExpWithMode)
import Language.Haskell.Meta.Syntax.Translate (toExp)
import Language.Haskell.TH (extsEnabled)

#if MIN_VERSION_haskell_src_meta(0,8,9)
import Data.Maybe (mapMaybe)
import Language.Haskell.Exts.Extension (Extension (..), Language (..))
import Language.Haskell.Exts.Parser (baseLanguage)
import Language.Haskell.Meta.Extensions (fromExtension)
#endif

import Text.Interpolation.Nyan.Core

{- | Interpolates strings containing arbitrary Haskell expressions.

This is used in the interpolator provided by "Text.Interpolation.Nyan" module.

Known issues:

* If @haskell-src-meta@ prior to @0.8.9@ version is used, a default set of
  extensions for the given Haskell dialect (e.g. Haskell2010) is used
  for the interpolated values.
  With the modern version of @haskell-src-meta@, we do our best to be trasparent
  and pick the extensions enabled in the module where interpolator is called
  (some rare extensions may still be unsupported).

-}
fullHaskellValueInterpolator :: ValueInterpolator
fullHaskellValueInterpolator = ValueInterpolator $ \txt -> do
  when (T.all isSpace txt) $
    fail "Empty placeholder"
  enabledExtensions <- extsEnabled
  let parseMode = defaultParseMode
        { parseFilename = "interpolator placeholder"
#if MIN_VERSION_haskell_src_meta(0,8,9)
        , baseLanguage = HaskellAllDisabled
#else
          -- use default language
#endif
        , extensions = providedExtensions enabledExtensions
        }
  case parseExpWithMode parseMode (T.unpack txt) of
    ParseFailed _loc e -> fail e
    ParseOk res        -> return (toExp res)
  where
    providedExtensions =
#if MIN_VERSION_haskell_src_meta(0,8,9)
      map EnableExtension . mapMaybe fromExtension
#else
      -- There is no easy way to do the conversion between template-haskell's
      -- and haskell-src-exts's Extension types, so using only language-default
      -- extensions (e.g. Haskell2010).
      --
      -- It may be tempting to hardcode some common extensions, but we better
      -- not do that, otherwise on @haskell-src-meta@ bump the user's code will
      -- start relying on the extensions enabled in the module and so may break,
      -- which would be extremly bad if occurs in a library.
      --
      -- Meanwhile, using all the language-default extensions seems safe,
      -- they all seem to be supported by relatively recent versions of
      -- @haskell-src-meta@. An issue will fire only if the user disabled
      -- some extension in the module, and this starts affecting the interpolator
      -- after the bump; let's treat that as a non-caught timely bug in the
      -- user's code.
      \_enabledExts -> []
#endif

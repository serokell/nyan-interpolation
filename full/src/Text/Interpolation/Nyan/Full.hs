-- | Contains full-fledged interpolator.
module Text.Interpolation.Nyan.Full
  ( fullHaskellValueInterpolator
  ) where

import Control.Monad (when)
import Data.Char (isSpace)
import qualified Data.Text as T
import Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..))
import Language.Haskell.Exts.Parser (ParseMode (extensions, parseFilename),
                                     ParseResult (ParseFailed, ParseOk), defaultParseMode,
                                     parseExpWithMode)
import Language.Haskell.Meta.Syntax.Translate (toExp)

import Text.Interpolation.Nyan.Core

-- | Interpolates strings containing arbitrary Haskell expressions.
fullHaskellValueInterpolator :: ValueInterpolator
fullHaskellValueInterpolator = ValueInterpolator $ \txt -> do
  when (T.all isSpace txt) $
    fail "Empty placeholder"
  let parseMode = defaultParseMode
        { parseFilename = "interpolator placeholder"
        , extensions = providedExtensions
        }
  case parseExpWithMode parseMode (T.unpack txt) of
    ParseFailed _loc e -> fail e
    ParseOk res        -> return (toExp res)
  where
    providedExtensions = map EnableExtension
      -- To fetch extensions available in the environment we need
      -- https://github.com/haskell-party/haskell-src-meta/issues/33
      -- Let's wait for extensions translator to appear in several LTS revisions
      -- before using it here to avoid extra deps; for now just hardcode all
      -- the ever needed extensions.
      [ PostfixOperators
      , UnicodeSyntax
      , TypeApplications
      ]

-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

-- | Producing TH splices for interpolation.
module Text.Interpolation.Nyan.Core.Internal.Splice where

import Control.Monad (forM, unless, when)
import Data.Char (isSpace)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Fmt (Builder, fmt)
import Language.Haskell.TH

import Text.Interpolation.Nyan.Core.Internal.Base
import Text.Interpolation.Nyan.Core.Internal.RMode

-- | Expression result of which can yet be somehow extended.
type ExtendableRes = (ExpQ -> ExpQ) -> ExpQ

-- | Build interpolated string into TH splice.

-- Note: one of things we aim at is concise produced code, as the user may
-- sometimes need to read what is being generated.
intSplice
  :: InterpolatorOptions
  -> (SwitchesOptions, InterpolatedString)
  -> ExpQ
intSplice iopts (sopts, istr) = do
  invokeDemonstration
  if not (monadic sopts)
  then
    [| $finalConvertFuncQ $ mconcat
        $(ListE <$> forM istr \case
            IpString txt ->
              mkStrLiteralQ txt
            IpInt IntData{..} -> do
              [|$(renderFuncQ idMode)
                $(runValueInterpolator (valueInterpolator iopts) idCode)
                |]
          )
      |]
  else
    [| $finalConvertFuncQ . mconcat <$> sequenceA
        $(ListE <$> forM istr \case
            IpString txt ->
              [|pure $(mkStrLiteralQ txt)|]
            IpInt IntData{..} -> do
              [|$(renderFuncQ idMode) <$>
                $(runValueInterpolator (valueInterpolator iopts) idCode)
                |]
          )
      |]
  where
    -- Contains: render rmode'xxx
    renderFuncQ :: Text -> ExpQ
    renderFuncQ mode =
      return $
      VarE 'renderWithMode
        `AppE`
        VarE (mkName $ "rmode'" <> T.unpack mode)

    -- Contains: fmt @Builder @ret
    finalConvertFuncQ :: ExpQ
    finalConvertFuncQ = return $ case returnType sopts of
      AnyFromBuilder  -> fmtE
      ConcreteText    -> fmtE `AppTypeE` ConT ''Text
      ConcreteLText   -> fmtE `AppTypeE` ConT ''LT.Text
      ConcreteBuilder -> VarE 'id `AppTypeE` ConT ''Builder
      where
        fmtE = VarE 'fmt

    mkStrLiteralQ :: Text -> ExpQ
    mkStrLiteralQ str = do
      haveOverloadedStrings <- isExtEnabled OverloadedStrings
      let fromStringF
            | haveOverloadedStrings = Nothing
            | otherwise = Just (VarE 'fromString)
      return $ maybe id AppE fromStringF (LitE . StringL $ T.unpack str)

    invokeDemonstration :: Q ()
    invokeDemonstration = do
      let msg = case demonstrationLevel sopts of
            DemonstrateNone      -> Nothing
            DemonstrateExact     -> Just $ mconcat
              [ "Interpolated text will look like:\n"
              , flip foldMap istr \case
                  IpString txt -> txt
                  IpInt _      -> "..."
              , "\n"
              ]
            DemonstrateInvisible -> Just $ mconcat
              [ "Interpolated text will look like:\n"
              , let showInvisibles = replaceInvisibleChars (invisibleCharsDemonstration iopts)
                in flip foldMap istr \case
                  IpString txt -> T.pack $ showInvisibles (T.unpack txt)
                  IpInt _      -> "..."
              , "<end>\n"
              ]

      -- We report as an error, not as a warning, because
      -- in normal circumstances the user wants to disable
      -- the demonstration immediately after checking, he/she
      -- probably do not want to build half of the project and
      -- then build it again after disabling the demonstration.
      --
      -- So we want to build the entire module, but do not go further.
      mapM_ (reportError . T.unpack) msg

{- | Interpolates only strings containing single variable.

This allows for @{var}@-like interpolated values, no applications,
operators or other constructions are allowed.

-}
simpleValueInterpolator :: ValueInterpolator
simpleValueInterpolator = ValueInterpolator \txt -> do
  let varNameTxt = T.strip txt
  unless (T.all isAllowedChar varNameTxt) $
    fail "Only passing sole variables are allowed by this interpolator"
  when (T.null varNameTxt) $
    fail "Empty placeholder"
  lookupValueName (T.unpack varNameTxt) >>= \case
    Nothing      -> fail $ "Variable '" <> T.unpack varNameTxt <> "' is not in scope"
    Just varName -> return (VarE varName)
  where
    isAllowedChar c =
      -- handling the most common things to remind the user that only variables
      -- are allowed;
      -- want to rather be too permissive than too restrictive
      not (isSpace c) && c /= '$' && c /= '.'

{- | This is a variation of 'simpleValueInterpolator' that requires all the
referred variables to start from a special @i'@ prefix.

One major issue with 'simpleValueInterpolator' is that, with it the user can
mistakenly pick a value from the wrong scope, for instance, a global value
instead of a local one.
This value interpolator tries to solve the issue by bringing the practice
to call the interpolator like

@
let renderedText =
      let i'value1 = ...
          i'value2 = ...
      in [int||Values are {value1} and {value2}]
@

and so interpolating only local declarations.

-}
tickedValueInterpolator :: ValueInterpolator
tickedValueInterpolator = ValueInterpolator
  \txt -> runValueInterpolator simpleValueInterpolator ("i'" <> txt)

-- | Marks the most common space-like characters.
simpleInvisibleCharsDemonstration :: InvisibleCharsDemonstration
simpleInvisibleCharsDemonstration = InvisibleCharsDemonstration go
  where
    go = \case
      ' ' : s ->
        '·' : go s
      '\r' : '\n' : s ->
        '⤶' : '\r' : '\n' : go s
      '\r' : s      ->
        '⤶' : '\r' : go s
      '\n' : s        ->
        '⤶' : '\n' : go s

      -- It's a good question how to render tab.
      -- It may look like 2 spaces, or 8 spaces, depending on
      -- the machine where it is rendered.
      -- So my stance is that using Tab for text alignment should be avoided,
      -- and we better choose some symbol that /does not/ reflect the space
      -- potentially occupied by a tab.
      -- A use case that we aim at is e.g. using Tab in CSV; there we would
      -- just want to know that Tab is present, we don't care how Tab
      -- character affects the text appearance.
      '\t' : s        -> '→' : go s

      c : s           -> c : go s
      []              -> []

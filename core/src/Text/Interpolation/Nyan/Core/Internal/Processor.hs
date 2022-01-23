module Text.Interpolation.Nyan.Core.Internal.Processor where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Interpolation.Nyan.Core.Internal.Base

-- | Applies the transformations like spaces stripping.
processIntString :: SwitchesOptions -> ParsedInterpolatedString -> InterpolatedString
processIntString sopts istr = istr
  & V.fromList
  & do if leadingNewlineStripping sopts then stripLeadingNewline else id
  & do if trailingSpacesStripping sopts then stripTrailingLeadingWs else id
  & do if indentationStripping sopts then stripCommonIndentation else id
  & V.toList
  & do if reducedNewlines sopts then reduceNewlines else id
  & V.fromList
  & do if spacesTrimming sopts then trimLeftSpaces . trimRightSpaces else id
  & V.toList
    -- We don't need the information about trailing whitespaces anymore
  & unfoldWsData
    -- Glue strings, as the previous stage put texts and whitespaces separately
  & glueStrings
  where
    (&) = flip ($)

    stripLeadingNewline ps = case V.uncons ps of
      Just (PipNewline _, ps') -> ps'
      _                        -> ps

    stripTrailingLeadingWs ps = case V.unsnoc ps of
      Just (ps', PipLeadingWs _) -> ps'
      _                          -> ps

    trimSpacesInPiece trimText = \case
      PipNewline _ -> Nothing
      PipLeadingWs _ -> Nothing
      PipEmptyLine -> Nothing
      PipString s ->
        let s' = trimText s
        in if T.null s' then Nothing else Just (PipString s')
      p@PipInt{} -> Just p

    trimLeftSpaces ps = case V.uncons ps of
      Nothing -> mempty
      Just (p, ps') -> case trimSpacesInPiece T.stripStart p of
        Nothing -> trimLeftSpaces ps'
        Just p' -> V.cons p' ps'

    trimRightSpaces ps = case V.unsnoc ps of
      Nothing -> mempty
      Just (ps', p) -> case trimSpacesInPiece T.stripEnd p of
        Nothing -> trimRightSpaces ps'
        Just p' -> V.snoc ps' p'

    stripCommonIndentation ps =
      let
        interestingIndent piece = do
          PipLeadingWs ws <- pure piece
            -- Lines without payload will likely be completely empty
            -- which is forced by trailing newline stripping
            -- So a line with 0 leading spaces won't not affect anything
          guard (ws /= 0)
          Just ws
        minIndent = case mapMaybe interestingIndent (V.toList ps) of
          []  -> error "min indent requested unnecessarily"
          res -> minimum res
      in flip V.mapMaybe ps \case
        PipLeadingWs ws ->
          guard (ws > minIndent) $> PipLeadingWs (ws - minIndent)
        other -> Just other

    reduceNewlines = \case
        -- The initial case is special - we just want to remove the leading newline
        PipNewline{} : l -> skipNext l
        p : l            -> p : reduceNext l
        []               -> []
      where
        -- Reduce the next encountered newline
        reduceNext = \case
          PipNewline{} : p : l -> case p of
            -- Multiple newlines in a row are just reduced
            PipEmptyLine{} -> p : skipNext l
            -- Otherwise we see two adjacent non-empty lines
            _              -> PipString " " : skipNext (p : l)
          [PipNewline{}] -> []
          p : l -> p : reduceNext l
          [] -> []

        -- Skip all the next newlines as-is
        skipNext = \case
          p@PipNewline{} : l   -> p : skipNext l
          p@PipEmptyLine{} : l -> p : skipNext l
          -- This case is questionable.
          -- Let's assume, that those who think that invisible spaces should not
          -- affect newlines reduction, also have trailing spaces cleanup in IDE.
          -- And there might be people who want special tuning and make
          -- invisible spaces to break the newlines sequence.
          p@PipLeadingWs{} : l -> p : skipNext l
          p@PipString{} : l    -> p : reduceNext l
          p@PipInt{} : l       -> p : reduceNext l
          []                   -> []

    unfoldWsData :: ParsedInterpolatedString -> InterpolatedString
    unfoldWsData = map \case
      PipString s    -> IpString s
      PipNewline nl  -> IpString nl
      PipLeadingWs n -> IpString . mconcat $ replicate (fromIntegral n) " "
      PipEmptyLine   -> IpString mempty
      PipInt i       -> IpInt i

    glueStrings :: InterpolatedString -> InterpolatedString
    glueStrings = \case
      []                             -> []
      IpString s1 : IpString s2 : ps -> glueStrings (IpString (s1 <> s2) : ps)
      p : ps                         -> p : glueStrings ps

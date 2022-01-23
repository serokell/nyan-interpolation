module Text.Interpolation.Nyan.Core.Internal.Parser where

import Control.Applicative (many, optional)
import Control.Monad (guard, when, (<=<))
import Control.Monad.State (MonadState, execStateT, get, put)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Fmt (Builder, build, fmt)
import Text.Interpolation.Nyan.Core.Internal.Base
import Text.Megaparsec (Parsec, customFailure, eof, errorBundlePretty, label, lookAhead, parse,
                        single, takeWhile1P, takeWhileP)
import Text.Megaparsec.Error (ShowErrorComponent (..))

newtype OptionChanged = OptionChanged Bool
  deriving stock (Show, Eq)

-- | An accumulator for switch options during parsing.
data SwitchesOptionsBuilder = SwitchesOptionsBuilder
  { spacesTrimmingB          :: (OptionChanged, Maybe Bool)
  , indentationStrippingB    :: (OptionChanged, Maybe Bool)
  , leadingNewlineStrippingB :: (OptionChanged, Maybe Bool)
  , trailingSpacesStrippingB :: (OptionChanged, Maybe Bool)
  , returnTypeB              :: (OptionChanged, Maybe ReturnType)
  , reducedNewlinesB         :: (OptionChanged, Maybe Bool)
  , monadicB                 :: (OptionChanged, Maybe Bool)
  , demonstrationLevelB      :: DemonstrationLevel
  }

toSwitchesOptionsBuilder :: DefaultSwitchesOptions -> SwitchesOptionsBuilder
toSwitchesOptionsBuilder DefaultSwitchesOptions{..} =
  SwitchesOptionsBuilder
  { spacesTrimmingB = (OptionChanged False, defSpacesTrimming)
  , indentationStrippingB = (OptionChanged False, defIndentationStripping)
  , leadingNewlineStrippingB = (OptionChanged False, defLeadingNewlineStripping)
  , trailingSpacesStrippingB = (OptionChanged False, defTrailingSpacesStripping)
  , returnTypeB = (OptionChanged False, defReturnType)
  , reducedNewlinesB = (OptionChanged False, defMonadic)
  , monadicB = (OptionChanged False, defMonadic)
  , demonstrationLevelB = DemonstrateNone
  }

finalizeSwitchesOptions :: MonadFail m => SwitchesOptionsBuilder -> m SwitchesOptions
finalizeSwitchesOptions SwitchesOptionsBuilder{..} = do
  spacesTrimming <- fromOptional "spaces trimming" spacesTrimmingB
  indentationStripping <- fromOptional "indentation stripping" indentationStrippingB
  leadingNewlineStripping <- fromOptional "leading newline stripping" leadingNewlineStrippingB
  trailingSpacesStripping <- fromOptional "trailing spaces stripping" trailingSpacesStrippingB
  returnType <- fromOptional "return type" returnTypeB
  reducedNewlines <- fromOptional "reduced newlines" reducedNewlinesB
  monadic <- fromOptional "monadic" monadicB
  let demonstrationLevel = demonstrationLevelB
  return SwitchesOptions{..}
  where
    fromOptional desc (_, mval) = case mval of
      Nothing  -> fail $ "Switch for " <> desc <> " must be specified"
      Just val -> pure val

type SwitchesOptionsSetter m = (MonadState SwitchesOptionsBuilder m, MonadFail m)

setIfNew
  :: (MonadFail m, Eq a)
  => String -> a -> (OptionChanged, Maybe a) -> m (OptionChanged, Maybe a)
setIfNew desc new (OptionChanged ch, old)
  | ch = fail $ "Modifying `" <> desc <> "` option for the second time"
  | old == Just new = fail $ "Switch option `" <> desc <> "` is set redundantly"
  | otherwise = return (OptionChanged True, Just new)

setSpacesTrimming :: SwitchesOptionsSetter m => Bool -> m ()
setSpacesTrimming enable = do
  opts <- get
  res <- setIfNew "spaces trimming" enable (spacesTrimmingB opts)
  put opts{ spacesTrimmingB = res }

setIndentationStripping :: SwitchesOptionsSetter m => Bool -> m ()
setIndentationStripping enable = do
  opts <- get
  res <- setIfNew "indentation stripping" enable (indentationStrippingB opts)
  put opts{ indentationStrippingB = res }

setLeadingNewlineStripping :: SwitchesOptionsSetter m => Bool -> m ()
setLeadingNewlineStripping enable = do
  opts <- get
  res <- setIfNew "leading newline stripping" enable (leadingNewlineStrippingB opts)
  put opts{ leadingNewlineStrippingB = res }

setTrailingSpacesStripping :: SwitchesOptionsSetter m => Bool -> m ()
setTrailingSpacesStripping enable = do
  opts <- get
  res <- setIfNew "trailing spaces stripping" enable (trailingSpacesStrippingB opts)
  put opts{ trailingSpacesStrippingB = res }

setReducedNewlines :: SwitchesOptionsSetter m => Bool -> m ()
setReducedNewlines enable = do
  opts <- get
  res <- setIfNew "reduced newlines" enable (reducedNewlinesB opts)
  put opts{ reducedNewlinesB = res }

setMonadic :: SwitchesOptionsSetter m => Bool -> m ()
setMonadic enable = do
  opts <- get
  res <- setIfNew "monadic" enable (monadicB opts)
  put opts{ monadicB = res }

setReturnType :: SwitchesOptionsSetter m => ReturnType -> m ()
setReturnType ty = do
  opts <- get
  res <- setIfNew "return type" ty (returnTypeB opts)
  put opts{ returnTypeB = res }

accountDemonstration :: SwitchesOptionsSetter m => m ()
accountDemonstration = do
  opts <- get
  when (demonstrationLevelB opts == maxBound) $
    fail "Too high demonstration level"
  put opts{ demonstrationLevelB = toEnum $ fromEnum (demonstrationLevelB opts) + 1 }

notAnyOf :: [Char -> Bool] -> Char -> Bool
notAnyOf ps c = not $ or (sequence ps c)

one :: a -> [a]
one = (: [])

data CustomParserFailure
  = SwitchesHelpRequested DefaultSwitchesOptions

-- These instances are necessary for megaparsec
instance Eq CustomParserFailure where
  a == b = compare a b == EQ
instance Ord CustomParserFailure where
  SwitchesHelpRequested{} `compare` SwitchesHelpRequested{} = EQ

instance ShowErrorComponent CustomParserFailure where
  showErrorComponent = \case
    SwitchesHelpRequested defSOpts -> fmt $ switchesHelpMessage defSOpts

switchesSectionP :: DefaultSwitchesOptions -> Parsec CustomParserFailure Text SwitchesOptions
switchesSectionP defSOpts =
  finalizeSwitchesOptions <=<
  flip execStateT (toSwitchesOptionsBuilder defSOpts) $ many $ label switchLabel $ asum
    [ asum
      [ single 's' $> True
      , single 'S' $> False
      ] >>= setSpacesTrimming

    , asum
      [ single 'd' $> True
      , single 'D' $> False
      ] >>= setIndentationStripping

    , asum
      [ single 'a' $> True
      , single 'A' $> False
      ] >>= setLeadingNewlineStripping

    , asum
      [ single 'z' $> True
      , single 'Z' $> False
      ] >>= setTrailingSpacesStripping

    , asum
      [ single 'n' $> True
      , single 'N' $> False
      ] >>= setReducedNewlines

    , asum
      [ single 'm' $> True
      , single 'M' $> False
      ] >>= setMonadic

    , asum
      [ single 'B' $> AnyFromBuilder
      , single 'b' $> ConcreteBuilder
      , single 't' $> ConcreteText
      , single 'T' $> ConcreteLText
      ] >>= setReturnType

    , single '!' >> accountDemonstration

    , single '?' >> customFailure (SwitchesHelpRequested defSOpts)

    ]
    where
      switchLabel = "switch option (type '?' here for help)"

switchesHelpMessage :: DefaultSwitchesOptions -> Builder
switchesHelpMessage sopts =
  let _exhaustivnessCheck :: SwitchesOptions = SwitchesOptions
        (error "")
        (error "")
        (error "")
        (error "")
        (error "")
        (error "")
        (error "")
        (error "")
        -- ↑ Note: If you edit this, you may also need to update
        -- the help messages below.
  in mconcat
    [ "\nHelp on switches:\n"
    , helpOnOptions (defSpacesTrimming sopts)
        [ ("s", "enable spaces trimming", Just True)
        , ("S", "disable spaces trimming", Just False)
        ]

    , helpOnOptions (defIndentationStripping sopts)
        [ ("d", "enable indentation stripping", Just True)
        , ("D", "disable indentation stripping", Just False)
        ]

    , helpOnOptions (defLeadingNewlineStripping sopts)
        [ ("a", "enable leading newline stripping", Just True)
        , ("A", "disable leading newline stripping", Just False)
        ]

    , helpOnOptions (defTrailingSpacesStripping sopts)
        [ ("z", "enable trailing spaces stripping", Just True)
        , ("Z", "disable trailing spaces stripping", Just False)
        ]

    , helpOnOptions (defReducedNewlines sopts)
        [ ("n", "enable newlines reducing", Just True)
        , ("N", "disable newlines reducing", Just False)
        ]

    , helpOnOptions (defMonadic sopts)
        [ ("m", "enable monadic interpolated values", Just True)
        , ("M", "disable monadic interpolated values", Just False)
        ]

    , helpOnOptions (defReturnType sopts)
        [ ("t", "return `Text`", Just ConcreteText)
        , ("T", "return lazy `Text`", Just ConcreteLText)
        , ("b", "return `Builder`", Just ConcreteBuilder)
        , ("B", "return any text-like type (`FromBuilder a => a`)", Just AnyFromBuilder)
        ]

    , helpOnOptions DemonstrateNone
        [ ("!", "show rendered text (without substitutions) as a warning", DemonstrateExact)
        , ("!!", "like ! but also marks invisible characters like spaces", DemonstrateInvisible)
        ]
    ]
  where
    helpOnOptions defVal available = mconcat
      [ "· " <> build @Text switch <> " - " <> help <> "\n"
      | (switch, help, val) <- available
      , val /= defVal
      ]

intPieceP :: Ord e => Parsec e Text [ParsedIntPiece]
intPieceP = asum
  [
    -- consume normal text
    one . PipString <$> takeWhile1P Nothing (notAnyOf [(== '\\'), (== '#'), isSpace])

    -- potentially interpolator case
  , single '#' *> do
      mode <- takeWhileP Nothing \c ->
        isAlphaNum c || c == '_'
      asum
        [ do
            -- interpolator
            _ <- single '{'
            intTxt <- many $ asum
              [ takeWhile1P (Just "interpolated piece") $ notAnyOf [(== '\\'), (== '}')]
              , single '\\' >> T.singleton <$> asum
                [ single '\\'
                , single '}'
                ]
              ]
            _ <- single '}'

            return . one $ PipInt IntData
              { idMode = mode
              , idCode = mconcat intTxt
              }

          -- just plain text
        , return $ one . PipString $ "#" <> mode
        ]

    -- escaped text
  , single '\\' *> asum
      [ one . PipString . T.singleton <$> single '\\'
      , one . PipString . T.singleton <$> single '#'
        -- trailing '\' cancels newline feed
      , newline *> lineStart
      ]

    -- newline
  , (:) <$> newline <*> lineStart

    -- fast spacing
  , one . PipString <$> takeWhile1P Nothing isNonNewlineSpace

  , fail "Unexpected: failed to consume some input"

  ]
  where
    newline = PipNewline . mconcat <$> sequence
      [ maybe "" T.singleton <$> optional (single '\r')
      , T.singleton <$> single '\n'
      ]
    isNonNewlineSpace c = isSpace c && c /= '\n' && c /= '\r'

    -- Parse indentation
    lineStart = asum
      [ lookAhead newline $> [PipEmptyLine]
      , do
          wss <- fromIntegral @Int @Word . T.length <$>
            takeWhileP Nothing isNonNewlineSpace
          return $ guard (wss > 0) $> PipLeadingWs wss
      ]

-- | Since the parser may produce several 'PipString' with different kind of
-- content (e.g. spaces and words), we would like to glue those before passing
-- the interpolated string to the next stage.
glueParsedStrings :: ParsedInterpolatedString -> ParsedInterpolatedString
glueParsedStrings = \case
  []                               -> []
  -- TODO: use Builder here
  PipString s1 : PipString s2 : ps -> glueParsedStrings (PipString (s1 <> s2) : ps)
  p : ps                           -> p : glueParsedStrings ps

intStringP
  :: DefaultSwitchesOptions
  -> Parsec CustomParserFailure Text (SwitchesOptions, ParsedInterpolatedString)
intStringP sopts = do
  switches <- switchesSectionP sopts
  _ <- single '|'
  pieces <- glueParsedStrings . concat <$> many intPieceP
  eof
  return (switches, pieces)

parseIntString
  :: DefaultSwitchesOptions
  -> Text
  -> Either String (SwitchesOptions, ParsedInterpolatedString)
parseIntString defSOpts txt =
  first errorBundlePretty $
    parse (intStringP defSOpts) "int QQ" txt

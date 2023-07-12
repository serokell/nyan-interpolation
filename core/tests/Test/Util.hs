-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Util where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Extension (..), Quasi (..), mkName)
import Test.HUnit (Assertion, assertFailure)

import Text.Interpolation.Nyan.Core

-- * HUnit helpers

(@?) :: HasCallStack => a -> (a -> Bool) -> Assertion
(@?) r p = if p r then pure () else assertFailure "Predicate does not hold"

-- * Working with Q monad

data QReport
  = QRWarning String
  | QRError String
  deriving stock (Show, Eq)

data QOpts = QOpts
  { qoOverloadedStringsEnabled :: Bool
  , qoAvailableVariables       :: [Text]
  }

defQOpts :: QOpts
defQOpts = QOpts
  { qoOverloadedStringsEnabled = True
  , qoAvailableVariables = mempty
  }

newtype TestQ a = TestQ { unTestQ :: ReaderT QOpts (ExceptT String (WriterT [QReport] IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runTestQ' :: QOpts -> TestQ a -> IO ([QReport], Either String a)
runTestQ' qopts = fmap swap . runWriterT . runExceptT . flip runReaderT qopts . unTestQ

runTestQ :: TestQ a -> IO (Either String a)
runTestQ = fmap snd . runTestQ' defQOpts

instance MonadFail TestQ where
  fail = TestQ . throwError

liftToTestQ :: IO a -> TestQ a
liftToTestQ = TestQ . lift . lift . lift

instance Quasi TestQ where
  qReport True msg  = TestQ $ tell [QRError msg]
  qReport False msg = TestQ $ tell [QRWarning msg]

  qRecover = error "not implemented"

  qIsExtEnabled = \case
    OverloadedStrings -> TestQ $ asks qoOverloadedStringsEnabled
    other             -> error $ "Asked for extension " <> show other
  qExtsEnabled = error "not implemented"

  qLookupName _cond name = TestQ do
    vars <- asks qoAvailableVariables
    return $ guard (T.pack name `elem` vars) $> mkName name

  qNewName a = liftToTestQ $ qNewName a
  qReify a = liftToTestQ $ qReify a
  qReifyFixity a = liftToTestQ $ qReifyFixity a
  qReifyType a = liftToTestQ $ qReifyType a
  qReifyInstances a b = liftToTestQ $ qReifyInstances a b
  qReifyRoles a = liftToTestQ $ qReifyRoles a
  qReifyAnnotations a = liftToTestQ $ qReifyAnnotations a
  qReifyModule a = liftToTestQ $ qReifyModule a
  qReifyConStrictness a = liftToTestQ $ qReifyConStrictness a
  qLocation = liftToTestQ qLocation
  qRunIO a = liftToTestQ $ qRunIO a
  qAddDependentFile a = liftToTestQ $ qAddDependentFile a
  qAddTempFile a = liftToTestQ $ qAddTempFile a
  qAddTopDecls a = liftToTestQ $ qAddTopDecls a
  qAddForeignFilePath a b = liftToTestQ $ qAddForeignFilePath a b
  qAddModFinalizer a = liftToTestQ $ qAddModFinalizer a
  qAddCorePlugin a = liftToTestQ $ qAddCorePlugin a
  qGetQ = liftToTestQ qGetQ
  qPutQ a = liftToTestQ $ qPutQ a
  qGetDoc a = liftToTestQ $ qGetDoc a
  qPutDoc a b = liftToTestQ $ qPutDoc a b
  qGetPackageRoot = liftToTestQ $ qGetPackageRoot

-- * Interpolators

int :: QuasiQuoter
int = mkInt defaultInterpolatorOptions
  { defaultSwitchesOptions = recommendedDefaultSwitchesOptions
  }

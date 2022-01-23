module Test.Util where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Tuple (swap)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Extension (..), Q, Quasi (..))
import Test.HUnit (Assertion, assertFailure)

import Text.Interpolation.Nyan.Core

-- * HUnit helpers

(@?) :: HasCallStack => a -> (a -> Bool) -> Assertion
(@?) r p = if p r then pure () else assertFailure "Predicate does not hold"

-- * Working with Q monad

data QReport
  = QRWarning String
  | QRError String

data QOpts = QOpts
  { qoOverloadedStringsEnabled :: Bool
  }

defQOpts :: QOpts
defQOpts = QOpts
  { qoOverloadedStringsEnabled = True
  }

newtype TestQ a = TestQ { unTestQ :: ReaderT QOpts (ExceptT String (WriterT [QReport] Q)) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runTestQ' :: QOpts -> TestQ a -> Q ([QReport], Either String a)
runTestQ' qopts = fmap swap . runWriterT . runExceptT . flip runReaderT qopts . unTestQ

runTestQ :: TestQ a -> Q (Either String a)
runTestQ = fmap snd . runTestQ' defQOpts

instance MonadFail TestQ where
  fail = TestQ . throwError

liftToTestQ :: Q a -> TestQ a
liftToTestQ = TestQ . lift . lift . lift

instance Quasi TestQ where
  qReport True msg  = TestQ $ tell [QRError msg]
  qReport False msg = TestQ $ tell [QRWarning msg]

  qRecover = error "not implemented"

  qIsExtEnabled = \case
    OverloadedStrings -> TestQ $ asks qoOverloadedStringsEnabled
    other             -> error $ "Asked for extension " <> show other
  qExtsEnabled = error "not implemented"

  qNewName a = liftToTestQ $ qNewName a
  qLookupName a b = liftToTestQ $ qLookupName a b
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

-- * Interpolators

int :: QuasiQuoter
int = mkInt defaultInterpolatorOptions
  { defaultSwitchesOptions = recommendedDefaultSwitchesOptions
  }

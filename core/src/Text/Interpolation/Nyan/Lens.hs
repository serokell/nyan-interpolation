-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Text.Interpolation.Nyan.Lens
  ( module Text.Interpolation.Nyan.Lens.TH
  , module Text.Interpolation.Nyan.Lens.Type
  , (^.)

  , (%~)
  , (%=)

  , (.~)
  , (.=)

  , (?~)
  , (?=)

  , (&~)
  )
  where

import Control.Monad.State (MonadState, State, execState, modify)
import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

import Text.Interpolation.Nyan.Lens.TH
import Text.Interpolation.Nyan.Lens.Type

(^.) :: s -> Getting a s a -> a
s ^. l = getConst $ l Const s

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)

(%=) :: MonadState s m => ASetter s s a b -> (a -> b) -> m ()
l %= f = modify (l %~ f)

(.~) :: ASetter s t a b -> b -> s -> t
l .~ b = runIdentity . l (const $ Identity b)

(.=) :: MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)

(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ b = l .~ (Just b)

(?=) :: MonadState s m => ASetter s s a (Maybe b) -> b -> m ()
l ?= b = modify (l ?~ b)

(&~) :: s -> State s a -> s
s &~ l = execState l s

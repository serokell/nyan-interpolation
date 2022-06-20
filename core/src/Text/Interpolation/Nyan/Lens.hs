-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Text.Interpolation.Nyan.Lens
  ( module Text.Interpolation.Nyan.Lens.Type
  , view
  , over
  , set
  , lens
  , (^.)
  )
  where

import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

import Text.Interpolation.Nyan.Lens.Type

(^.) :: s -> SimpleGetter s a -> a
s ^. l = view l s

set :: ASetter' s a -> a -> s -> s
set l a = runIdentity . l (const $ Identity a)

view :: SimpleGetter s a -> s -> a
view l = getConst . l Const

over :: ASetter' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

lens :: Functor f => (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens getter setter f s = setter s <$> f (getter s)

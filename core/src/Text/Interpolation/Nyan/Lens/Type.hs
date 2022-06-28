-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Text.Interpolation.Nyan.Lens.Type where

import Control.Applicative (Const)
import Data.Functor.Identity (Identity)

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type ASetter' s a = ASetter s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type SimpleGetter s a = forall r. Getting r s a

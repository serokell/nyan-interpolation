-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Text.Interpolation.Nyan.Core.Internal.RMode
  ( RMode(..)
  ) where

import Fmt (Builder)

{- | Type that describes rendering modes.

An interpolator that has @rmode'xxx :: RMode t@ variable available in scope
will be able to use @xxx@ rendering mode.

More precisely, any @#xxx{expr}@ in interpolator will be expanded to
@renderWithMode rmode'xxx (expr)@ Haskell code
(some switches may slightly change this behaviour though).

>>> rmode's :: Show a => RMode a
>>> rmode's = RMode (build . show)
>>>
>>> [int||Value is #s{5}|]
"Value is 5"

>>> rmode'hex :: RMode Word
>>> rmode'hex = RMode Fmt.hexF
>>>
>>> [int||Value is #hex{32}]
"Value is 0x20"

-}
newtype RMode a = RMode { renderWithMode :: a -> Builder }

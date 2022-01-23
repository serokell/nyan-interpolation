module Text.Interpolation.Nyan.Core.Internal.RMode
  ( RMode(..)
  ) where

import Fmt (Builder)

{- | Type that describes rendering modes.

An interpolator that has @rmode'xxx :: RMode t@ variable available in scope
will be able to use @xxx@ rendering mode; @t@ here stands for the type
of value appearing in @{}@.

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

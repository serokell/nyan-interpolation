-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{- | Flexible interpolation quoter.

>>> import Text.Interpolation.Nyan
>>> let what = "nyan" in [int||Let's #{what}!|]
"Let's nyan!"

This package comes as an attempt to satisfy numerious needs of a production-scale code.

For the full tutorial, see "Text.Interpolation.Nyan.Tutorial". And further goes
the basic introduction.

== Use of 'Buildable'. By default

Values are interpolated via `Buildable` typeclass as it is more efficient and
dedicated to human-readable errors.

Using 'Show' is still available as:

>>> [int||Shown value: #s{value}|]

The produced text is also polymorphic by default and can be 'Text',
lazy 'LT.Text', 'String', 'Builder' or other type.

== Multiline text

By default, the interpolator strips indentation, one leading newline, and
spaces before @|]@:

>>> :{
  [int|
    List:
      - a
      - b
   |]
:}
"List:\n  - a\n  - b\n"

This behaviour can be fine-tuned with switches.

== Switches

The section after @int@ stands for the switches set.

For instance, to enable spaces trimming:

>>> [int|s| No extra spaces here |]
"No extra spaces here"

To fix the return type to be 'Text':

>>> [int|t|My text|]

To preview the resulting text at compilation stage, pass @!@ switch:

>>> [int|s!| My text: #{value}   |]
>>> -- Will report "My text: ..."

This is reported as a build error, and substitutions are not performed.
Put @!@ twice to also see all the invisible characters.

There are much more switches supported. To quicky ask for a help on switches on
the go, put @?@ switch into the interpolator:

>>> [int|?||]
"<help on switches>"

== Customization

This interpolator allows for a variety of tunning, therefore such a name for
the library.

* Aside from `Buildable` and `Show` support, one can define their own so-called
/rendering modes/.
* The existing rendering modes can be hidden or grouped into modesets.
  For instance, there is a modeset with 'Show' taken as the default rendering
  mode; it is used as the basic modeset in "Text.Interpolation.Nyan.Show" module.
* The set of switches enabled by default is also customizable.

Generally, once the user gets comfortable with the library, we encourage them
to define their own variation of the interpolator that would serve their
preferences, and include it in their custom preludes or util modules.

-}
module Text.Interpolation.Nyan
  ( int
  , module RModes
  ) where

import Text.Interpolation.Nyan.Core
import Text.Interpolation.Nyan.RModes.Buildable as RModes
import Text.Interpolation.Nyan.RModes.CommonExtra as RModes

import Text.Interpolation.Nyan.Full

-- | The interpolator.
int :: QuasiQuoter
int = mkInt defaultInterpolatorOptions
  { defaultSwitchesOptions = recommendedDefaultSwitchesOptions
  , valueInterpolator = fullHaskellValueInterpolator
  }

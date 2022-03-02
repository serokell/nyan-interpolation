-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-unused-imports #-}


{- | Tutorial on the nyan-interpolation.

>>> import Nyan.Interpolation
>>>
>>> let who = "world"
>>> in [int||Hello #{who}!|]
"Hello world!"

Variables can be inserted using @#{ ... }@ syntax.

For the meaning of two @||@ after @int@, see the [Switches](#switches) section.

Defaults are:

* Values put via @#{ ... }@ will be rendered with the help of 'Buildable' instance.
* Resulting type will be any type that has 'FromBuilder' instance.
* If multiline text is provided, its indentation, leading newline and trailing spaces are stripped:

>>> :{
  putStrLn [int||
      Yay
      Here goes
        my text
      and ends here.
  |]  -- the position of the enclosing `|]` does not matter
:}
Yay
Here goes
  my text
and ends here.
<BLANKLINE>

Note that the module with interpolator is assumed to be imported with an
implicit import list. This should not spoil your namespace, @int@ is the only
exported definition you may likely get collisions with.

== Values rendering

If necessary, various rendering means can be used for values:

* @#{val}@ uses 'Buildable' instance;
* @#s{val}@ uses 'Show' instance;
* @#l{"text"}@ for text literals;
* @#n{5}@ for decimal numeric literals;

We refer to the text immediately following the @#@ sign as to /rendering mode/.

Rules for rendering modes are not fixed and depend on the imported interpolation module.
If you don't like 'Buildable' and prefer the conventional 'Show' typeclass,
"Text.Interpolation.Nyan.Show" module is your choice.

Also, you can define your own rules for rendering modes as described in [Adding custom rendering modes](#custom-rendering-modes) section.


== Escaping

If you don't want @#{@ to be interpreted as beginning of an interpolated piece, you can escape it:

>>> [int|| My code: \#{ code } |]
" My code: #{ code } "

Newlines can be escaped to avoid their appearance in the rendered text:

>>> :{
  [int||
    Text starts here \
    and continues here
  |]
:}
"Text starts here and continues here"

Note that [@n@ switch](#n-switch) provides another, often a more convenient way
for controlling newlines.

Finally, slash itself is also subject to escaping.

>>> [int|| My code: \\#{ letter } |]
" My code: \s "


#switches#

== Switches

Interpolated text starts from the swithes section.
Switches section consists of a sequence of characters that ends with @|@.

>>> [int|sb| Hey! ]

In most cases, lowercase letter stands for enabling a switch, while uppercase
letter - for disabling it.

Available switches:

==== s (trim [s]paces)

Trims space-like characters around the text.

>>> [int|s| The text  |]
"The text"

Newlines are affected too:

>>> :{
  [int|s|
<BLANKLINE>
    My text
<BLANKLINE>
  |]
:}
"My text"

==== D (no inten[d]ation stripping)

Indentation will not be stripped.

>>> :{
  [int|D|
    List:
      * Item 1
      * Item 2
  |]
:}
"    List:\n      * Item 1\n     * Item 2\n"

Compare with:

>>> :{
  [int||
    List:
      * Item 1
      * Item 2
  |]
:}
"List:\n  * Item 1\n  * Item 2\n"


==== A (no first newline stripping)

By default, the leading newline is stripped, allowing for neater rendering of
multiline interpolators. When the option is disabled via passing @A@ switch,
the initial line feed will appear in the resulting text.

>>> :{
  [int|A|
    The sentence.
  |]
:}
"\nThe sentence.\n"

==== Z (no last line stripping)

Similarly to the previous option, by default he position of the trailing @|]@
does not affect the result.
When the option is disabled via passing @Z@ switch, spaces at the last line
will be preserved.

>>> :{
  [int|Z|
    Foo
  Bar
    |]
:}
"  Foo\nBar\n  "

#n-switch#

==== n (special [n]ewlines)

Each substring of several newlines in a row will be trimmed by one newline.
Adjacent lines will be glued together with space.

This facilitates formatting in case code snippets are large.

>>> :{
  [int|n|
    Value 1 is #{veryLongComputationOfValue1},
    while value 2 is #{
      veryLongComputationOfValue2
    }
<BLANKLINE>
    Here we go.
<BLANKLINE>
<BLANKLINE>
    That's it.
 |]
:}
"Value 1 is 5, while value 2 is 10.\nHere we go.\n\nThat's it."

==== m ([m]onadic)

Accept monadic values.

>>> putStrLn =<< [int|m|Content of foo: #{readFile "foo"}|]

>>> serverConfig & [int|m|Connected to server #{name} at #{host . addr}:#{port . addr}|]

To be stricter, this switch requires only @Applicative@ instance.

In case monadic actions have side effects, they will be applied in the same order
in which braces appear in the quoter. /But you are not going to use this behaviour, don't you?/

==== t (return [t]ext)

The quoter will return concrete t'Text'.

==== T (return lazy [T]ext)

The quoter will return concrete lazy t'LT.Text'.

==== b (return [b]uilder)

The quoter will return concrete t'Builder'.

==== B (return From[B]uildable)

The quoter will return any type with t'FromBuilder' instance.

==== ! (preview)

Quoter will show as an error (non-blocking for the module's build) showing how the
resulting text looks like with all the enabled switches
(but without substitutions).

Passing the second @!@ will additionally highlight invisible characters.

==== ? (help)

Fail and remind what switches are available.

==== Switches interaction

Few rules to know here.

* Spaces trimming is applied last, it never affects whether other switches will
fire or not:

>>> :{
  [int|s|
    Text 1
    Text 2
  |]
:}
"Text 1\nText 2"

* The switch for stripping trailing whitespaces is applied first and
affects indentation stripping to also ignore the line with @|]@:

>>> :{
  [int||
    My text
  |]  -- this is safe, minimal detected indentation is still 2
:}

* Newlines reduction (@n@) is applied additionally to leading newline stripping (@a@):

>>> :{
  [int|n|
<BLANKLINE>
<BLANKLINE>
  Value 1 is #{veryLongComputationOfValue1},
  value 2 is #{veryLongComputationOfValue2}
<BLANKLINE>
 |]
:}
"\nValue 1 is 5, value 2 is 10\n"


=== Customizing the interpolator

There are two main vectors of customization:

* Changing the quasi-quoter, this includes default switches set, how values are interpolated, e.t.c.
* Editing available rendering modes.

The source of "Text.Interpolation.Nyan" can be a good example of grouping those
to provide a ready-for-use interpolator.

==== Changing default switches

You can use 'mkInt' to supply the desired set of default switches and thus create your own interpolator for the project-wide use.

@
import Text.Interpolation.Nyan.Core

int :: QuasiQuoter
int = mkInt defaultInterpolationOptions
  { switchesOptions = basicDefaultSwitchesOptions
    { spacesTrimming = True
    , returnType = ConcreteText
    }
  }

-- In another module --
-- Spaces trimmed, no indentation stripped, returns @Text@
[int|| My text |]

-- Spaces not trimmed, indentation stripped, returns @FromBuilder a => a@
[int|SdB| My text |]
@


#custom-rendering-modes#

==== Adding custom rendering modes

Rendering mode @xxx@ refers to whatever @rmode'xxx@ value that is available in scope.

For instance, the rendering mode for 'Show' is declared as

@
rmode's :: Show a => RMode a
rmode's = RMode $ build . show
@

Importing this @rmode's@ value will make @s@ mode with 'Show' semantics usable.

Declaring a rendering mode locally will also work:

>>> rmode'yay :: RMode Builder
>>> rmode'yay = RMode \t -> "*" <> t <> "!*"
>>>
>>> [int|t|Say #yay{"hello"}|]
"Say *hello!*"

You can declare /modesets/ ⁠— modules that export several rendering modes.
The user can then easily pick all or some of the modes by importing that module.

-}
module Text.Interpolation.Nyan.Tutorial where

import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Fmt (Buildable, Builder)
import Fmt.Internal.Core (FromBuilder)

import Text.Interpolation.Nyan.Core (mkInt)

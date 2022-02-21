<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# Design decisions

### Making switches as part of the quoter (`int`) vs. as part of the text in quasi quotation

We don't want to have variations of `int` quoter for every possible combination of the switches.

- The user would have to remember the order of switches;
- More difficult to define custom interpolators;
- Extending the set of switches would be difficult;
- We already have just too many switches.

And providing switches as separate Haskell arguments (`[int trim indent|...|]`) would not be concise or would litter the namespace.

### Making the second `|` mandatory vs. optional

One can argue that making second `|` mandatory is an extra, we shouldn't enforce the users
to write `[int||Text|]`. Possible alternatives:

- We could provide `int` without switches and `ints` with switches, or vice versa.

  Pros:

  - Saving extra `|` in case of no-switches.

  Cons:

  - Have to do more edits if suddenly I need to insert a switch.
  - More complex API.
  - It's more difficult to define custom interpolators.
  - I still have to type the @s@ extra character sometimes, so why not just type `|` the second time?

- We could make `|` optional in the very same `int` - recognize when switches are
  present and take them into account.

  Pros:

  - Saving extra `|` in case of no-switches.

  Cons:

  - Behaviour becomes unintuitive.
  - It is possible that I mistakenly wrote a text that is interpreted to contain switches.
    So API gets dangerous to use.

### Why requiring rendering modes (`rmode'xxx`) to be in scope?

One of the key points of the library is that rendering modes can be not only declared, but also hidden and grouped into modesets. Motivation:
- We want to allow at least two `Buildable`-oriented and `Show`-oriented modesets, to make the library more widely used.
* If one can't easily hide a rendering mode, then declaring new rendering modes in a library project is not more an option. And one never knows whether his project will be used as a dependency for another project or not.

So hardcoding the rendering modes is not an option.

Yes, unfortunatelly, this means that the interpolation module is better be imported without explicit list of imported items, but in our case this does not seem to be a big problem:
* We export only `int` and `rmode'xxx` things; the latter will unlikely produce collisions, and the former seems to be guessable.
* If that's not desired, one can put all the necessary definitions to their custom prelude or util module.
* At last, for those who really don't like implicit imports, importing `rmode'*` terms explicitly is probably not so difficult.

### Why `rmode's`?

Obviously the naming should be chosen so that these servant variables never collided with the user-defined functions.

I wanted to use `rmode_s` first, but it didn't work for a prosaic reason: default hlint rules forbid this ("use camelCase"),
hardening declaration of new custom rendering modes.

There is some article that proposed to always use explicit dictionary passing rather than typeclasses,
and name those dictonaries like `show'int`, and I sort of picked the same approach here.

@martoon

### Why this syntax for interpolated values was selected?

I first wanted to use `s{value}` syntax, but it obviously would clash with the text before it.
To allow text like `v123` I suggested writing `v\ {value}` where `\ ` is a fake space.
But that would be annoying, as the primary purpose of interpolator is to be very representative about the resulting text.

So apparently, the rendering mode should appear within the braces.
I chose `\` as a delimiter because
- The delimiter should look similarly to `|` for consistency, but using `|` itself would produce too many `|`s;
- `\` can't be used as an operator, only `\\` is a valid operator, so `\` never collides with actual Haskell code.

@martoon

### `{}` vs `#{}` vs `${}`

A good interpolator should require writing code close to the target text, even in semi-rare cases.

`{}` generally works bad because producing curly braces now requres escaping `\{ ... }`.

With rendering modes the situation gets even worse. I saw two variations here:
- Put the mode before the `{`, like `s{value}`. This worked too bad, now putting a value right after text required special care.
- Put the mode inside `{}`, e.g. `{s\ value }`. This would work, but I found this option not the most neat one, so left this syntax for the future features.

`#{}` syntax extends with modes very well: `#d{value}`, `#mode{value}`.
Need to produce `#{` is extremly rare, so the user mostly never needs escaping here.
And `#{}` stands out quite well. So it seemed like a good option to me.

And regarding the character, the common choices seem to be `#` and `$`.
IMO `#s{value}` looks beter than `$s{value}` - the eye trained on bash interprets the latter as a variable `$s` followed by something, while `#s` for a Haskeller exactly denotes something being configured by the label `s`.

@martoon

---

In theory, we could make this configurable in options for `mkInt` to let the user pick the desired syntax, but going that far into flexibility would deliver some problems. For instance, we want to have IDEs support for our interpolator syntax, and maintaining all the variations and letting the user pick the right one depending on the project would be too tedious.

@martoon

### How switches names were selected?

I actually lacked imagination here, better schemes are welcome.

But the choice of a few switches had a reasoning:

#### `A` and `Z`

I was thinking about `F` and `L` names first, but their are ambiguous: do they stand for "first" and "last", or for "final" and "leading"?

So `A` and `Z` seemed more clear.

@martoon

### Why there is no switch for returning `String`?

Arbitrary use of `String` for such purposes is not a good practice.

We encourage using it only when required by a third-party library, but in this case
the default `FromBuilder r => r` return type fits.

But I'm open to discussion here. @martoon

### Debug tracing?

I once wanted to add a switch that would enable tracing of the result, but then abandoned this idea.
Plain `traceShowId` works quite well here:
- With a proper prelude, adding a call of `traceShowId` takes no effort;
- Tracing function [can produce a warning](https://hackage.haskell.org/package/universum-1.7.2/docs/Universum-Debug.html#v:trace) on call.

Moreover, I think `traceShowId` works better than adding a new switch because
- It is better to be explicit, choosing appropriately between `traceShowId`, `traceM` or even
  some `traceShowTagged` that accepts a grep-able tag.
- Doing `IO` stuff seems to be a bit out of scope of this library.

@martoon

### Why `int`, not just `i`

`i` might be a good name for a local variable, but not for a global declaration, especially the one imported implicitly.

But you always can declare an alias yourself.

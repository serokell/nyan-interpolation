# Nyan-interpolation

Flexible production-scale string interpolation library.

## Motivation

We wanted a solution that would

- Use efficient `Buildable` and `FromBuildable` by default, but would allow using `Show` if necessary too.
- Have numerious switches: indentation stripping, spaces trimming, result preview, and more.
- Have two variations:
  - Full-fledged interpolator, but with a heavy `haskell-src-exts` dependency;
  - Light-weight interpolator that interpolates only variables, but not arbitrary expressions.
- Have convenient defaults, that both can be overriden at use site and customized in the quasi-quoter.
  - It is still possible to use `Show` as default;
  - Default set of switches can be changed to suit your preferences.

## How to use

The usage looks like:

```hs
import Text.Interpolation.Nyan

myText :: Text
myText =
  let val = 5 :: Int
  [int|s| Value is #{val} |]
-- "Value is 5"

myMultilineText :: Text
myMultilineText =
  [int||
    What would you get if you fix a cat and a rainbow?
    And a cookie?
  |]
-- "What would you get if you fix a cat and a rainbow?\nAnd a cockie?\n"
```

The full introduction can be found in the haddock documentation.

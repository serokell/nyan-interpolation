<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# Nyan-interpolation

[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)
[![GitHub CI](https://github.com/serokell/nyan-interpolation/workflows/CI/badge.svg)](https://github.com/serokell/nyan-interpolation/actions)

[![nyan-interpolation](https://badgen.net/hackage/v/nyan-interpolation?color=purple&label=nyan-interpolation)](https://hackage.haskell.org/package/nyan-interpolation)
[![nyan-interpolation-core](https://badgen.net/hackage/v/nyan-interpolation-core?color=cyan&label=nyan-interpolation-core)](https://hackage.haskell.org/package/nyan-interpolation-core)
[![nyan-interpolation-simple](https://badgen.net/hackage/v/nyan-interpolation-simple?color=green&label=nyan-interpolation-simple)](https://hackage.haskell.org/package/nyan-interpolation-simple)

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

## How To Use

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

## Packages

This repository contains the following haskell packages:
* [`full`](./full) contains the basic interpolator and corresponds to the [`nyan-interpolation`](https://hackage.haskell.org/package/nyan-interpolation) library;
* [`core`](./core) provides means for defining custom interpolators and corresponds to the [`nyan-interpolation-core`](https://hackage.haskell.org/package/nyan-interpolation-core) library;
* [`simple`](./simple) contains the lightweight interpolator and corresponds to the [`nyan-interpolation-simple`](https://hackage.haskell.org/package/nyan-interpolation-simple) library.

## IDE Integration

### VSCode

We provide [snippets](/ide/vscode/int.code-snippets), and syntax highlighting is yet to be implemented (see #3).

## For Contributors

Please see [CONTRIBUTING.md](/.github/CONTRIBUTING.md) for more information.

## About Serokell

Nyan-interpolation is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!

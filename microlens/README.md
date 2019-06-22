# microlens-process

[![Hackage](https://img.shields.io/hackage/v/microlens-process.svg)](https://hackage.haskell.org/package/microlens-process)
[![Build Status](https://travis-ci.org/emilypi/lens-process.svg?branch=master)](https://travis-ci.org/emilypi/lens-process)

### This package is still in Beta!

This package is intended to be on the lighter side, with few dependencies aside from `microlens`.


### Motivation

`microlens-process` provides optics for the [process](https://hackage.haskell.org/package/process) package. These optics provide convenient lenses, as well as classy variants for significant classifiable portions of the library for convenience. In addition, we provide some combinators for working with `CreateProcess` types. The intention of this package is to create a well-typed optical layer for `process`, reflecting the shape of certain types of commands at the type level. For instance, consider the following:

For a fuller and more well-typed layer, see [lens-process](https://github.com/emilypi/lens-process)


# lens-process

[![Hackage](https://img.shields.io/hackage/v/lens-process.svg)](https://hackage.haskell.org/package/lens-process)
[![Build Status](https://travis-ci.org/emilypi/lens-process.svg?branch=master)](https://travis-ci.org/emilypi/lens-process)

This package is intended to be minimalistic, with few dependencies aside from `lens`. For full disclosure, this is the minimal dependency graph of `lens-process`:

![lens-process dependencies](https://i.imgur.com/ObGVm2f.jpg)

### Motivation

`lens-process` provides optics for the [process](https://hackage.haskell.org/package/process) package. These optics provide convenient lenses, traversals, and prisms, as well as classy variants for significant classifiable portions of the library for convenience. In addition, we provide some combinators for working with `CreateProcess` types. The intention of this package is to create a well-typed optical layer for `process`, reflecting the shape of certain types of commands at the type level.

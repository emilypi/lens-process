
# lens-process

[![Hackage](https://img.shields.io/hackage/v/lens-process.svg)](https://hackage.haskell.org/package/lens-process)
[![Build Status](https://travis-ci.org/emilypi/lens-process.svg?branch=master)](https://travis-ci.org/emilypi/lens-process)

### This package is still in Beta!

This package is intended to be on the lighter side, with few dependencies aside from `lens`. For full disclosure, this is the minimal dependency graph of `lens-process`:

![lens-process dependencies](dependencies.png)

### Motivation

`lens-process` provides optics for the [process](https://hackage.haskell.org/package/process) package. These optics provide convenient lenses, traversals, and prisms, as well as classy variants for significant classifiable portions of the library for convenience. In addition, we provide some combinators for working with `CreateProcess` types. The intention of this package is to create a well-typed optical layer for `process`, reflecting the shape of certain types of commands at the type level. For instance, consider the following:

```haskell
myStdInProcess
  :: forall a
  . CommandProcess
  -> (Handle -> IO StdStream)
  -> IO (Either Text a)
myStdInProcess cp f g = do
  (mhin, _, _, _) <- createProcess cp
  case mhin of
    Nothing -> error "oh no!"
    Just t -> f t

```

This is very standard `process` code. However, if anyone else encounters this code, it is immediately apparent that information is lacking from the type signature. What `Handle` are we using? What type of command are we running? It is not reflected. Consider instead this replacement:

```haskell
myStdInProcess
  :: forall a b c
  . (HasStdIn a, IsUseHandle b)
  => a -> (b -> IO c) -> IO c
myStdInProcess cp f = do
  handler <- createProcess cp
  case handler ^? _Stdin . _Just . re _UsesHandle of
    Nothing -> error "oh no!"
    Just t -> f t

```

What have we gained here? Well, for one, I know I'm working with something that only has a `_Stdin`, and that i will be piping whatever its `_Stdin` entry is into a `UseHandle`. Then, my function handles it accordingly. Much information has been gained! Classy optics gives us the necessary information to proceed as planned with more confidence.

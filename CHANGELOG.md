# Revision history for lens-process


## 0.2.0.0

The module formerly known as `Internal.hs` is now called `ProcessHandler.hs`

the `CreateProcess` optics have changed to avoid name clashes with `System.IO (stdin, stderr, stdout)`,
instead opting for the post-fixed underscore.

Added doctest suite.


## 0.1.0.0

Decided agains the internal module, narrowing scope

## 0.0.5.0

Expanded the zoo, better docs

## 0.0.4.0

Added 'Internal' module providing convenient handlers for the output of a `createProcess` call.

Added some spurious deps for future work on `process`

## 0.0.3.0

## 0.0.2.0

Widened bounds, fixed up some documentation, added few combinators

## 0.0.1.0

* First version. Released on an unsuspecting world.

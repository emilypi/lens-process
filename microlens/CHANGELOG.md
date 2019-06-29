# Revision history for microlens-process

## 0.2.0.0

Final design - this will not change unless `System.Process` significantly changes.

- `System.Process.Microlens.CommandSpec` is renamed to `System.Process.Microlens.CmdSpec` to be more flush with the data names
- documentation improvements all around
- Type signatures of combinators `inheriting`, `piping`, `handling` and `nostreaming` have had their unnecessary constraints dropped
- `microlens-process` is now in lockstep featurewise
- Traversals + Lenses have been added where in `lens-process` they would have been Prisms and Isos respectively. While it is slightly
  too powerful a constraint, the functionality is sound.

## 0.1.0.2

GHC versions < 710 fail because applicative is not in base. Explicit import
added where needed

## 0.1.0.1

Fix the cabal version

## 0.1.0.0

Expose a new module called `System.Process.Microlens.CommandSpec`, exposing
traversals into the arguments of a raw command.

Added doctests

Improved docs

## 0.0.2.0

Widen bounds, improve docs

## 0.0.1.1

* Correct and widen version bounds

## 0.0.1.0

* First version. Released on an unsuspecting world.

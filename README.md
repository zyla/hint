# hint

[![Build Status](https://travis-ci.org/mvdan/hint.svg?branch=master)](https://travis-ci.org/mvdan/hint)

This library defines an Interpreter monad. It allows to load Haskell
modules, browse them, type-check and evaluate strings with Haskell
expressions and even coerce them into values. The library is thread-safe
and type-safe (even the coercion of expressions to values).

It is, esentially, a huge subset of the GHC API wrapped in a simpler
API.

### Example

Check [example.hs](examples/example.hs) to see a simple but
comprehensive example (it must be run from the `examples` directory).

Algebraic effects and named handlers in Bluefin
===============================================

Algebraic effects are a minimalistic basis for **user-defined effects**.
Using algebraic effects, we can reimplement effects that were built-in the
Bluefin library, and more.

This package leverages the delimited continuations primitives added in
GHC 9.6 to implement algebraic effects in the Bluefin effect system.

This is an experimental project. There is a surprising performance
characteristic to be aware of for any practical applications.
[Details down below.](#quadratic-behavior-of-non-tail-recursion)

## Highlights

### Concurrency

### Nondeterminism

### Truly scoped exceptions.

The scoped exceptions from `Bluefin.Exception` are not completely scoped because
they can be observed by `bracket`. That is probably the right behavior in practice,
but for the sake of science, `Bluefin.Algae.Error` provides truly scoped exceptions,
and implements "`bracket`-observable" exceptions on top.

### Comparison with Bluefin effects

Bluefin is simply a well-scoped framework for the "handle pattern".
Without delimited continuations, only tail-resumptive handlers are expressible.
Bluefin still supports exceptions and state as built-in effects (instead of
them being definable via a common primitive) so much expressiveness remains.

## Lowlights

### Quadratic behavior of non-tail recursion.

For example, the following recursive counter will take time quadratic in `n`
because every call of `modify'` traverses the call stack to find its handler
and capture the continuation.

```haskell
leftRecCounter :: z :> zz => Handler (State Int) z -> Int -> Eff zz ()
leftRecCounter _state 0 = pure ()
leftRecCounter state n = do
  leftRecCounter state (n - 1)
  modify' state (+ 1)
```

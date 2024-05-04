Algebraic effects and named handlers in Bluefin
===============================================

Algebraic effects are a minimalistic basis for **user-defined effects**.
Using algebraic effects, we can reimplement, from scratch, effects that
were built-in the Bluefin library, and more.

This package leverages the delimited continuations primitives added in
GHC 9.6 to implement algebraic effects in the Bluefin effect system.

This is an experimental project. There is a surprising performance
characteristic to be aware of for any practical applications.
[Details down below.](#quadratic-behavior-of-non-tail-recursion)

## Highlights

### Concurrency

Algebraic effects can implement cooperative multithreading.

In the following example, two threads yield a string back and forth,
appending a suffix every time.

```
pingpong :: Eff ss String
pingpong = 'withCoroutine' coThread mainThread
  where
    coThread z0 h = do
      z1 <- 'yield' h (z0 ++ "pong")
      z2 <- 'yield' h (z1 ++ "dong")
      'yield' h (z2 ++ "bong")
    mainThread h = do
      s1 <- 'yield' h "ping"
      s2 <- 'yield' h (s1 ++ "ding")
      s3 <- 'yield' h (s2 ++ "bing")
      pure s3

-- runPureEff pingpong == "pingpongdingdongbingbong"
```

Note that, under the hood, `coThread` and `mainThread` are two `IO` computations.
And we can interleave their executions without native multithreading. This is the
power of delimited continuations.

### Nondeterminism

With the power to interrupt and resume operations freely, we can
do backtracking search in the `Eff` monad.

```
pythagoras :: z :> zz => Handler NonDet.Choice z -> Eff zz (Int, Int, Int)
pythagoras choice = do
  x <- pick choice [1 .. 10]
  y <- pick choice [1 .. 10]
  z <- pick choice [1 .. 10]
  assume choice (x .^ 2 + y .^ 2 == z .^ 2)
  pure (x, y, z)

  where (.^) = (Prelude.^) :: Int -> Int -> Int

-- runPureEff (toList pythagoras) == [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
```

### Truly scoped exceptions.

The scoped exceptions from `Bluefin.Exception` are not completely scoped because
they can be observed by `bracket`. That is probably the right behavior in practice,
but makes the semantics of Bluefin less clear. For the sake of science,
`Bluefin.Algae.Exception` provides truly scoped exceptions, and implements
"`bracket`-observable" scoped exceptions on top.

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

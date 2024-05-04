Named algebraic effect handlers in Bluefin
==========================================

This package leverages the delimited continuations primitives added in
GHC 9.6 to implement algebraic effects in the Bluefin effect system.

Algebraic effects are a minimalistic basis for **user-defined effects**.
Using algebraic effects, we can reimplement, from scratch, effects that
were built-in the Bluefin library, and more.

This is an experimental project. There are surprising performance
characteristics which may be problematic for practical applications.
[Details down below.](#quadratic-behavior-of-non-tail-recursion)

## Highlights

### Concurrency

In the following example, two threads yield a string back and forth, appending
a suffix every time.

```haskell
pingpong :: Eff ss String
pingpong = withCoroutine coThread mainThread
  where
    coThread z0 h = do
      z1 <- yield h (z0 ++ "pong")
      z2 <- yield h (z1 ++ "dong")
      yield h (z2 ++ "bong")
    mainThread h = do
      s1 <- yield h "ping"
      s2 <- yield h (s1 ++ "ding")
      s3 <- yield h (s2 ++ "bing")
      pure s3

-- runPureEff pingpong == "pingpongdingdongbingbong"
```

Note that `coThread` and `mainThread` are just `IO` computations under the hood.
And we can interleave their executions without native multithreading. This is the
power of delimited continuations.

### Nondeterminism

With the ability to interrupt and resume operations freely, we can do
backtracking search in the `Eff` monad.

```haskell
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

#### Backtracking and state

Resuming continuations more than once exposes the impurity of the
implementation of the built-in state effect in `Bluefin.State`.
Here is a program using nondeterminism and state. There are two branches
(`choose`), both modify the state (`incr`).

```
import qualified Bluefin.State as B

nsExampleB :: [Int]
nsExampleB = runPureEff $ NonDet.toList \choice ->
  snd <$> B.runState 0 \state -> do
    _ <- NonDet.choose choice True False
    B.modify' (+ 1) state

-- nsExampleB == [1,2]
```

The state handler (`runState`) is under the nondeterminism handler
(`toList`), which suggests a state-passing interpetation, where the
original state is restored upon backtracking (both branches return `1`):

```
nsExamplePure :: [Int]
nsExamplePure = runPureEff $ NonDet.toList \choice ->
  let state = 0                          -- initial state
  _ <- NonDet.choose choice True False
  let state' = state' + 1                -- modify' (+ 1)
  pure state'                            -- (snd <$> runState) returns the final state

-- nsExamplePure == [1,1]
```

Because `Bluefin.State` is backed by `IORef`, the mutation persists
through backtracking (the second branch returns `2` in the first example).

In comparison, the state effect defined using algebraic effects
(`Bluefin.Algae.State`) has the pure semantics.

```haskell
import qualified Bluefin.Algae.State as A

nsExampleA :: [Int]
nsExampleA = runPureEff $ NonDet.toList \choice ->
  A.execState 0 \state -> do
    _ <- NonDet.choose choice True False
    A.modify' (+ 1) state

-- nsExampleA == [1,1]
```

### Truly scoped exceptions.

The scoped exceptions from `Bluefin.Exception` are not completely scoped because
they can be observed by `bracket`. That is probably the right behavior in practice,
but makes the semantics of Bluefin less clear. For the sake of science,
`Bluefin.Algae.Exception` provides truly scoped exceptions, and implements
"`bracket`-observable" scoped exceptions on top.

### Comparison with Bluefin effects

The Bluefin effect system provides a well-scoped [handle pattern][handle].
Without delimited continuations, only tail-resumptive algebraic effect handlers
are expressible in Bluefin. Those are effect handlers restricted to the
following form, which is equivalent to type `forall r. f r -> Eff ss r`.

```haskell
(\e k -> _ >>= k)
  :: forall r. f r -> (r -> Eff ss a) -> Eff ss a
```

Unlike algebraic effects with which other computational effects can be
user-defined, Bluefin provides a collection of built-in effects
(state, exceptions, coroutines).

[handle]: https://jaspervdj.be/posts/2018-03-08-handle-pattern.html

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

## More reading

Named effect handlers are described in the literature in:

- [Binders by day, labels by night](https://maciejpirog.github.io/papers/binders-labels.pdf)
    by Dariusz Biernacki et al.
- [First-class names for effect handlers](https://www.microsoft.com/en-us/research/uploads/prod/2021/05/namedh-tr.pdf)
    by Ningning Xie et al. (impemented in the [Koka](https://koka-lang.github.io/koka/doc/index.html) language)
- [Effects, capabilities, and Boxes](https://dl.acm.org/doi/pdf/10.1145/3527320)
    by Jonathan Brachtäuser et al.

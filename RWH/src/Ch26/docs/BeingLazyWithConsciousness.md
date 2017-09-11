# Being lazy with consciousness

Lazy evaluation sometimes makes it trickier to really understand how a piece of
code for folks used to languages with strict semantics (as I am). We sometimes
want (or need) to introduce strictness to avoid space leaks and to make
memory allocations more predictable in certain parts of our code. The usual
suggestion is to "carefully sprinkle strict evaluation" and one of then
classic examples is using `foldl` to sum a list of ints, with the
result that instead of returning a result using constant space, it ends up
failing because thunks pile up until memory is exhausted.
But apart from a couple of really good examples in Real World Haskell I didn't
found too many examples of what that actually mean in more complex scenarios and
I find it always tricky to add strictness to a piece of Haskell code and I'm
confident that one more example won't hurt.

We'll be using the Bloom filter implemented in chapter 26 of Real World Haskell
as an example, the version contained in the book creates the Bloom filter
lazily so our goal will be to create a strict version of that particular piece
of code. In a nutshell, a [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter)
is a probabilistic data structure composed of a number of hashes and an array of
bits whose API allows only insertion and membership test. The latter API might
return false positives with a probability set when the filter is instantiated.
Here's the original version (the code is mostly as it is in the book with a few
amendments):

``` haskell
-- file: BloomFilter/BloomFilter.hs

import BloomFilter.Immutable as B (IBloom, fromList)
import BloomFilter.Hash (Hashable, doubleHash)
import Data.List (genericLength)
import Data.Either (either)

mkFromList :: Hashable a => Int -> [a] -> Either String (B.IBloom a)
mkFromList errRate xs =
  either Left (Right . mkBFilt) $ suggestSizing (genericLength xs) errRate
  where
    mkBFilt (bits, numHashes) =
      B.fromList (doubleHash numHashes) bits xs
```
The function `suggestSizing` provides the optimal size of the underlying array
and the number of hashes to generate given the length of the input list and the
desired rate of false positives, but it's not important for the topic of this
article. Let's try this code out in GHCI:

```
ƛ :set +s -- to print timing/memory stats after each evaluation
ƛ :load BloomFilter.BloomFilter
ƛ let ebf = mkFromList 0.01 ([1..10^6]::[Int])
ebf :: Either String (B.IBloom Int)
(0.01 secs, 4658656 bytes)
```
The fact that `ebf` has not been fully evaluated should be clear since the
evaluation took almost no time, but let's ask GHCI for help:

```
ƛ :print ebf -- prints a value without forcing its computation
ebf = (_t2::Either String (B.IBloom Int))

```
GHCI is telling us that `ebf` is a thunk `_ts` of type `Either String (B.IBloom Int)`.
If we're still not convinced that `ebf` is not evaluated we can ask it if an element
is contained in the Bloom filter:

```
ƛ either (const False) (1 `B.elem`) ebf
True
it :: Bool
(19.44 secs, 13818404512 bytes)
ƛ either (const False) (11 `B.elem`) ebf
True
it :: Bool
(0.01 secs, 3118248 bytes)
```
From the timing/memory information should be pretty clear now that the evaluation
was forced when we explicitly asked for a membership test. That expected given
Haskell's non-strict semantic. If we ask GHCI to give us information about `ebf`
we can see that now it gives us a different answer:

```
ƛ :print ebf
ebf = Right
        (B.IB
           (_t3::Int -> [Word32])
           (Data.Array.Base.UArray
              (GHC.Word.W32# 0) (GHC.Word.W32# 9592954) 9592955
              (_t4::ghc-prim-0.5.0.0:GHC.Prim.ByteArray#)))
```
Let's not focusing on the types - again not important - GHCI is telling us
the value of `ebf` after evaluation. We'd like to force this evaluation *before*
the first time the Bloom filter is used, namely when it is created.


## Tools of the job

There are various ways of forcing evaluation in Haskell, the main ones are: `seq`,
`deepseq`, `rnf` (the last two can be found in the `Control.DeepSeq` module and
need the argument to be an instance of the `NFData` type class) or the handy
`BangPatterns` extension, which is syntactic sugar for `seq`. As a first try,
let's force the evaluation of `ebf` using bang patters and see what happens:

```
ƛ :set -XBangPatterns
ƛ let !ebf' = mkFromList 0.01 ([1..10^6]::[Int])
ebf' :: Either String (B.IBloom Int)
(0.34 secs, 197720920 bytes)
ƛ :print ebf'
ebf' = Right (_t5::B.IBloom Int)
```
That did something, specifically it evaluated `ebf'` a bit so that now we already
know that the construction of the Bloom filter succeeded but did we manage to
instantiate it? By carefully reading the output of GHCI it should be clear that
we're not quite there yet but let's again double check:

```
ƛ either (const False) (11 `B.elem`) ebf'
True
it :: Bool
(19.02 secs, 13624548640 bytes)
```
The membership test took still 19 seconds, as we expected. So what's happening
here? Now it's probably a good point to introduce some terminology that will
help us out understanding what's happening and how to go forward.

## Normal form (NF) and weak head normal form (WHNF)

A reducible expression (or redex) is an expression that can be evaluated until
a value is obtained, i.e. `let x = 1 + 6` is a redex since it can be evaluated
to obtain `let x = 5`. Let's again double check it in GHCI:

```
ƛ let x = 1 + 5 :: Int
x :: Int
ƛ :print x
x = (_t6::Int)
ƛ let !x = 1 + 5 :: Int
x :: Int
ƛ :print x
x = 6
```
At this point `x` cannot be futher evaluated and is said to be in
normal (or canonical) form. Now what about an expression like `Right (1 + 5)`?
It should be clear that it's not in normal form so can we just force evaluation
by adding a bang pattern? Let's see if it works:

```
ƛ let !x = Right (1 + 5) :: Either a Int
x :: Either a Int
ƛ :print x
x = Right (_t8::Int)
```
What's happening here?! It turns out that an expression in Haskell can be in
other form called weak head normal form when it's not a redex itself and further
evaluation of its sub-expressions cannot make it a redex. `Right (1 + 5)` isn't
a redex (`Right` is a constructor for the `Either` type) and it cannot be made
one if the sub-expression `1 + 5` is evaluated. Does that mean we have to unwrap
the sub-expression in order for it to be evaluated? Not necesserily. We could force
the evaluation of the sub-expression before we wrap it:

```
ƛ let !x = let !y = 1 + 5 :: Int in Right y
x :: Either a Int
ƛ :print x
x = Right 6
```
or levarage some of the functions in the `Control.DeepSeq` module:

```
ƛ let !x = let x = Right (1 + 5) :: Either a Int in x `deepseq` x
x :: Either a Int
ƛ :print x
x = Right 6
ƛ let !x = Right (1 + 5) :: Either a Int
x :: Either a Int
ƛ rnf x
()
it :: ()
ƛ :print x
x = Right 6
```
`deepseq` is like `seq` on steroids, it reduces an expression and all its
sub-expressions to normal form (`rnf` which stays for "reduce to normal form" does
exactly the same). Keep in mind is that in order to use these two functions
the argument must be an instance of `NFData` (normal for data).

A better and more in depth explanation and a bunch of very informative links and
more examples can be found in Stephen Diehl's
[What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/#laziness)


## Taking control
Now that we'are aware of all this, let's create a strict version of our `mkFromList`
function and let's call it `mkFromList'` (using the convention other
functions like `foldr'` use). The first function we need to change is
`(Right . mkBFilt)`: this is equivalent to `\x -> Right (mkBFilt x)`
(using [eta-expansion](https://en.wikipedia.org/wiki/Lambda_calculus#.CE.B7-conversion))
and to `\pair -> let bfilt = mkBFilt pair in Right bfilt` if we massage the lambda
a bit. Here `bfilt` needs again to be evaluated so again the easiest thing to do
is to add a bang pattern: `\pair -> let !bfilt = mkBFilt pair in Right bfilt`.
A quick note for all the eta-reduce freaks out there (I put myself in the
category and I made the following mistake myself): beware that the
following code is not equlivalent to what we just did and **won't** work:

``` haskell
-- file: BloomFilter/BloomFilter.hs

mkFromList' errRate xs =
  either Left (Right . mkFilt') $ suggestSizing (genericLength xs) errRate
  where
    mkBFilt' (bits, numHashes) =
      let !bfilt =  B.fromList (doubleHash numHashes) bits xsin bfilt
```
Again `(Right . mkFilt')` is equivalent to `\pair -> Right (mkFilt' pair)` that
is a lambda will be evaluated lazily. Are we done yet? Almost there but still no
cigar. Let's have a look at the type of `ebf'` again: `Either String (B.IBloom Int)`.
What's `IBloom` (the 'I' stays for "immutable")? Here's how it's defined:

``` haskell
-- file: BloomFilter/Internals.hs

data IBloom =
  IB { hash  :: (a -> [Word32])
     , array :: UArray Word32 Bool
     }
```
This closely reflects the definition of a Bloom filter, we have a hash
function that produces hashes for a given value and an array of bits.
Keeping in mind that a constructor is also a function, we might notice that
there is still something we need to force evaluation upon: the `array` field.
In order to do this let's write a strict version of `mkBFilt`, this time using
`seq` for a change:

``` haskell
-- file: BloomFilter/BloomFilter.hs

mkBFilt' (bits, numHashes) =
  let bfilt = B.fromList (doubleHash numHashes) bits xs
  in array bfilt `seq` bfilt
```
Equivalently, we could have pattern-matched on `bfilt` and used a bang pattern
on its `array` field. The final version of our `mkFromList'` function looks like
this:

``` haskell
-- file: BloomFilter/BloomFilter.hs

mkFromList' :: Hashable a => ErrorRate -> [a] -> Either String (B.IBloom a)
mkFromList' errRate xs =
  either Left rightBFilt' $ suggestSizing (genericLength xs) errRate
  where
    rightBFilt' x = let !bfilt = mkBFilt' x in Right bfilt

    mkBFilt' (bits, numHashes) =
      let bfilt = B.fromList (doubleHash numHashes) bits xs
      in array bfilt `seq` bfilt
```
Let's test it in GHCI:

```
ƛ let !ebf'' = mkFromList' 0.01 ([1..10^6]::[Int])
ebf'' :: Either String (B.IBloom Int)
(19.29 secs, 13819004104 bytes)
ƛ :print ebf''
ebf'' = Right
         (B.IB
            (_t1::Int -> [Word32])
            (Data.Array.Base.UArray
               (GHC.Word.W32# 0) (GHC.Word.W32# 9592954) 9592955
               (_t2::ghc-prim-0.5.0.0:GHC.Prim.ByteArray#)))
```
And YES! We finally managed to fully evaluate our Bloom filter before it's first
use in our code.

## Wrapping up

1. Be aware of how to introduce strictness in Haskell code: `seq`, the
  `BangPatterns` extension or the functions in the `Control.DeepSeq` module
2. Use GHCI and leverage the `:print` command and the `+s` flag to help
   understanding how our code is evaluated while developing
3. Keep in mind the difference between NF and WHNF, if we cannot manage to force
   evaluation of an expression is because some sub-expression is still in WHNF
4. Carefully analyse our code to identify where strictness need to be added

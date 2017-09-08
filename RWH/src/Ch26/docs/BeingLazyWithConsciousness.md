# Being lazy with consciousness

Lazy evaluation has his lovers and haters but it's inevitably something we have
to face when writing Haskell code. It usually makes it harder to really
understand a piece of code for folks used to languages with strict
semantics (as I am). Especially when we want (or need) to introduce strictness
to avoid space leaks and to make memory allocation more predictable in certain
parts of our code. The usual suggestion is to "carefully sprinkle strict evaluation"
here and there to achieve this goal. The classic example is calling `foldl` to
sum a list of ints, with the result that instead of returning a result using
constant space, `foldl` ends up failing with a ... error because thunks pile up
until memory is exhausted. Apart from a couple of really good examples in Real
World Haskell I didn't found too many examples of what that actually mean in more
complex scenarios and I find it always tricky to add strictness to a piece of
Haskell code so I'm confident that one more example won't hurt. I'll use the
Bloom filter implemented in Real World Haskell ch. 26 as an example, the version
in the book creates the Bloom filter lazily so our goal will be to create a strict
version of that particular piece of code.

There are various ways of forcing evaluation in Haskell - mainly using `seq`,
`deepseq`, `rnf` (the last two can be found in the `Control.DeepSeq` module and
need the argument to be an instance of the `NFData` type class) or the handy
`BangPatterns` extension - and generally the suggestion is to "carefully sprinkle
strict evaluation" here and there in your code. Let's put this suggestion in
action!

Here's the original version (the code is mostly as it is in the
book with a few amendments):

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
desired error rate, but it's not important for the topic of this article.
Let's try this code out in GHCI:

```
 :set +s -- to print timing/memory stats after each evaluation
 :load BloomFilter.BloomFilter
 let ebf = mkFromList 0.01 ([1..10^6]::[Int])
ebf :: Either String (B.IBloom Int)
(0.01 secs, 4658656 bytes)
```
The fact that `ebf` has not been fully evaluated should be clear since the
evaluation took almost no time, but let's ask GHCI for help:

```
 :print ebf
ebf = (_t2::Either String (B.IBloom Int))

```
GHCI is telling us that `ebf` is an thunk `_ts` of type `Either String (B.IBloom Int)`.
If we're still not convinced that `ebf` is not evaluated we can ask it if an element
is contained in the Bloom filter:

```
 either (const False) (1 `B.elem`) ebf
True
it :: Bool
(19.44 secs, 13818404512 bytes)
 either (const False) (11 `B.elem`) ebf
True
it :: Bool
(0.01 secs, 3118248 bytes)
```
From the timing/memory information should be pretty clear now that the evaluation
was forced when we explicitly asked for a membership test. That expected given
Haskell's non-strict semantic. If we ask GHCI to give us information about `ebf`
we can see that now it gives us a different answer:

```
 :print ebf
ebf = Right
        (B.IB
           (_t3::Int -> [Word32])
           (Data.Array.Base.UArray
              (GHC.Word.W32# 0) (GHC.Word.W32# 9592954) 9592955
              (_t4::ghc-prim-0.5.0.0:GHC.Prim.ByteArray#)))
```
Leaving out the possibly scary types - again not important - GHCI is telling us
the value of `ebf` after evaluation. We'd like to force this evaluation *before*
the first time the Bloom filter is used, namely when it is created. As a first
try, let's apply the suggestion mentioned above and force the evaluation of `ebf`
using bang patters, that is:

```
 :set -XBangPatterns
 let !ebf' = mkFromList 0.01 ([1..10^6]::[Int])
ebf' :: Either String (B.IBloom Int)
(0.34 secs, 197720920 bytes)
 :print ebf'
ebf' = Right (_t5::B.IBloom Int)
```
That did something, specifically it evaluated `ebf'` a bit so that now we already
know that the construction of the Bloom filter succeed but did we manage to
instantiate it? By carefully reading the output of GHCI it should be clear that
we're not quite there yet but let's again double check:

```
 either (const False) (11 `B.elem`) ebf'
True
it :: Bool
(19.02 secs, 13624548640 bytes)
```
The membership test took still 19 seconds, as we expected. Now it's probably a
good point to introduce some terminology that will help us out understanding
what's happening and how to go forward.

## Normal form (NF) and weak head normal form (WHNF)

TODO

## Time for strictness
With that in mind, let's create a strict version of our `mkFromList` function
to reach our goal, let's call it `mkFromList'` using the convention other
functions like `foldr'` use. The first function we need to tweak is
`(Right . mkBFilt)`: this is equivalent to `\x -> Right (mkBFilt x)`
(using [eta-expansion](TODO)) and to
`\pair -> let bfilt = mkBFilt pair in Right bfilt` if we massage the lambda a bit.
Here `bfilt` needs again to be evaluated so again the easiest thing to do is to
modify it like this: `\pair -> let !bfilt = mkBFilt pair in Right bfilt`
(and probably make it its own function). Now for the eta-reduce freaks out there
(I put myself in the category and I made the mistake myself) beware that the
following is not equlivalent to what we just did and **won't** work:

``` haskell
-- file: BloomFilter/BloomFilter.hs

mkFromList' errRate xs =
  either Left (Right . mkFilt') $ suggestSizing (genericLength xs) errRate
  where
    mkBFilt' (bits, numHashes) =
      let !bfilt =  B.fromList (doubleHash numHashes) bits xsin bfilt
```
Again `(Right . mkFilt')` is equivalent to `\pair -> Right (mkFilt' pair)` that
is a lambda or anonymous function that will be evaluated lazily. Are we done yet?
We're almost there but still no cigar. Let's have a look at the type of `ebf'`
again: `Either String (B.IBloom Int)`. What's `IBloom` (the 'I' stays for
"immutable")? Here's how it's defined:

``` haskell
-- file: BloomFilter/Internals.hs

data IBloom =
  IB { hash  :: (a -> [Word32])
     , array :: UArray Word32 Bool
     }
```
This closely reflects the definition of a Bloom filter, we need a hash
function that produces hashes for a given value and an array of bits that are
set when a new value is inserted and checked when the Bloom filter is queried.
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
 let !ebf'' = mkFromList' 0.01 ([1..10^6]::[Int])
ebf'' :: Either String (B.IBloom Int)
(19.29 secs, 13819004104 bytes)
 :print ebf''
ebf'' = Right
         (B.IB
            (_t1::Int -> [Word32])
            (Data.Array.Base.UArray
               (GHC.Word.W32# 0) (GHC.Word.W32# 9592954) 9592955
               (_t2::ghc-prim-0.5.0.0:GHC.Prim.ByteArray#)))
```
And YES! We finally managed to fully evaluate our Bloom filter before it is first
used in our code.

# Summary
The "carefully sprinkle strict evaluation" suggestion is gold but tricky to put
into action, since it requires a careful analysis of the code that isn't always
trivial. A few bullet points to help us are:

1. Be aware of how to introduce strictness in Haskell code: `seq`, the
  `BangPatterns` extension or the functions in the `Control.DeepSeq` module
2. Use GHCI and leverage the `:print` command and the `+s` flag to help
   understanding how our code is evaluated while developing
3. Always keep in mind the difference between WHNF and NF, it's almost always
   the case that we think something is in NF but it isn't
4. Carefully analyse our code to identify which expressions are in WHNF and
   force their full evaluation

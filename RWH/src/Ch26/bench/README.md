## Dump output of the simplifier

``` sh
stack ghc -- -O2 -c -ddump-simpl --make src/Foo.hs > dump.txt
```

## Build benchmarks with profiling turned ON

``` sh
stack build --bench --no-test --profile
```

ghc --make Main.hs -o main
ghc --make DFAGenerator.hs -o dfa-tester
ghc --make Benchmark.hs -o benchmark
cabal install --lib random
cabal install --lib time
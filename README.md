# How to run the project?
## First method
### 1. Fill `automaton_input.txt` using given format
states: q0 q1 q2  
alphabet: a b  
start: q0  
accepting: q2  
transitions:  
q0 a q1  
q0 b q0  
q1 a q2  
q1 b q0  
q2 a q2  
q2 b q2  

### 2. Compile
```ghc --make Main.hs -o main```
### 3. Run 
```.\main.exe```

## Second method, testing using random DFAs
### 1. Install deps
```cabal install --lib random```
### 2. Compile
```ghc --make DFAGenerator.hs -o dfa-tester```
### 3. Run
```.\dfa-tester.exe```

## Third method, benchmark via generating Cerny automatons
### 1. Install deps
```cabal install --lib time```
### 2. Compile
```ghc --make Benchmark.hs -o benchmark```
### 3. Run
```.\benchmark.exe```


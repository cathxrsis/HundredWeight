# HundredWeight
A Haskell library for proving change ringing methods.

[![Build Status](https://travis-ci.org/cathxrsis/changes.svg?branch=master)](https://travis-ci.org/cathxrsis/changes)

# Example
to print Grandsire Doubles, type the following into the GHCI REPL:

> prelude >:l rounds.hs

> main *> grand = ["3","1","5","1","5","1","5","1","5","1"]

> main *> grandsire = grand ++ grand ++ grand

> main *> putStr (printMethod (method 5 (map (changeFromChar) grandsire)))


## Features

### Implemented
- Data types for bell, place notation, row and method
- Function for evaluating changes at bell and row level
### Future
- Place notation parser

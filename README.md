# Changes
A Haskell library for proving change ringing methods.

# Example
to print Grandsire Doubles, type the following into the GHCI REPL:

> prelude >:l rounds.hs
> main *> grand = ["3","1","5","1","5","1","5","1","5","1"]
> main *> grandsire = grand ++ grand ++ grand
> main *> putStr (printMethod (method 5 grandsire))

## Features

### Implemented
- Data types for bell, place notation, row and method
- Function for evaluating changes at bell and row level
### Future
- Place notation parser

# Mixed Strategies
Copyright © 2012 Bart Massey

This Haskell code, given a payoff matrix, calculates an
optimal mixed strategy for a two-player zero-sum
single-round iterated simultaneous game.

The code follows the method of Chapter 6 of J.D. Williams's
classic book *The Compleat Strategyst* (McGraw-Hill
1954). You might want to have a copy of that book handy to
help in understanding some of the details of the API.

The code is in two parts. A library module,
`Data.MixedStrategy`, provides most of the calculating and
API functionality. A small driver program, `oms`, reads a
payoff matrix from standard input and writes a solution
trace to standard output. The `examples` directory contains
several examples, including two from *The Compleat
Strategyst* used to validate the implementation.

Everything builds fine using `cabal`.

This program is licensed under the "MIT License".  Please
see the file `COPYING` in the source distribution of this
software for license terms.

This repository contains my work for CSE210A.
It is all my own code,
though I did consult https://en.wikibooks.org/wiki/Haskell/ParseExps to improve my parsing.

Running the Program

The main file of interest is Comm.hs, which implements the WHILE language with arrays,
It can be compiled by running 'make' in the top level directory, and creates a binary called 'while' in the same directory.
It can be run with the command 'make run', which will compile the executable if necessary and run it with './while'.

Testing

The program is tested by running './test.sh'.
I also wrote several new test cases in 'tests/custom.bats'

Requirements

- Control.Applicative
- Data.Map
- Data.Char
- Prelude
- Text.ParserCombinators.ReadP




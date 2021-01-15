This repository contains my work for CSE210A. It is all my own code (except the testing scripts of course).

Running the Program

The main file of interest is language.hs, which implements an arithmetic language with exponentiation,
It can be compiled by running 'make' in the top level directory, and creates a binary called 'arith' in the same directory.
It can be run with the command 'make run', which will compile the executable if necessary and run it with './arith'.

Testing

The program is tested by running './test.sh'.
I also wrote several new test cases in 'tests/custom.bats'

Requirements

- Control.Applicative
- Data.Char
- Text.ParserCombinators.ReadP




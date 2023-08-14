% syntax analysis

# Compiling Regular expression 
In this homework, you are tasked to develop a lexer and a parser for a regular expression. 

Besides the Scala code for Task 3, you should submit a report which should include detail steps of solving Task 1 and Task 2.


# Regular Expression Parsing

The Regular Expression Syntax is defined as follows 

```
  (Regular Expression) r ::= [c-c] | [cc] | [] | c | r|r | r* | r? | rr | (r) 
  (Character) c ::= a | ... | z | A | ... | Z | 1 | ... | 0
```

## Task 1

Eliminate the Left recursion from the above language, if there is any.


## Task 2

Apply Left factorization to the intermediate result from Task 1.


## Task 3 

Complete the code in `Parser.scala` to implement the top-down parser using Monadic Parser Combinator. You may choose to use the backtracking parsec library `BacktrackParsec.scala` or the on-demand backtrack parsec library `Parsec.scala`. 
The lexer `Lexer.scala` included in the project stub code is a good reference, which you don't need to modify.

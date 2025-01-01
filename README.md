# flippre: FliPpr, embedded 

This is an embedded implementation of FliPpr, an invertible
pretty-printing system. The library provides functions and the
datatypes so that one can write their pretty-printers that are akin to
the usual functional programming style as possible, and invertible to
produce CFG parsers. The current implementation uses `Earley` for
parsing.

## Build Instruction 

This haskell package is managed via stack. So just type

    stack build
    
to build the library. Some, examples are given under `flippre-examples`
directory. For example, to access arithmetic expressions, type: 

    stack repl flippre-examples:exe:arith 

This code lets us check how to acess the code through stack repl:

    stack ide targets
    
using the command main then tests it out

Other tests 
pprExp exp1
    1 + 2 * 3
parseExpP "1 + 2 * 3"
    [Add (Num 1) (Mul (Num 2) (Num 3))]

flippre-examples:exe:arithlet 


2013 skapat,
takes pretty printing, returns parser
derive a parser from a pretty printer goal()

non/pretty input (still valid string) -> 

wadler's combinations bygger det pao

anvaender indexering foer att avgoera anvaendade av paranteser i samband med pretty printing

group foer att undvika problem med space vs newline.

<+ for additional info. pretty <+ nonpretty
manyparens uses this

2018 - PDF
use lambda functions insead of global func defs

rec foer baettre 

arith, let, simplelang

can only generate cfg
- study cfg (grammar based technology)
parsing technique, 
can only define only cfg

e, k, x must be used linearly (only once)

Laes hela boken, typ noedvaendigt
start with examples (learning, understanding), think about how to approach the problems i want to solve, 
markdown -> ->HTML
tweak in order to do it

opening, closing tab
cannot input string and output string at the moment.

might be possible to do a workaround. 


cabal might be easier this works:
    cabal run arith

stack haddock --no-haddock-deps 
to build documentation

present what i want to do. flipper in general. 

flipper library (look into it)

look through examples and ask.
read until chapter 11. 
13, 14, 15 chapter unnecessary. 
vad aer pure? (superclass of a monad?) (monad subclass of applicative and ___?)
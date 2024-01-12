# LISPKIT LISP 

This is an implementation of [Lispkit Lisp](https://en.wikipedia.org/wiki/Lispkit_Lisp) in OCaml. 

Lispkit Lisp is a small, lexically scoped, purely functional subset of Lisp, introduced in the following book 


> Peter Handerson <br>
> **Functional Programming**, Application and Implementation <br>
> Prentice-Hall International, 1980 

While the original book is not available for purchase anymore, many libraries still have a copy. 

## Implementation 

The implementation of the compiler is bootstrapped through an interpreter in `lib/eval.ml`. The compiler itself is written in Lispkit Lisp and is essentially the same as appendix 2 of the above mentioned book. I chose to use a very simple ADT to represent the syntax of the language (`lib/sexp.ml`). This means that the code is not very "OCaml idiomatic", but it trivially allows to see data as code (a feature known as [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity)). 

## Warning

This is work in progress. The compiler and evaluator work (the evaluator can evaluate the compiler applied to itself), however the SECD machine does not yet work. 

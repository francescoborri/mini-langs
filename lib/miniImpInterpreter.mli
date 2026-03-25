open MiniImpAST

exception RuntimeError of string

val run : ast -> int -> int

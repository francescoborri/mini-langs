open MiniFunAST

exception RuntimeError of string

val run : ast -> int -> int

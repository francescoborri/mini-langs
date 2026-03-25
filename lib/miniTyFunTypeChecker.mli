open MiniTyFunAST
open Either

exception TypeError of string

val type_check : ast -> (__type, string) Either.t

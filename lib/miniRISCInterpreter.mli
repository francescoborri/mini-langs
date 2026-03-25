open MiniRISCInstructionSet

exception RuntimeError of string

val run : ast -> int -> int -> int
type reg = RegIn | RegOut | RegA | RegB | Reg of int
type label = MainLabel | Label of int
type brop = Add | Sub | Mul | And | Less
type biop = AddImm | SubImm | MulImm | AndImm
type uop = Not | Copy

type instruction =
  | Nop
  | BinRegOp of brop * reg * reg * reg
  | BinImmOp of biop * reg * int * reg
  | UnOp of uop * reg * reg
  | Load of reg * reg
  | LoadImm of int * reg
  | Store of reg * reg
  | Jump of label
  | CondJump of reg * label * label

type ast = (instruction * (label option)) list

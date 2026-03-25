open Var

type cmd =
  | Skip
  | Assign of var * aexpr
  | Seq of cmd * cmd
  | IfThenElse of bexpr * cmd * cmd
  | While of bexpr * cmd

and aexpr =
  | Int of int
  | Var of var
  | Add of aexpr * aexpr
  | Sub of aexpr * aexpr
  | Mul of aexpr * aexpr

and bexpr =
  | Bool of bool
  | LessThan of aexpr * aexpr
  | And of bexpr * bexpr
  | Not of bexpr

type expr = Aexpr of aexpr | Bexpr of bexpr
type ast = Prog of var * var * cmd
type simple_cmd = SSkip | SAssign of var * aexpr | SGuard of bexpr

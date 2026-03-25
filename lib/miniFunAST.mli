open Var

type ast =
  | Int of int
  | Bool of bool
  | Var of var
  | Fun of var * ast
  | App of ast * ast
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Equal of ast * ast
  | LessThan of ast * ast
  | And of ast * ast
  | Or of ast * ast
  | Not of ast
  | IfThenElse of ast * ast * ast
  | Let of var * ast * ast 
  | LetFun of var * var * ast * ast

type rtval =
  | RTInt of int
  | RTBool of bool
  | RTClosure of var * ast * env
  | RTRecClosure of var * var * ast * env

and env = rtval VarEnv.t
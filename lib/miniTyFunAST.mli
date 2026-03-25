open Var

type __type = TInt | TBool | TFun of __type * __type

type ast =
  | Int of int
  | Bool of bool
  | Var of var
  | Fun of var * __type * ast
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
  | LetFun of var * var * __type * ast * ast

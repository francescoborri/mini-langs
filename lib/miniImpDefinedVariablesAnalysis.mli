open Var
open MiniImpAST
open MiniImpCFG

exception UndefinedVariable of string

type error_loc = Expr of expr | Output of var
type undef_var_error = { undef_var : var; error_loc : error_loc }

val analyze : cfg -> undef_var_error list

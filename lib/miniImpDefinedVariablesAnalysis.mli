open Var
open MiniImpAST
open MiniImpCFG

exception UndefinedVariable of string

type undef_var_error = { undef_var : var; expr : expr }

val analyze : cfg -> undef_var_error list

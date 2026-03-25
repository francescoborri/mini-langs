open Var
open MiniImpAST

type cfg = (simple_cmd list, var) GenericCFG.cfg

val propagate_constants : ast -> ast
val build_cfg : ast -> cfg

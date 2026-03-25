open Var
open MiniImpAST

exception RuntimeError of string

let rec eval_cmd env = function
  | Skip -> env
  | Assign (x, aexpr) -> VarEnv.extend x (eval_aexpr env aexpr) env
  | Seq (cmd1, cmd2) -> eval_cmd (eval_cmd env cmd1) cmd2
  | IfThenElse (bexpr, cmd1, cmd2) ->
      eval_cmd env (if eval_bexpr env bexpr then cmd1 else cmd2)
  | While (bexpr, cmd) as while_cmd ->
      if eval_bexpr env bexpr then
        let env' = eval_cmd env cmd in
        eval_cmd env' while_cmd
      else env

and eval_aexpr env = function
  | Int n -> n
  | Var x -> (
      match VarEnv.lookup x env with
      | Some v -> v
      | None -> raise (RuntimeError ("Unbound variable: " ^ x)))
  | Add (aexpr1, aexpr2) -> eval_aexpr env aexpr1 + eval_aexpr env aexpr2
  | Sub (aexpr1, aexpr2) -> eval_aexpr env aexpr1 - eval_aexpr env aexpr2
  | Mul (aexpr1, aexpr2) -> eval_aexpr env aexpr1 * eval_aexpr env aexpr2

and eval_bexpr env = function
  | Bool b -> b
  | LessThan (aexpr1, aexpr2) -> eval_aexpr env aexpr1 < eval_aexpr env aexpr2
  | And (bexpr1, bexpr2) -> eval_bexpr env bexpr1 && eval_bexpr env bexpr2
  | Not bexpr -> not (eval_bexpr env bexpr)

let run (Prog (x, y, cmd)) input =
  let input_env = VarEnv.extend x input VarEnv.empty in
  match VarEnv.lookup y (eval_cmd input_env cmd) with
  | Some n -> n
  | None -> raise (RuntimeError ("Unbound variable: " ^ y))

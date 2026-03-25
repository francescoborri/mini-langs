open Var
open MiniFunAST

exception RuntimeError of string

let rec eval env = function
  | Int n -> RTInt n
  | Bool b -> RTBool b
  | Var x -> (
      match VarEnv.lookup x env with
      | Some v -> v
      | None -> raise (RuntimeError ("Undefined variable: " ^ x)))
  | Fun (x, term) -> RTClosure (x, term, env)
  | App (term1, term2) -> (
      let val1 = eval env term1 in
      let val2 = eval env term2 in
      match val1 with
      | RTClosure (x, body, env') ->
          let env'' = VarEnv.extend x val2 env' in
          eval env'' body
      | RTRecClosure (f, x, body, env') as rec_closure ->
          let env'' =
            env' |> VarEnv.extend f rec_closure |> VarEnv.extend x val2
          in
          eval env'' body
      | _ -> raise (RuntimeError "Attempting to apply a non-function"))
  | Add (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTInt n1, RTInt n2 -> RTInt (n1 + n2)
      | _ -> raise (RuntimeError "Type error in addition"))
  | Sub (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTInt n1, RTInt n2 -> RTInt (n1 - n2)
      | _ -> raise (RuntimeError "Type error in subtraction"))
  | Mul (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTInt n1, RTInt n2 -> RTInt (n1 * n2)
      | _ -> raise (RuntimeError "Type error in multiplication"))
  | Equal (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTInt n1, RTInt n2 -> RTBool (n1 = n2)
      | _ -> raise (RuntimeError "Type error in equality comparison"))
  | LessThan (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTInt n1, RTInt n2 -> RTBool (n1 < n2)
      | _ -> raise (RuntimeError "Type error in less-than comparison"))
  | And (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTBool b1, RTBool b2 -> RTBool (b1 && b2)
      | _ -> raise (RuntimeError "Type error in logical AND"))
  | Or (term1, term2) -> (
      match (eval env term1, eval env term2) with
      | RTBool b1, RTBool b2 -> RTBool (b1 || b2)
      | _ -> raise (RuntimeError "Type error in logical OR"))
  | Not term -> (
      match eval env term with
      | RTBool b -> RTBool (not b)
      | _ -> raise (RuntimeError "Type error in logical NOT"))
  | IfThenElse (cond, term1, term2) -> (
      match eval env cond with
      | RTBool true -> eval env term1
      | RTBool false -> eval env term2
      | _ -> raise (RuntimeError "Condition in if-then-else must be a boolean"))
  | Let (x, term1, term2) ->
      let val1 = eval env term1 in
      let env' = VarEnv.extend x val1 env in
      eval env' term2
  | LetFun (f, x, body, term) ->
      let closure = RTRecClosure (f, x, body, env) in
      let env' = VarEnv.extend f closure env in
      eval env' term

let run term input =
  match eval VarEnv.empty (App (term, Int input)) with
  | RTInt output -> output
  | _ -> raise (RuntimeError "Expected the program to return an integer")

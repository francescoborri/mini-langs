open Either
open Var
open MiniTyFunAST

exception TypeError of string

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (ty1, ty2) ->
      Printf.sprintf "(%s -> %s)" (string_of_type ty1) (string_of_type ty2)

let rec rec_type_check env = function
  | Int _ -> Left TInt
  | Bool _ -> Left TBool
  | Var x -> (
      match VarEnv.lookup x env with
      | Some ty -> Left ty
      | None -> Right ("Unbound variable: %s" ^ x))
  | Fun (x, ty, term) -> (
      match rec_type_check (VarEnv.extend x ty env) term with
      | Left ty' -> Left (TFun (ty, ty'))
      | Right msg -> Right msg)
  | App (term1, term2) -> (
      match (rec_type_check env term1, rec_type_check env term2) with
      | Left (TFun (ty, ty')), Left ty'' when ty = ty'' -> Left ty'
      | Left (TFun (ty, ty')), Left ty'' ->
          Right
            (Printf.sprintf "Expected argument of type %s, but got %s"
               (string_of_type ty) (string_of_type ty''))
      | Left (TFun _), Right msg -> Right msg
      | Left ty, _ ->
          Right
            (Printf.sprintf "Expected a function, but got %s"
               (string_of_type ty))
      | Right msg, _ -> Right msg)
  | Add (term1, term2) | Sub (term1, term2) | Mul (term1, term2) -> (
      match (rec_type_check env term1, rec_type_check env term2) with
      | Left TInt, Left TInt -> Left TInt
      | _ -> Right "Expected integers")
  | Equal (term1, term2) | LessThan (term1, term2) -> (
      match (rec_type_check env term1, rec_type_check env term2) with
      | Left TInt, Left TInt -> Left TBool
      | _ -> Right "Expected integers")
  | And (term1, term2) | Or (term1, term2) -> (
      match (rec_type_check env term1, rec_type_check env term2) with
      | Left TBool, Left TBool -> Left TBool
      | _ -> Right "Expected booleans")
  | Not term -> (
      match rec_type_check env term with
      | Left TBool -> Left TBool
      | _ -> Right "Expected a boolean")
  | IfThenElse (cond, term1, term2) -> (
      match
        ( rec_type_check env cond,
          rec_type_check env term1,
          rec_type_check env term2 )
      with
      | Left TBool, Left ty, Left ty' when ty = ty' -> Left ty
      | Left TBool, Left ty, Left ty' ->
          Right
            (Printf.sprintf
               "Then and else branches must have the same type, but got %s and \
                %s"
               (string_of_type ty) (string_of_type ty'))
      | Left _, Left _, Left _ -> Right "Expected a boolean condition"
      | Left _, Left _, Right msg
      | Left _, Right msg, Left _
      | Left _, Right msg, Right _
      | Right msg, _, _ ->
          Right msg)
  | Let (x, term1, term2) -> (
      match rec_type_check env term1 with
      | Left ty -> rec_type_check (VarEnv.extend x ty env) term2
      | Right msg -> Right msg)
  | LetFun (f, x, ty_fun, body, term) -> (
      match ty_fun with
      | TFun (ty_in, ty_out) -> (
          let env' = env |> VarEnv.extend f ty_fun |> VarEnv.extend x ty_in in
          match rec_type_check env' body with
          | Left ty_out' when ty_out = ty_out' ->
              rec_type_check (VarEnv.extend f ty_fun env) term
          | Left ty_out' ->
              Right
                (Printf.sprintf "Function body has type %s, but expected %s"
                   (string_of_type ty_out') (string_of_type ty_out))
          | Right msg -> Right msg)
      | _ -> Right "Function type annotation must be of the form ty1 -> ty2")

let type_check = rec_type_check VarEnv.empty

open MiniTyFunParser
open MiniTyFunAST

let string_of_token = function
  | NAME x -> "NAME " ^ x
  | INT n -> "INT " ^ string_of_int n
  | BOOL b -> "BOOL " ^ string_of_bool b
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | EQUAL -> "EQUAL"
  | LESS -> "LESS"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | LET -> "LET"
  | LETFUN -> "LETFUN"
  | ASSIGN -> "ASSIGN"
  | IN -> "IN"
  | COLON -> "COLON"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | FUN -> "FUN"
  | ARROW -> "ARROW"
  | TINT -> "TINT"
  | TBOOL -> "TBOOL"
  | TFUN -> "TFUN"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EOF -> "EOF"

let rec string_of_ast = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var x -> x
  | Fun (x, ty, body) ->
      "fun " ^ x ^ " : " ^ string_of_type ty ^ " -> " ^ string_of_ast body
  | App (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " " ^ string_of_ast term2 ^ ")"
  | Add (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " + " ^ string_of_ast term2 ^ ")"
  | Sub (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " - " ^ string_of_ast term2 ^ ")"
  | Mul (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " * " ^ string_of_ast term2 ^ ")"
  | Equal (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " == " ^ string_of_ast term2 ^ ")"
  | LessThan (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " < " ^ string_of_ast term2 ^ ")"
  | And (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " and " ^ string_of_ast term2 ^ ")"
  | Or (term1, term2) ->
      "(" ^ string_of_ast term1 ^ " or " ^ string_of_ast term2 ^ ")"
  | Not term -> "(not" ^ string_of_ast term ^ ")"
  | IfThenElse (cond, term1, term2) ->
      "if " ^ string_of_ast cond ^ " then " ^ string_of_ast term1 ^ " else "
      ^ string_of_ast term2
  | Let (x, term1, term2) ->
      "let " ^ x ^ " = " ^ string_of_ast term1 ^ " in " ^ string_of_ast term2
  | LetFun (f, x, ty, body, term) ->
      "letfun " ^ f ^ " " ^ x ^ " : " ^ string_of_type ty ^ " = "
      ^ string_of_ast body ^ " in " ^ string_of_ast term

and string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (term1, term2) ->
      "(" ^ string_of_type term1 ^ " -> " ^ string_of_type term2 ^ ")"

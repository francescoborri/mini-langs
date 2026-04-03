open Utils
open MiniImpParser
open MiniImpAST
open MiniImpCFG

let string_of_token = function
  | INT n -> Printf.sprintf "INT %d" n
  | BOOL b -> Printf.sprintf "BOOL %b" b
  | NAME x -> Printf.sprintf "NAME %s" x
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | LESS -> "LESS"
  | AND -> "AND"
  | NOT -> "NOT"
  | ASSIGN -> "ASSIGN"
  | SEQ -> "SEQ"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | SKIP -> "SKIP"
  | MAIN (input, output) -> Printf.sprintf "MAIN %s %s" input output
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EOF -> "EOF"

let rec string_of_cmd indent_level cmd =
  let indent = String.make (indent_level * 2) ' ' in
  match cmd with
  | Skip -> indent ^ "skip"
  | Assign (var, aexpr) ->
      Printf.sprintf "%s%s := %s" indent var (string_of_aexpr aexpr)
  | Seq (cmd1, cmd2) ->
      Printf.sprintf "%s;\n%s"
        (string_of_cmd indent_level cmd1)
        (string_of_cmd indent_level cmd2)
  | IfThenElse (bexpr, then_cmd, else_cmd) ->
      Printf.sprintf "%sif %s then (\n%s\n%s) else (\n%s\n%s)" indent
        (string_of_bexpr bexpr)
        (string_of_cmd (indent_level + 1) then_cmd)
        indent
        (string_of_cmd (indent_level + 1) else_cmd)
        indent
  | While (bexpr, body_cmd) ->
      Printf.sprintf "%swhile %s do (\n%s\n%s)" indent (string_of_bexpr bexpr)
        (string_of_cmd (indent_level + 1) body_cmd)
        indent

and string_of_aexpr = function
  | Int n -> string_of_int n
  | Var x -> x
  | Add (aexpr1, aexpr2) ->
      Printf.sprintf "(%s + %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)
  | Sub (aexpr1, aexpr2) ->
      Printf.sprintf "(%s - %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)
  | Mul (aexpr1, aexpr2) ->
      Printf.sprintf "(%s * %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)

and string_of_bexpr = function
  | Bool b -> string_of_bool b
  | LessThan (aexpr1, aexpr2) ->
      Printf.sprintf "(%s < %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)
  | And (bexpr1, bexpr2) ->
      Printf.sprintf "(%s and %s)" (string_of_bexpr bexpr1)
        (string_of_bexpr bexpr2)
  | Not bexpr -> Printf.sprintf "(not %s)" (string_of_bexpr bexpr)

let string_of_expr = function
  | Aexpr aexpr -> string_of_aexpr aexpr
  | Bexpr bexpr -> string_of_bexpr bexpr

let string_of_ast (Prog (input, output, cmd)) =
  Printf.sprintf "def main with input %s output %s as\n%s" input output
    (string_of_cmd 1 cmd)

let string_of_simple_cmd = function
  | SSkip -> "skip"
  | SAssign (x, aexpr) -> Printf.sprintf "%s := %s" x (string_of_aexpr aexpr)
  | SGuard bexpr -> Printf.sprintf "%s?" (string_of_bexpr bexpr)

let dot_string_of_cfg =
  GenericCFG.dot_string_of_cfg
    (List.map string_of_simple_cmd >> String.concat "\n")
    id
    (fun _ -> None)
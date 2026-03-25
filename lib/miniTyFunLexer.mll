{
  open MiniTyFunParser

  exception LexicalError of string
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let name = letter (letter | digit | '_')*
let number = digit+
let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"

rule read = parse
  | whitespace { read lexbuf }
  | number as n { INT (int_of_string n) }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LESS }
  | "==" { EQUAL }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "let" { LET }
  | "letfun" { LETFUN }
  | '=' { ASSIGN }
  | "in" { IN }
  | ':' { COLON }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "=>" { ARROW }
  | "int" { TINT }
  | "bool" { TBOOL }
  | "->" { TFUN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | name as x { NAME x }
  | _ as symbol { raise (LexicalError ("Unexpected symbol " ^ String.make 1 symbol)) }

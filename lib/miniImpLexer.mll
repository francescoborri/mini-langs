{
  open MiniImpParser

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
  | "and" { AND }
  | "not" { NOT }
  | ":=" { ASSIGN }
  | ';' { SEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "skip" { SKIP }
  | "def main with input " (name as input) " output " (name as output) " as" { MAIN (input, output) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | name as x { NAME x }
  | _ as symbol { raise (LexicalError ("Unexpected symbol " ^ String.make 1 symbol)) }

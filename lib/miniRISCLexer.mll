{
  open MiniRISCParser
  open MiniRISCInstructionSet

  exception LexicalError of string
}

let digit = ['0'-'9']
let number = '-'? digit+
let whitespace = [' ' '\t']+ | '\r' | '\n' | "\r\n"

rule read = parse
  | whitespace { read lexbuf }
  | "in" { REG RegIn }
  | "out" { REG RegOut }
  | "ra" | "rA" { REG RegA }
  | "rb" | "rB" { REG RegB }
  | 'r' (digit+ as reg_id) { REG (Reg (int_of_string reg_id)) }
  | "main" { LABEL MainLabel }
  | ['l' 'L'] (digit+ as label_id) { LABEL (Label (int_of_string label_id)) }
  | number as n { INT (int_of_string n) }
  | "nop" { NOP }
  | "add" { ADD }
  | "sub" { SUB }
  | "mult" { MULT }
  | "and" { AND }
  | "less" { LESS }
  | "addI" { ADDI }
  | "subI" { SUBI }
  | "multI" { MULTI }
  | "andI" { ANDI }
  | "not" { NOT }
  | "copy" { COPY }
  | "load" { LOAD }
  | "loadI" { LOADI }
  | "store" { STORE }
  | "jump" { JUMP }
  | "cjump" { CJUMP }
  | "=>" { ARROW }
  | ':' { COLON }
  | eof { EOF }
  | _ as symbol { raise (LexicalError ("Unexpected symbol " ^ String.make 1 symbol)) }

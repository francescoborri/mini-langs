%{
  open MiniImpAST
%}

%token <string> NAME
%token <int> INT
%token <bool> BOOL
%token <string * string> MAIN
%token PLUS MINUS TIMES LESS AND NOT
%token ASSIGN SEQ IF THEN ELSE WHILE DO SKIP
%token LPAREN RPAREN EOF

%left SEQ
%nonassoc ELSE DO
%left AND
%nonassoc NOT
%left PLUS MINUS
%left TIMES

%start <ast> prog

%%

let prog := (input, output) = MAIN ; cmd = cmd ; EOF ; < Prog >

let int_atom :=
  | ~ = INT ; <>
  | MINUS ; n = int_atom ; { -n }

let aexpr :=
  | ~ = int_atom ; < Int >
  | var = NAME ; < Var >
  | expr1 = aexpr ; PLUS ; expr2 = aexpr ; < Add >
  | expr1 = aexpr ; MINUS ; expr2 = aexpr ; < Sub >
  | expr1 = aexpr ; TIMES ; expr2 = aexpr ; < Mul >
  | LPAREN ; ~ = aexpr ; RPAREN ; <>

let bexpr :=
  | bool = BOOL ; < Bool >
  | aexpr1 = aexpr ; LESS ; aexpr2 = aexpr ; < LessThan >
  | bexpr1 = bexpr ; AND ; bexpr2 = bexpr ; < And >
  | NOT ; ~ = bexpr ; < Not >
  | LPAREN ; ~ = bexpr ; RPAREN ; <>

let cmd :=
  | SKIP ; { Skip }
  | var = NAME ; ASSIGN ; expr = aexpr ; < Assign >
  | cmd1 = cmd ; SEQ ; cmd2 = cmd ; < Seq >
  | IF ; bexpr = bexpr ; THEN ; cmd1 = cmd ; ELSE ; cmd2 = cmd ; < IfThenElse >
  | WHILE ; bexpr = bexpr ; DO ; cmd = cmd ; < While >
  | LPAREN ; ~ = cmd ; RPAREN ; <>

%%
%{
  open MiniTyFunAST
%}

%token <string> NAME
%token <int> INT
%token <bool> BOOL
%token PLUS MINUS TIMES EQUAL LESS AND OR NOT
%token LET LETFUN ASSIGN IN COLON IF THEN ELSE FUN ARROW
%token TINT TBOOL TFUN
%token LPAREN RPAREN EOF

%left OR
%left AND
%nonassoc NOT
%left EQUAL LESS
%left PLUS MINUS
%left TIMES
%nonassoc APP

%right TFUN

%start <ast> prog

%%

let prog := ~ = term ; EOF ; <>

let term :=
  | FUN ; arg = NAME ; COLON ; ty = __type ; ARROW ; term = term ; < Fun >
  | IF ; cond = term ; THEN ; term1 = term ; ELSE ; term2 = term ; < IfThenElse >
  | LET ; var = NAME ; ASSIGN ; term1 = term ; IN ; term2 = term ; < Let >
  | LETFUN ; fname = NAME ; arg = NAME ; COLON ; ty = __type ; ASSIGN ; term1 = term ; IN ; term2 = term ; < LetFun >
  | ~ = op_term ; <>

let op_term :=
  | term1 = op_term ; PLUS ; term2 = op_term ; < Add >
  | term1 = op_term ; MINUS ; term2 = op_term ; < Sub >
  | term1 = op_term ; TIMES ; term2 = op_term ; < Mul >
  | term1 = op_term ; EQUAL ; term2 = op_term ; < Equal >
  | term1 = op_term ; LESS ; term2 = op_term ; < LessThan >
  | term1 = op_term ; AND ; term2 = op_term ; < And >
  | term1 = op_term ; OR ; term2 = op_term ; < Or >
  | NOT ; ~ = op_term ; < Not >
  | ~ = app_term ; <> %prec APP

let app_term :=
  | term1 = app_term ; term2 = atom ; < App >
  | ~ = atom ; <>

let atom :=
  | atom = opt_minus_atom ; <>
  | bool = BOOL ; < Bool >

let opt_minus_atom :=
  | int = INT ; < Int >
  | var = NAME ; < Var >
  | LPAREN ; ~ = term ; RPAREN ; <>
  | MINUS ; atom = opt_minus_atom ; { Sub (Int 0, atom) }

let __type :=
  | TINT ; { TInt }
  | TBOOL ; { TBool }
  | ty1 = __type ; TFUN ; ty2 = __type ; < TFun >
  | LPAREN ; ~ = __type ; RPAREN ; <>

%%
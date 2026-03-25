%{
  open MiniRISCInstructionSet
%}

%token <int> INT
%token <MiniRISCInstructionSet.reg> REG
%token <MiniRISCInstructionSet.label> LABEL
%token NOP
%token ADD SUB MULT AND LESS
%token ADDI SUBI MULTI ANDI
%token NOT COPY
%token LOAD LOADI STORE JUMP CJUMP
%token ARROW COLON
%token EOF

%start <ast> prog

%%

let prog := ~ = labelled_instr ; ~ = instrs ; { labelled_instr :: instrs }

let instrs :=
  | ~ = instr ; ~ = instrs ; { (instr, None) :: instrs }
  | ~ = labelled_instr ; ~ = instrs ; { labelled_instr :: instrs }
  | EOF ; { [] }

let labelled_instr := label = LABEL ; COLON ; instr = instr ; { (instr, Some label) }

let instr :=
  | NOP ; { Nop }
  | op = brop ; src1 = REG ; src2 = REG ; ARROW ; dest = REG ; < BinRegOp >
  | op = biop ; src = REG ; imm = INT ; ARROW ; dest = REG ; < BinImmOp >
  | op = urop ; src = REG ; ARROW ; dest = REG ; < UnOp >
  | LOAD ; src = REG ; ARROW ; dest = REG ; < Load >
  | LOADI ; imm = INT ; ARROW ; dest = REG ; < LoadImm >
  | STORE ; src = REG ; ARROW ; dest = REG ; < Store >
  | JUMP ; dest = LABEL ; < Jump >
  | CJUMP ; src = REG ; then_label = LABEL ; else_label = LABEL ; < CondJump >

let brop :=
  | ADD ; { Add }
  | SUB ; { Sub }
  | MULT ; { Mul }
  | AND ; { And }
  | LESS ; { Less }

let biop :=
  | ADDI ; { AddImm }
  | SUBI ; { SubImm }
  | MULTI ; { MulImm }
  | ANDI ; { AndImm }

let urop :=
  | NOT ; { Not }
  | COPY ; { Copy }

%%
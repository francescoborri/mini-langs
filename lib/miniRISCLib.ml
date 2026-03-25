open MiniRISCInstructionSet
open MiniRISCLexer
open MiniRISCParser
open MiniRISCInterpreter
open MiniRISCCFG
open MiniRISCLiveRegistersAnalysis
open MiniRISCRegisterAllocation
open MiniRISCRegUtils
open MiniRISCUtils
module InstructionSet = MiniRISCInstructionSet
module Lexer = MiniRISCLexer
module Parser = MiniRISCParser
module Interpreter = MiniRISCInterpreter
module CFG = MiniRISCCFG
module LiveRegistersAnalysis = MiniRISCLiveRegistersAnalysis
module RegisterAllocation = MiniRISCRegisterAllocation
module RegUtils = MiniRISCRegUtils
module Utils = MiniRISCUtils

let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let run = Interpreter.run
let translate_cfg = CFG.translate_cfg
let add_jump_instrs = CFG.add_jump_instrs

let optimized_register_allocation =
  RegisterAllocation.optimized_register_allocation

let unoptimized_register_allocation =
  RegisterAllocation.unoptimized_register_allocation

let compile dest cfg =
  let target_code = cfg |> Utils.generate_target_code in
  open_out dest |> fun out ->
  output_string out target_code;
  close_out out

open GenericCFG
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
let standard_reg_alloc = RegisterAllocation.standard_reg_alloc
let optimized_reg_alloc = RegisterAllocation.optimized_reg_alloc

let generate_target_code cfg =
  cfg.code_map |> NodeMap.to_list
  |> List.map (fun (node, instrs) ->
      let label_str = node |> Utils.label_of_node cfg |> string_of_label in
      let instrs_str =
        instrs
        |> List.map string_of_instruction
        |> List.map (( ^ ) "  ")
        |> String.concat "\n"
      in
      Printf.sprintf "%s:\n%s" label_str instrs_str)
  |> String.concat "\n"

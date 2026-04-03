open GenericCFG
open Utils
open MiniRISCInstructionSet
open MiniRISCParser
open MiniRISCLiveRegistersAnalysis
open MiniRISCRegUtils

let string_of_reg = function
  | Reg n -> Printf.sprintf "r%d" n
  | RegA -> "ra"
  | RegB -> "rb"
  | RegIn -> "in"
  | RegOut -> "out"

let string_of_label = function
  | MainLabel -> "main"
  | Label n -> Printf.sprintf "l%d" n

let string_of_brop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mult"
  | And -> "and"
  | Less -> "less"

let string_of_biop = function
  | AddImm -> "addI"
  | SubImm -> "subI"
  | MulImm -> "multI"
  | AndImm -> "andI"

let string_of_uop = function Not -> "not" | Copy -> "copy"

let string_of_instruction = function
  | Nop -> "nop"
  | BinRegOp (brop, src_reg1, src_reg2, dest_reg) ->
      Printf.sprintf "%s %s %s => %s" (string_of_brop brop)
        (string_of_reg src_reg1) (string_of_reg src_reg2)
        (string_of_reg dest_reg)
  | BinImmOp (biop, src_reg, imm, dest_reg) ->
      Printf.sprintf "%s %s %d => %s" (string_of_biop biop)
        (string_of_reg src_reg) imm (string_of_reg dest_reg)
  | UnOp (uop, src_reg, dest_reg) ->
      Printf.sprintf "%s %s => %s" (string_of_uop uop) (string_of_reg src_reg)
        (string_of_reg dest_reg)
  | Load (src_reg, dest_reg) ->
      Printf.sprintf "load %s => %s" (string_of_reg src_reg)
        (string_of_reg dest_reg)
  | LoadImm (imm, dest_reg) ->
      Printf.sprintf "loadI %d => %s" imm (string_of_reg dest_reg)
  | Store (src_reg, dest_reg) ->
      Printf.sprintf "store %s => %s" (string_of_reg src_reg)
        (string_of_reg dest_reg)
  | Jump label -> Printf.sprintf "jump %s" (string_of_label label)
  | CondJump (cond_reg, label_true, label_false) ->
      Printf.sprintf "cjump %s %s %s" (string_of_reg cond_reg)
        (string_of_label label_true)
        (string_of_label label_false)

let label_of_node cfg node =
  if node = cfg.initial_node then MainLabel
  else
    let (Node node_id) = node in
    Label node_id

let dot_string_of_cfg cfg =
  { cfg with code_map = NodeMap.map fst cfg.code_map }
  |> GenericCFG.dot_string_of_cfg
       (List.map string_of_instruction >> String.concat "\n")
       string_of_reg
       (label_of_node cfg >> string_of_label >> Option.some)

let dot_string_of_cfg_with_jumps cfg =
  GenericCFG.dot_string_of_cfg
    (List.map string_of_instruction >> String.concat "\n")
    string_of_reg
    (label_of_node cfg >> string_of_label >> Option.some)
    cfg

let string_of_live_regs cfg analysis_state =
  analysis_state
  |> NodeMap.mapi (fun node { in_regs; out_regs } ->
      let label = label_of_node cfg node in
      Printf.sprintf "%s:\n  - Live in: { %s }\n  - Live out: { %s }" (string_of_label label)
        (in_regs |> RegSet.to_list |> List.map string_of_reg
       |> String.concat ", ")
        (out_regs |> RegSet.to_list |> List.map string_of_reg
       |> String.concat ", "))
  |> NodeMap.to_list |> List.map snd |> String.concat "\n"

let string_of_spilled_regs =
  RegSet.elements >> List.map string_of_reg >> String.concat ", "
  >> Printf.sprintf "Spilled registers: { %s }"

let string_of_reg_coloring =
  RegMap.bindings
  >> List.map (fun (virtual_reg, physical_reg) ->
      Printf.sprintf "  - %s -> %s"
        (string_of_reg virtual_reg)
        (string_of_reg physical_reg))
  >> String.concat "\n"
  >> Printf.sprintf "Register coloring:\n%s"

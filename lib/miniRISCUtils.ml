open GenericCFG
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

let string_of_cfg =
  GenericCFG.string_of_cfg
    (fun (instrs, opt_reg) ->
      Printf.sprintf "%s%s"
        (instrs |> List.map string_of_instruction |> String.concat "\n")
        (Option.fold ~none:""
           ~some:(fun reg ->
             Printf.sprintf "\nGuard register: %s" (string_of_reg reg))
           opt_reg))
    string_of_reg

let string_of_cfg_with_jumps =
  GenericCFG.string_of_cfg
    (fun instrs ->
      instrs |> List.map string_of_instruction |> String.concat "\n")
    string_of_reg

let string_of_live_regs cfg analysis_state =
  analysis_state
  |> NodeMap.mapi (fun node { in_regs; out_regs } ->
      let label =
        if node = cfg.initial_node then MainLabel
        else
          Label
            (let (Node id) = node in
             id)
      in
      Printf.sprintf "%s:\n  In: {%s}\n  Out: {%s}" (string_of_label label)
        (in_regs |> RegSet.to_list |> List.map string_of_reg
       |> String.concat ", ")
        (out_regs |> RegSet.to_list |> List.map string_of_reg
       |> String.concat ", "))
  |> NodeMap.to_list |> List.map snd |> String.concat "\n"

let generate_target_code cfg =
  cfg.code_map |> NodeMap.to_list
  |> List.map (fun (node, instrs) ->
      let (Node node_id) = node in
      let label =
        if node = cfg.initial_node then MainLabel else Label node_id
      in
      Printf.sprintf "%s:\n%s" (string_of_label label)
        (String.concat "\n"
           (List.map (fun instr -> "  " ^ string_of_instruction instr) instrs)))
  |> String.concat "\n"

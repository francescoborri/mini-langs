open MiniRISCInstructionSet
open MiniRISCUtils

exception RuntimeError of string

module LabelMem = struct
  module Map = Map.Make (struct
    type t = label

    let compare = compare
  end)

  type t = instruction list Map.t

  let add = Map.add

  let find label map =
    match Map.find_opt label map with
    | Some instrs -> instrs
    | None -> raise (RuntimeError ("Undefined label: " ^ string_of_label label))

  let empty = Map.empty
end

module RegMem = struct
  module RegMap = Map.Make (struct
    type t = reg

    let compare = compare
  end)

  type t = { mem : int RegMap.t; num_regs : int }

  let add reg value { mem; num_regs } =
    let mem' = RegMap.add reg value mem in
    if RegMap.cardinal mem' <= num_regs then { mem = mem'; num_regs }
    else raise (RuntimeError "Exceeded maximum number of registers")

  let find reg { mem; _ } =
    match RegMap.find_opt reg mem with
    | Some value -> value
    | None -> raise (RuntimeError ("Unbound register: " ^ string_of_reg reg))

  let empty num_regs = { mem = RegMap.empty; num_regs }
end

module Mem = struct
  module MemMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type t = int MemMap.t

  let add = MemMap.add

  let find addr mem =
    match MemMap.find_opt addr mem with
    | Some value -> value
    | None ->
        raise (RuntimeError ("Unbound memory address: " ^ string_of_int addr))

  let empty = MemMap.empty
end

let build_label_instrs_map ast =
  let flush_block map current_label current_instrs_rev =
    match current_label with
    | None -> map
    | Some label -> LabelMem.add label (List.rev current_instrs_rev) map
  in
  let rec helper map current_label current_instrs_rev = function
    | [] -> flush_block map current_label current_instrs_rev
    | (instr, Some next_label) :: instrs ->
        let map = flush_block map current_label current_instrs_rev in
        helper map (Some next_label) [ instr ] instrs
    | (instr, None) :: instrs ->
        helper map current_label (instr :: current_instrs_rev) instrs
  in
  helper LabelMem.empty None [] ast

let eval_brop brop val1 val2 =
  match brop with
  | Add -> val1 + val2
  | Sub -> val1 - val2
  | Mul -> val1 * val2
  | And when (val1 = 0 || val1 = 1) && (val2 = 0 || val2 = 1) ->
      if val1 = 1 && val2 = 1 then 1 else 0
  | Less -> if val1 < val2 then 1 else 0
  | And -> raise (RuntimeError "AND operation requires boolean values (0 or 1)")

let eval_biop biop val1 imm =
  match biop with
  | AddImm -> val1 + imm
  | SubImm -> val1 - imm
  | MulImm -> val1 * imm
  | AndImm when val1 = 0 || (val1 = 1 && (imm = 0 || imm = 1)) ->
      if val1 = 1 && imm = 1 then 1 else 0
  | AndImm ->
      raise (RuntimeError "AND operation requires boolean values (0 or 1)")

let eval_unop uop val1 =
  match uop with
  | Not when val1 = 0 || val1 = 1 -> if val1 = 0 then 1 else 0
  | Copy -> val1
  | Not -> raise (RuntimeError "NOT operation requires boolean values (0 or 1)")

let eval_instr reg_mem mem instr =
  match instr with
  | Nop -> (reg_mem, mem, None)
  | BinRegOp (brop, src_reg1, src_reg2, dest_reg) ->
      let value1 = RegMem.find src_reg1 reg_mem in
      let value2 = RegMem.find src_reg2 reg_mem in
      let result = eval_brop brop value1 value2 in
      (RegMem.add dest_reg result reg_mem, mem, None)
  | BinImmOp (biop, src_reg, imm, dest_reg) ->
      let value = RegMem.find src_reg reg_mem in
      let result = eval_biop biop value imm in
      (RegMem.add dest_reg result reg_mem, mem, None)
  | UnOp (uop, src_reg, dest_reg) ->
      let value = RegMem.find src_reg reg_mem in
      let result = eval_unop uop value in
      (RegMem.add dest_reg result reg_mem, mem, None)
  | Load (src_reg, dest_reg) ->
      let addr = RegMem.find src_reg reg_mem in
      let value = Mem.find addr mem in
      (RegMem.add dest_reg value reg_mem, mem, None)
  | LoadImm (imm, dest_reg) -> (RegMem.add dest_reg imm reg_mem, mem, None)
  | Store (src_reg, dest_reg) ->
      let addr = RegMem.find dest_reg reg_mem in
      let value = RegMem.find src_reg reg_mem in
      (reg_mem, Mem.add addr value mem, None)
  | Jump label -> (reg_mem, mem, Some label)
  | CondJump (cond_reg, label_true, label_false) ->
      let guard = RegMem.find cond_reg reg_mem in
      if guard <> 0 && guard <> 1 then
        raise (RuntimeError "Guard register must hold a boolean value (0 or 1)");
      let next_label = if guard <> 0 then label_true else label_false in
      (reg_mem, mem, Some next_label)

let eval_instrs label_instrs_map reg_mem mem instrs =
  let rec helper reg_mem mem = function
    | [] -> (reg_mem, mem, None)
    | instr :: instrs -> (
        let reg_mem', mem', next_label = eval_instr reg_mem mem instr in
        match next_label with
        | None -> helper reg_mem' mem' instrs
        | Some next_label ->
            let next_instrs = LabelMem.find next_label label_instrs_map in
            helper reg_mem' mem' next_instrs)
  in
  helper reg_mem mem instrs

let eval_prog label_instrs_map reg_mem mem =
  let instrs = LabelMem.find MainLabel label_instrs_map in
  eval_instrs label_instrs_map reg_mem mem instrs

let run prog num_regs input =
  let label_instrs_map = build_label_instrs_map prog in
  let reg_mem = RegMem.empty num_regs |> RegMem.add RegIn input in
  let mem = Mem.empty in
  let reg_mem, mem, _ = eval_prog label_instrs_map reg_mem mem in
  RegMem.find RegOut reg_mem

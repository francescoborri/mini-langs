open GenericCFG
open MiniRISCCFG
open MiniRISCInstructionSet
open MiniRISCRegUtils
open Utils

type local_regs_info = { used_regs : RegSet.t; defined_regs : RegSet.t }
(** Type for storing register information at each node. [used_regs] is the set
    of registers used by any instruction in the node and not defined before that
    instructions, whereas [defined_regs] is just the set of registers defined by
    the node *)

type global_regs_info = local_regs_info NodeMap.t
(** Type for storing register information for the whole CFG. Maps each node to
    its corresponding [local_regs_info] *)

type analysis_result = RegSet.t DataflowAnalysis.global_state

let collect_local_info =
  List.fold_left
    (fun { used_regs; defined_regs } instr ->
      let used_regs =
        RegSet.union used_regs
          (RegSet.filter
             (fun reg -> RegSet.mem reg defined_regs |> not)
             (using_regs instr))
      in
      let defined_regs =
        Option.fold ~none:defined_regs
          ~some:(fun reg -> RegSet.add reg defined_regs)
          (defining_reg instr)
      in
      { used_regs; defined_regs })
    { used_regs = RegSet.empty; defined_regs = RegSet.empty }

(** Returns the registers information for the entire CFG *)
let collect_global_info cfg = NodeMap.map collect_local_info cfg.code_map

module LiveRegistersAnalysis = DataflowAnalysis.Make (struct
  type t = reg
  type cmd = instruction list
  type aux = global_regs_info

  module Set = RegSet

  let direction = DataflowAnalysis.Backward
  let modality = DataflowAnalysis.May
  let gen (_, regs_info) node = (NodeMap.find node regs_info).used_regs
  let kill (_, regs_info) node = (NodeMap.find node regs_info).defined_regs
  let initial_node cfg = cfg.final_node
  let initial_node_state cfg = RegSet.singleton cfg.output
  let compute_initial_state _ = RegSet.empty
  let compute_aux_info cfg = collect_global_info cfg
end)

let analyze cfg = LiveRegistersAnalysis.worklist_algorithm cfg

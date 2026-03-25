open GenericCFG
open MiniRISCInstructionSet
open MiniRISCCFG
open MiniRISCRegUtils

type node_analysis_state = { in_regs : RegSet.t; out_regs : RegSet.t }
type cfg_analysis_state = node_analysis_state NodeMap.t

val live_regs_analysis : cfg_with_jumps -> cfg_analysis_state

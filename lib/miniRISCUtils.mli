open MiniRISCInstructionSet
open MiniRISCParser
open MiniRISCCFG
open MiniRISCLiveRegistersAnalysis
open MiniRISCRegUtils

val string_of_reg : reg -> string
val string_of_label : label -> string
val string_of_instruction : instruction -> string
val label_of_node : ('a, reg) GenericCFG.cfg -> GenericCFG.node -> label
val dot_string_of_cfg : cfg -> string
val dot_string_of_cfg_with_jumps : cfg_with_jumps -> string
val string_of_live_regs : cfg_with_jumps -> cfg_analysis_state -> string
val string_of_spilled_regs : RegSet.t -> string
val string_of_reg_coloring : reg RegMap.t -> string

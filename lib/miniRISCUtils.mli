open MiniRISCInstructionSet
open MiniRISCParser
open MiniRISCCFG
open MiniRISCLiveRegistersAnalysis

val string_of_reg : reg -> string
val string_of_label : label -> string
val string_of_instruction : instruction -> string
val string_of_cfg : cfg -> string
val string_of_cfg_with_jumps : cfg_with_jumps -> string
val string_of_live_regs : cfg_with_jumps -> cfg_analysis_state -> string
val generate_target_code : cfg_with_jumps -> string
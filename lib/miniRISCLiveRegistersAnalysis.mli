open GenericCFG
open MiniRISCInstructionSet
open MiniRISCCFG
open MiniRISCRegUtils

type analysis_result = RegSet.t DataflowAnalysis.global_state

val analyze : cfg_with_jumps -> analysis_result

open MiniRISCInstructionSet
open MiniRISCCFG
open MiniRISCRegUtils

exception RegisterAllocationError of string

val standard_reg_alloc : int -> cfg_with_jumps -> cfg_with_jumps * RegSet.t * reg RegMap.t
val optimized_reg_alloc : int -> cfg_with_jumps -> cfg_with_jumps * RegSet.t * reg RegMap.t
open MiniRISCInstructionSet
open MiniRISCCFG
open MiniRISCRegUtils

exception RegisterAllocationError of string

val optimized_register_allocation : int -> cfg_with_jumps -> cfg_with_jumps * RegSet.t * reg RegMap.t
val unoptimized_register_allocation : int -> cfg_with_jumps -> cfg_with_jumps * RegSet.t * reg RegMap.t
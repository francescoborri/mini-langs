open MiniRISCInstructionSet

type cfg = (instruction list * reg option, reg) GenericCFG.cfg
type cfg_with_jumps = (instruction list, reg) GenericCFG.cfg

val translate_cfg : MiniImpLib.CFG.cfg -> cfg
val add_jump_instrs : cfg -> cfg_with_jumps

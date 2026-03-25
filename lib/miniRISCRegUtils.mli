open Var
open MiniRISCInstructionSet
module RegSet : Set.S with type elt = reg
module RegMap : Map.S with type key = reg

module VarRegMap : sig
  type t = { inner_map : reg VarMap.t; last_reg : int }

  val empty : t
  val lookup : var -> t -> reg * t
  val fresh_reg : t -> reg * t
  val initialize : var -> var -> t
end

val using_regs : instruction -> RegSet.t
val defining_reg : instruction -> reg option

open GenericCFG
open Utils

type modality = May | Must
type direction = Forward | Backward
type 'a local_state = { in_values : 'a; out_values : 'a }
type 'a global_state = 'a local_state NodeMap.t

module type S = sig
  type t
  type cmd
  type aux

  module Set : Set.S with type elt = t

  val modality : modality
  val direction : direction
  val gen : (cmd, t) cfg * aux -> node -> Set.t
  val kill : (cmd, t) cfg * aux -> node -> Set.t
  val initial_node : (cmd, t) cfg -> node
  val initial_node_state : (cmd, t) cfg -> Set.t
  val compute_initial_state : (cmd, t) cfg * aux -> Set.t
  val compute_aux_info : (cmd, t) cfg -> aux
end

module Make (S : S) : sig
  val worklist_algorithm : (S.cmd, S.t) cfg -> S.Set.t global_state
end

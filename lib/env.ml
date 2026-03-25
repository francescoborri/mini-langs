(** Environment module for managing generic bindings. *)
module type S = sig
  type key

  module EnvMap : Map.S with type key = key

  type 'a t = 'a EnvMap.t

  val empty : 'a t
  val extend : key -> 'a -> 'a t -> 'a t
  val lookup : key -> 'a t -> 'a option
end

(** Functor to create an environment module given a specific key type. *)
module Make (Var : Map.OrderedType) : S with type key = Var.t = struct
  type key = Var.t

  module EnvMap = Map.Make (Var)

  type 'a t = 'a EnvMap.t

  let empty = EnvMap.empty
  let extend name value t = EnvMap.add name value t
  let lookup name t = EnvMap.find_opt name t
end

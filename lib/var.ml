open Env

type var = string

module VarEnv = Make (struct
  type t = var

  let compare = compare
end)

module VarSet = Set.Make (struct
  type t = var

  let compare = compare
end)

module VarMap = Map.Make (struct
  type t = var

  let compare = compare
end)

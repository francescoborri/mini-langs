open Var
open MiniRISCInstructionSet

module RegSet = Set.Make (struct
  type t = reg

  let compare = compare
end)

module RegMap = Map.Make (struct
  type t = reg

  let compare = compare
end)

(** Module for handling variables and registers during the translation from
    miniImp to miniRISC. [lookup] is used to get the register corresponding to a
    variable, or to create a new one if the variable is not already mapped.
    [fresh_reg] is used to get a new register, which is useful when translating
    expression that need to store intermediate results. The mapping from
    variables to registers is stored in the [inner_map] field, while the
    [last_reg] field is used to keep track of the last register used, so that we
    can generate new registers by incrementing it. The [initialize] function is
    used to create a new map with the input and output variables mapped to the
    special in and out registers *)
module VarRegMap = struct
  type t = { inner_map : reg VarMap.t; last_reg : int }

  let empty = { inner_map = VarMap.empty; last_reg = -1 }

  let lookup var map =
    match VarMap.find_opt var map.inner_map with
    | Some reg -> (reg, map)
    | None ->
        let new_reg = Reg (map.last_reg + 1) in
        ( new_reg,
          {
            inner_map = VarMap.add var new_reg map.inner_map;
            last_reg = map.last_reg + 1;
          } )

  let fresh_reg map =
    let new_reg = Reg (map.last_reg + 1) in
    (new_reg, { map with last_reg = map.last_reg + 1 })

  let initialize input output =
    {
      empty with
      inner_map =
        VarMap.empty |> VarMap.add input RegIn |> VarMap.add output RegOut;
    }
end

(** Returns the set of registers used by an instruction *)
let using_regs = function
  | Nop | LoadImm _ | Jump _ -> RegSet.empty
  | BinRegOp (_, reg1, reg2, _) | Store (reg1, reg2) ->
      RegSet.of_list [ reg1; reg2 ]
  | BinImmOp (_, reg, _, _)
  | UnOp (_, reg, _)
  | Load (reg, _)
  | CondJump (reg, _, _) ->
      RegSet.singleton reg

(** Returns the register defined by an instruction, if any *)
let defining_reg = function
  | Nop | Store _ | Jump _ | CondJump _ -> None
  | BinRegOp (_, _, _, reg)
  | BinImmOp (_, _, _, reg)
  | UnOp (_, _, reg)
  | Load (_, reg)
  | LoadImm (_, reg) ->
      Some reg

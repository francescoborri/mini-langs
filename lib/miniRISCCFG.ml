open GenericCFG
open Var
open MiniRISCInstructionSet
open MiniRISCRegUtils
open MiniImpLib.AST
open MiniImpLib.CFG

type cfg = (instruction list * reg option, reg) GenericCFG.cfg
(** CFG representation for miniImp. Each node is associated to a list of
    [instruction] and an optional [reg], which, if it is present, means that the
    node has a final implicit branch instruction conditioned on that register.
    The input/output entities of the CFG are of type [reg]. *)

type cfg_with_jumps = (instruction list, reg) GenericCFG.cfg
(** Same as [cfg], but jump instructions are explicitly written in every node*)

let rec pre_map_vars_aexpr aexpr map =
  match aexpr with
  | Int _ -> map
  | Var x -> VarRegMap.lookup x map |> snd
  | Add (aexpr1, aexpr2) | Sub (aexpr1, aexpr2) | Mul (aexpr1, aexpr2) ->
      map |> pre_map_vars_aexpr aexpr1 |> pre_map_vars_aexpr aexpr2

let rec pre_map_vars_bexpr bexpr map =
  match bexpr with
  | Bool _ -> map
  | And (bexpr1, bexpr2) ->
      map |> pre_map_vars_bexpr bexpr1 |> pre_map_vars_bexpr bexpr2
  | Not bexpr -> pre_map_vars_bexpr bexpr map
  | LessThan (aexpr1, aexpr2) ->
      map |> pre_map_vars_aexpr aexpr1 |> pre_map_vars_aexpr aexpr2

let pre_map_vars_simple_cmd simple_cmd map =
  match simple_cmd with
  | SSkip -> map
  | SAssign (x, aexpr) ->
      map |> VarRegMap.lookup x |> snd |> pre_map_vars_aexpr aexpr
  | SGuard bexpr -> map |> pre_map_vars_bexpr bexpr

(** Given a [cfg], it returns a mapping from variables to registers that
    contains all the variables appearing in the CFG *)
let rec pre_map_vars cfg =
  let empty_map = VarRegMap.initialize cfg.input cfg.output in
  NodeMap.fold
    (fun _ simple_cmds map ->
      List.fold_left
        (fun var_reg_map simple_cmd ->
          pre_map_vars_simple_cmd simple_cmd var_reg_map)
        map simple_cmds)
    cfg.code_map empty_map

(** Compile an [aexpr] into a list of instructions that compute the value of the
    [aexpr] in a destination register. The function also returns an updated
    variable-to-register mapping, which keeps track of fresh registers generated
    during the compilation. The function also tries to generates as less
    registers as possible, considering different cases and exploting the
    [BinImmOp] instructions when possible *)
let rec compile_aexpr var_reg_map dest_reg = function
  | Int n -> ([ LoadImm (n, dest_reg) ], var_reg_map)
  | Var x ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      ([ UnOp (Copy, src_reg, dest_reg) ], var_reg_map)
  | Add (Int n, Var x) | Add (Var x, Int n) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      ([ BinImmOp (AddImm, src_reg, n, dest_reg) ], var_reg_map)
  | Add (Var x, Var y) ->
      let src_reg1, var_reg_map = VarRegMap.lookup x var_reg_map in
      let src_reg2, var_reg_map = VarRegMap.lookup y var_reg_map in
      ([ BinRegOp (Add, src_reg1, src_reg2, dest_reg) ], var_reg_map)
  | Add (Int n, aexpr) | Add (aexpr, Int n) ->
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      (instrs @ [ BinImmOp (AddImm, dest_reg, n, dest_reg) ], var_reg_map)
  | Add (Var x, aexpr) | Add (aexpr, Var x) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let temp_reg, var_reg_map =
        if src_reg = dest_reg then VarRegMap.fresh_reg var_reg_map
        else (dest_reg, var_reg_map)
      in
      let instrs, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr in
      (instrs @ [ BinRegOp (Add, src_reg, temp_reg, dest_reg) ], var_reg_map)
  | Add (aexpr1, aexpr2) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs1, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr1 in
      let instrs2, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr2 in
      ( instrs1 @ instrs2 @ [ BinRegOp (Add, temp_reg, dest_reg, dest_reg) ],
        var_reg_map )
  | Sub (Var x, Int n) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      ([ BinImmOp (SubImm, src_reg, n, dest_reg) ], var_reg_map)
  | Sub (Int n, Var x) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let temp_reg, var_reg_map =
        if src_reg = dest_reg then VarRegMap.fresh_reg var_reg_map
        else (dest_reg, var_reg_map)
      in
      ( [ LoadImm (n, temp_reg); BinRegOp (Sub, temp_reg, src_reg, dest_reg) ],
        var_reg_map )
  | Sub (Var x, Var y) ->
      let src_reg1, var_reg_map = VarRegMap.lookup x var_reg_map in
      let src_reg2, var_reg_map = VarRegMap.lookup y var_reg_map in
      ([ BinRegOp (Sub, src_reg1, src_reg2, dest_reg) ], var_reg_map)
  | Sub (aexpr, Int n) ->
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      (instrs @ [ BinImmOp (SubImm, dest_reg, n, dest_reg) ], var_reg_map)
  | Sub (Int n, aexpr) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      ( instrs
        @ [
            LoadImm (n, temp_reg); BinRegOp (Sub, temp_reg, dest_reg, dest_reg);
          ],
        var_reg_map )
  | Sub (Var x, aexpr) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let temp_reg, var_reg_map =
        if src_reg = dest_reg then VarRegMap.fresh_reg var_reg_map
        else (dest_reg, var_reg_map)
      in
      let instrs, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr in
      (instrs @ [ BinRegOp (Sub, src_reg, temp_reg, dest_reg) ], var_reg_map)
  | Sub (aexpr, Var x) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let temp_reg, var_reg_map =
        if src_reg = dest_reg then VarRegMap.fresh_reg var_reg_map
        else (dest_reg, var_reg_map)
      in
      let instrs, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr in
      (instrs @ [ BinRegOp (Sub, temp_reg, src_reg, dest_reg) ], var_reg_map)
  | Sub (aexpr1, aexpr2) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs1, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr1 in
      let instrs2, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr2 in
      ( instrs1 @ instrs2 @ [ BinRegOp (Sub, temp_reg, dest_reg, dest_reg) ],
        var_reg_map )
  | Mul (Int n, Var x) | Mul (Var x, Int n) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      ([ BinImmOp (MulImm, src_reg, n, dest_reg) ], var_reg_map)
  | Mul (Var x, Var y) ->
      let src_reg1, var_reg_map = VarRegMap.lookup x var_reg_map in
      let src_reg2, var_reg_map = VarRegMap.lookup y var_reg_map in
      ([ BinRegOp (Mul, src_reg1, src_reg2, dest_reg) ], var_reg_map)
  | Mul (Int n, aexpr) | Mul (aexpr, Int n) ->
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      (instrs @ [ BinImmOp (MulImm, dest_reg, n, dest_reg) ], var_reg_map)
  | Mul (Var x, aexpr) | Mul (aexpr, Var x) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let temp_reg, var_reg_map =
        if src_reg = dest_reg then VarRegMap.fresh_reg var_reg_map
        else (dest_reg, var_reg_map)
      in
      let instrs, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr in
      (instrs @ [ BinRegOp (Mul, src_reg, temp_reg, dest_reg) ], var_reg_map)
  | Mul (aexpr1, aexpr2) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs1, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr1 in
      let instrs2, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr2 in
      ( instrs1 @ instrs2 @ [ BinRegOp (Mul, temp_reg, dest_reg, dest_reg) ],
        var_reg_map )

(** Compile a [bexpr] into a list of instructions that compute the value of the
    [bexpr] in a destination register. The function also returns an updated
    variable-to-register mapping, which keeps track of fresh registers generated
    during the compilation. The function also tries to generates as less
    registers as possible, considering different cases and exploting the
    [BinImmOp] instructions when possible *)
let rec compile_bexpr var_reg_map dest_reg = function
  | Bool b -> ([ LoadImm ((if b then 1 else 0), dest_reg) ], var_reg_map)
  | And (bexpr1, bexpr2) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs1, var_reg_map = compile_bexpr var_reg_map temp_reg bexpr1 in
      let instrs2, var_reg_map = compile_bexpr var_reg_map dest_reg bexpr2 in
      ( instrs1 @ instrs2 @ [ BinRegOp (And, temp_reg, dest_reg, dest_reg) ],
        var_reg_map )
  | Not bexpr ->
      let instrs, var_reg_map = compile_bexpr var_reg_map dest_reg bexpr in
      (instrs @ [ UnOp (Not, dest_reg, dest_reg) ], var_reg_map)
  (* The two subsequent branches do not create any clash problem between src_reg and dest_reg since 
  boolean values cannot be assigned to variables, and thus dest_reg is always different from src_reg *)
  | LessThan (Int n, Var x) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      ( [ LoadImm (n, dest_reg); BinRegOp (Less, dest_reg, src_reg, dest_reg) ],
        var_reg_map )
  | LessThan (Var x, Int n) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      ( [ LoadImm (n, dest_reg); BinRegOp (Less, src_reg, dest_reg, dest_reg) ],
        var_reg_map )
  | LessThan (Var x, Var y) ->
      let src_reg1, var_reg_map = VarRegMap.lookup x var_reg_map in
      let src_reg2, var_reg_map = VarRegMap.lookup y var_reg_map in
      ([ BinRegOp (Less, src_reg1, src_reg2, dest_reg) ], var_reg_map)
  | LessThan (Int n, aexpr) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      ( instrs
        @ [
            LoadImm (n, temp_reg); BinRegOp (Less, temp_reg, dest_reg, dest_reg);
          ],
        var_reg_map )
  | LessThan (aexpr, Int n) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      ( instrs
        @ [
            LoadImm (n, temp_reg); BinRegOp (Less, dest_reg, temp_reg, dest_reg);
          ],
        var_reg_map )
  (* The two subsequent branches do not create any clash problem between src_reg and dest_reg since 
  boolean values cannot be assigned to variables, and thus dest_reg is always different from src_reg *)
  | LessThan (Var x, aexpr) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      (instrs @ [ BinRegOp (Less, src_reg, dest_reg, dest_reg) ], var_reg_map)
  | LessThan (aexpr, Var x) ->
      let src_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let instrs, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr in
      (instrs @ [ BinRegOp (Less, dest_reg, src_reg, dest_reg) ], var_reg_map)
  | LessThan (aexpr1, aexpr2) ->
      let temp_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let instrs1, var_reg_map = compile_aexpr var_reg_map temp_reg aexpr1 in
      let instrs2, var_reg_map = compile_aexpr var_reg_map dest_reg aexpr2 in
      ( instrs1 @ instrs2 @ [ BinRegOp (Less, temp_reg, dest_reg, dest_reg) ],
        var_reg_map )

(** Compile a list of [simple_cmd] into a list of instructions. The function
    also returns an optional guard register, which is the register used in the
    implicit branch instruction of the last node of the block, if it is present.
    The function raises an error if there is a guard command that is not the
    last command of the block. *)
let rec compile_simple_cmds var_reg_map = function
  | [] -> ([], None)
  | SSkip :: cmds ->
      let instrs, opt_guard_reg = compile_simple_cmds var_reg_map cmds in
      (Nop :: instrs, opt_guard_reg)
  | SAssign (x, aexpr) :: cmds ->
      let dest_reg, var_reg_map = VarRegMap.lookup x var_reg_map in
      let assign_instrs, var_reg_map =
        compile_aexpr var_reg_map dest_reg aexpr
      in
      let instrs, opt_guard_reg = compile_simple_cmds var_reg_map cmds in
      (assign_instrs @ instrs, opt_guard_reg)
  | [ SGuard bexpr ] ->
      let guard_reg, var_reg_map = VarRegMap.fresh_reg var_reg_map in
      let guard_instrs, var_reg_map =
        compile_bexpr var_reg_map guard_reg bexpr
      in
      (guard_instrs, Some guard_reg)
  | SGuard _ :: cmds ->
      raise (CFGError "Guard must be the last command in a block")

(** Translates a miniImp [cfg] to a miniRISC [cfg] *)
let rec translate_cfg cfg =
  let map = pre_map_vars cfg in
  let code_map =
    NodeMap.map
      (fun simple_cmds -> compile_simple_cmds map simple_cmds)
      cfg.code_map
  in
  { cfg with code_map; input = RegIn; output = RegOut }

(** Add explicit jump instructions to a [cfg] that has implicit branch
    instructions. The function assumes that the input CFG is consistent, meaning
    that every node with a guard register has a corresponding branch edge, and
    vice versa. *)
let add_jump_instrs cfg =
  let code_map =
    NodeMap.mapi
      (fun node (instrs, opt_guard_reg) ->
        match (opt_guard_reg, NodeMap.find_opt node cfg.edges) with
        | None, Some (Next (Node dest_node_id)) ->
            instrs @ [ Jump (Label dest_node_id) ]
        | Some cond_reg, Some (Branch (Node dest_node_id1, Node dest_node_id2))
          ->
            instrs
            @ [ CondJump (cond_reg, Label dest_node_id1, Label dest_node_id2) ]
        | None, None when node = cfg.final_node -> instrs
        | _ ->
            raise
              (CFGError
                 "Inconsistent CFG: guard command without branch or branch \
                  without guard"))
      cfg.code_map
  in
  { cfg with code_map }

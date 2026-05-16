open Either
open Var
open GenericCFG
open MiniImpAST
open MiniImpCFG

exception UndefinedVariable of string

type error_loc =
  | Expr of expr
  | Output of var
      (** Type for representing the location where an undefined variable is
          used. It can be either in an expression or as the output variable of
          the program *)

type undef_var_error = { undef_var : var; error_loc : error_loc }
(** Type for representing an error of undefined variable. Contains the name of
    the undefined variable and the location where it is used *)

type local_vars_info = { all_vars : VarSet.t; defined_vars : VarSet.t }
(** Type for storing variable information at each node. [all_vars] is a set
    containg all the variables appearing the node, whereas [defined_vars] is a
    set containing the variables defined in the*)

type global_vars_info = local_vars_info NodeMap.t
(** Type for storing variable information for the whole CFG. Maps each node to
    its corresponding [local_vars_info] *)

(** Returns the set of variables appearing in [aexpr] union [vars] *)
let rec aexpr_vars aexpr vars =
  match aexpr with
  | Int _ -> vars
  | Var var -> VarSet.add var vars
  | Add (aexpr1, aexpr2) -> vars |> aexpr_vars aexpr1 |> aexpr_vars aexpr2
  | Sub (aexpr1, aexpr2) -> vars |> aexpr_vars aexpr1 |> aexpr_vars aexpr2
  | Mul (aexpr1, aexpr2) -> vars |> aexpr_vars aexpr1 |> aexpr_vars aexpr2

(** Returns the set of variables appearing in [bexpr] union [vars] *)
let rec bexpr_vars bexpr vars =
  match bexpr with
  | Bool _ -> vars
  | LessThan (aexpr1, aexpr2) -> vars |> aexpr_vars aexpr1 |> aexpr_vars aexpr2
  | And (bexpr1, bexpr2) -> vars |> bexpr_vars bexpr1 |> bexpr_vars bexpr2
  | Not bexpr -> vars |> bexpr_vars bexpr

let collect_local_info =
  List.fold_left
    (fun vars simple_cmd ->
      match simple_cmd with
      | SSkip -> vars
      | SAssign (var, aexpr) ->
          let all_vars = vars.all_vars |> VarSet.add var |> aexpr_vars aexpr in
          let defined_vars = vars.defined_vars |> VarSet.add var in
          { all_vars; defined_vars }
      | SGuard bexpr -> { vars with all_vars = bexpr_vars bexpr vars.all_vars })
    { all_vars = VarSet.empty; defined_vars = VarSet.empty }

(** Returns the variables information for the entire CFG *)
let collect_info cfg = NodeMap.map collect_local_info cfg.code_map

module DefinedVariablesAnalysis = DataflowAnalysis.Make (struct
  type t = var
  type cmd = simple_cmd list
  type aux = global_vars_info

  module Set = VarSet

  let direction = DataflowAnalysis.Forward
  let modality = DataflowAnalysis.Must
  let gen (_, vars_info) node = (NodeMap.find node vars_info).defined_vars
  let kill _ _ = VarSet.empty
  let initial_node cfg = cfg.initial_node
  let initial_node_state cfg = VarSet.singleton cfg.input

  let compute_initial_state (cfg, vars_info) =
    NodeMap.fold
      (fun _ local_info acc -> VarSet.union local_info.all_vars acc)
      vars_info VarSet.empty
  let compute_aux_info cfg = collect_info cfg
end)

(** Check for undefined variables in the CFG using the results of the defined
    variables analysis. For each node, it checks if any variable used in the
    node's simple commands is not in the [in_vars] set for that node, which is
    incrementally updated collecting the variables defined in the node. If an
    undefined variable is found, an error of type [undef_var_error] is created,
    containing the name of the undefined variable and the expression where it is
    used. After an error is found in a node, the analysis for that node is
    stopped and the next node is checked. The function returns a list of all the
    errors found in the CFG *)
let analyze cfg =
  let fixpoint = DefinedVariablesAnalysis.worklist_algorithm cfg in
  let check_simple_cmd in_vars simple_cmd =
    match simple_cmd with
    | SSkip -> Left in_vars
    | SAssign (var, aexpr) -> (
        let used_vars = aexpr_vars aexpr VarSet.empty in
        match
          VarSet.filter
            (fun used_var -> not (VarSet.mem used_var in_vars))
            used_vars
          |> VarSet.choose_opt
        with
        | None -> Left (VarSet.add var in_vars)
        | Some undef_var -> Right { undef_var; error_loc = Expr (Aexpr aexpr) })
    | SGuard bexpr -> (
        let used_vars = bexpr_vars bexpr VarSet.empty in
        match
          VarSet.filter
            (fun used_var -> not (VarSet.mem used_var in_vars))
            used_vars
          |> VarSet.choose_opt
        with
        | None -> Left in_vars
        | Some undef_var -> Right { undef_var; error_loc = Expr (Bexpr bexpr) })
  in
  let check_node node instrs errors =
    let in_vars = (NodeMap.find node fixpoint).in_values in
    let outcome =
      List.fold_left
        (fun in_vars_or_error simple_cmd ->
          match in_vars_or_error with
          | Left in_vars -> check_simple_cmd in_vars simple_cmd
          | Right error -> Right error)
        (Left in_vars) instrs
      |> function
      | Left in_vars ->
          if node = cfg.final_node && not (VarSet.mem cfg.output in_vars) then
            Right { undef_var = cfg.output; error_loc = Output cfg.output }
          else Left in_vars
      | Right error -> Right error
    in
    match outcome with Left in_vars -> errors | Right error -> error :: errors
  in
  NodeMap.fold check_node cfg.code_map []

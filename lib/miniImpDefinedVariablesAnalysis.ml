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

type node_vars_info = { all_vars : VarSet.t; defined_vars : VarSet.t }
(** Type for storing variable information at each node. [all_vars] is a set
    containg all the variables appearing the node, whereas [defined_vars] is a
    set containing the variables defined in the*)

type cfg_vars_info = node_vars_info NodeMap.t
(** Type for storing variable information for the whole CFG. Maps each node to
    its corresponding [node_vars_info] *)

type node_analysis_state = { in_vars : VarSet.t; out_vars : VarSet.t }
(** Type for storing the analysis state at each node. [in_vars] is the set of
    variables defined before executing the node, whereas [out_vars] is the set
    of variables defined after executing the node *)

type cfg_analysis_state = node_analysis_state NodeMap.t
(** Type for storing the analysis state for the whole CFG. Maps each node to its
    corresponding [node_analysis_state] *)

let empty_node_vars = { all_vars = VarSet.empty; defined_vars = VarSet.empty }

(** Returns the set of variables appearing in [aexpr] union [vars] *)
let rec collect_vars_aexpr aexpr vars =
  match aexpr with
  | Int _ -> vars
  | Var var -> VarSet.add var vars
  | Add (aexpr1, aexpr2) ->
      vars |> collect_vars_aexpr aexpr1 |> collect_vars_aexpr aexpr2
  | Sub (aexpr1, aexpr2) ->
      vars |> collect_vars_aexpr aexpr1 |> collect_vars_aexpr aexpr2
  | Mul (aexpr1, aexpr2) ->
      vars |> collect_vars_aexpr aexpr1 |> collect_vars_aexpr aexpr2

(** Returns the set of variables appearing in [bexpr] union [vars] *)
let rec collect_vars_bexpr bexpr vars =
  match bexpr with
  | Bool _ -> vars
  | LessThan (aexpr1, aexpr2) ->
      vars |> collect_vars_aexpr aexpr1 |> collect_vars_aexpr aexpr2
  | And (bexpr1, bexpr2) ->
      vars |> collect_vars_bexpr bexpr1 |> collect_vars_bexpr bexpr2
  | Not bexpr -> vars |> collect_vars_bexpr bexpr

(** Returns the variables information for the entire CFG *)
let collect_vars_info cfg =
  NodeMap.map
    (fun simple_cmds ->
      List.fold_left
        (fun { all_vars; defined_vars } simple_cmd ->
          match simple_cmd with
          | SSkip -> { all_vars; defined_vars }
          | SAssign (var, aexpr) ->
              let all_vars' =
                all_vars |> VarSet.add var |> collect_vars_aexpr aexpr
              in
              let defined_vars' = defined_vars |> VarSet.add var in
              { all_vars = all_vars'; defined_vars = defined_vars' }
          | SGuard bexpr ->
              let all_vars' = all_vars |> collect_vars_bexpr bexpr in
              { all_vars = all_vars'; defined_vars })
        empty_node_vars simple_cmds)
    cfg.code_map

(** Top element of the CPO for the analysis, meaning that each node has both
    [in_vars] and [out_vars] equal to the set of all variables appearing in the
    CFG *)
let analysis_state_top cfg_vars_info =
  let all_vars =
    NodeMap.fold
      (fun _ node_vars_info all_vars ->
        VarSet.union node_vars_info.all_vars all_vars)
      cfg_vars_info VarSet.empty
  in
  cfg_vars_info
  |> NodeMap.map (fun _ -> { in_vars = all_vars; out_vars = all_vars })

(** Compute the fixpoint of the defined variables analysis for the given CFG.
    Returns a map from each node to its corresponding [node_analysis_state] at
    the fixpoint. The fixpoint is computed using a breadth-first search (BFS)
    approach, propagating the defined variables information through the CFG
    until no changes occur. This is done by maintaining a queue of nodes to
    visit, and for each node, computing the new [in_vars] and [out_vars] based
    on the previous state and the variables defined in the node. If a node has
    already been visited and its [in_vars] has not changed, it is not re-added
    to the queue *)
let defined_vars_analysis cfg =
  let cfg_vars_info = collect_vars_info cfg in
  let rec bfs queue visited_nodes analysis_state =
    match Queue.take_opt queue with
    | None -> analysis_state
    | Some (prev_node_out_vars, node) -> (
        let in_vars = (NodeMap.find node analysis_state).in_vars in
        let in_vars' =
          if node = cfg.initial_node then VarSet.singleton cfg.input
          else VarSet.inter prev_node_out_vars in_vars
        in
        if NodeSet.mem node visited_nodes && VarSet.equal in_vars in_vars' then
          bfs queue visited_nodes analysis_state
        else
          let defined_vars_in_node =
            (NodeMap.find node cfg_vars_info).defined_vars
          in
          let out_vars' = VarSet.union in_vars' defined_vars_in_node in
          let state' =
            analysis_state
            |> NodeMap.add node { in_vars = in_vars'; out_vars = out_vars' }
          in
          let visited_nodes' = NodeSet.add node visited_nodes in
          match NodeMap.find_opt node cfg.edges with
          | None -> bfs queue visited_nodes' state'
          | Some (Next node) ->
              Queue.add (out_vars', node) queue;
              bfs queue visited_nodes' state'
          | Some (Branch (node1, node2)) ->
              Queue.add (out_vars', node1) queue;
              Queue.add (out_vars', node2) queue;
              bfs queue visited_nodes' state')
  in
  let queue = Queue.create () in
  Queue.add (VarSet.empty, cfg.initial_node) queue;
  bfs queue NodeSet.empty (analysis_state_top cfg_vars_info)

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
  let fixpoint = defined_vars_analysis cfg in
  let check_simple_cmd in_vars simple_cmd =
    match simple_cmd with
    | SSkip -> Left in_vars
    | SAssign (var, aexpr) -> (
        let used_vars = collect_vars_aexpr aexpr VarSet.empty in
        match
          VarSet.filter
            (fun used_var -> not (VarSet.mem used_var in_vars))
            used_vars
          |> VarSet.choose_opt
        with
        | None -> Left (VarSet.add var in_vars)
        | Some undef_var -> Right { undef_var; error_loc = Expr (Aexpr aexpr) })
    | SGuard bexpr -> (
        let used_vars = collect_vars_bexpr bexpr VarSet.empty in
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
    let in_vars = (NodeMap.find node fixpoint).in_vars in
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

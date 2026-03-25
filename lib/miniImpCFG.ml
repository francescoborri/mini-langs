open Var
open GenericCFG
open MiniImpAST

type cfg = (simple_cmd list, var) GenericCFG.cfg
(** CFG representation for miniImp. Each node is associated to a list of
    [simple_cmd], and the input/output entities of the CFG are of type [var]. *)

(** Given an [aexpr], it returns an equivalent [aexpr] where constant
    subexpressions have been evaluated. *)
let rec propagate_constants_aexpr = function
  | Int n -> Int n
  | Var x -> Var x
  | Add (aexpr1, aexpr2) -> (
      let aexpr1' = propagate_constants_aexpr aexpr1 in
      let aexpr2' = propagate_constants_aexpr aexpr2 in
      match (aexpr1', aexpr2') with
      | Int n, Int m -> Int (n + m)
      | Int 0, aexpr | aexpr, Int 0 -> aexpr
      | _ -> Add (aexpr1', aexpr2'))
  | Sub (aexpr1, aexpr2) -> (
      let aexpr1' = propagate_constants_aexpr aexpr1 in
      let aexpr2' = propagate_constants_aexpr aexpr2 in
      match (aexpr1', aexpr2') with
      | Int n, Int m -> Int (n - m)
      | aexpr, Int 0 -> aexpr
      | _ -> Sub (aexpr1', aexpr2'))
  | Mul (aexpr1, aexpr2) -> (
      let aexpr1' = propagate_constants_aexpr aexpr1 in
      let aexpr2' = propagate_constants_aexpr aexpr2 in
      match (aexpr1', aexpr2') with
      | Int n, Int m -> Int (n * m)
      | Int 0, _ | _, Int 0 -> Int 0
      | Int 1, aexpr | aexpr, Int 1 -> aexpr
      | _ -> Mul (aexpr1', aexpr2'))

(** Given a [bexpr], it returns an equivalent [bexpr] where constant
    subexpressions have been evaluated. *)
let rec propagate_constants_bexpr = function
  | Bool b -> Bool b
  | LessThan (aexpr1, aexpr2) -> (
      let aexpr1' = propagate_constants_aexpr aexpr1 in
      let aexpr2' = propagate_constants_aexpr aexpr2 in
      match (aexpr1', aexpr2') with
      | Int n, Int m -> if n < m then Bool true else Bool false
      | _ -> LessThan (aexpr1', aexpr2'))
  | And (bexpr1, bexpr2) -> (
      let bexpr1' = propagate_constants_bexpr bexpr1 in
      let bexpr2' = propagate_constants_bexpr bexpr2 in
      match (bexpr1', bexpr2') with
      | Bool true, Bool true -> Bool true
      | Bool false, _ | _, Bool false -> Bool false
      | _ -> And (bexpr1', bexpr2'))
  | Not bexpr -> (
      let bexpr' = propagate_constants_bexpr bexpr in
      match bexpr' with
      | Bool true -> Bool false
      | Bool false -> Bool true
      | _ -> Not bexpr')

(** Given a [cmd], it returns an equivalent [cmd] where constant subexpressions
    have been evaluated. *)
let rec propagate_constants_cmd = function
  | Skip -> Skip
  | Assign (x, aexpr) -> (
      match propagate_constants_aexpr aexpr with
      | Int n -> Assign (x, Int n)
      | aexpr' -> Assign (x, aexpr'))
  | Seq (cmd1, cmd2) ->
      let cmd1' = propagate_constants_cmd cmd1 in
      let cmd2' = propagate_constants_cmd cmd2 in
      Seq (cmd1', cmd2')
  | IfThenElse (cond, then_cmd, else_cmd) -> (
      let cond' = propagate_constants_bexpr cond in
      let then_cmd' = propagate_constants_cmd then_cmd in
      let else_cmd' = propagate_constants_cmd else_cmd in
      match cond' with
      | Bool true -> then_cmd'
      | Bool false -> else_cmd'
      | _ -> IfThenElse (cond', then_cmd', else_cmd'))
  | While (cond, body_cmd) -> (
      let cond' = propagate_constants_bexpr cond in
      let body_cmd' = propagate_constants_cmd body_cmd in
      match cond' with
      | Bool true -> While (cond', body_cmd')
      | Bool false -> Skip
      | _ -> While (cond', body_cmd'))

(** Given a [prog], it returns an equivalent [prog] where constant
    subexpressions have been evaluated. *)
let propagate_constants (Prog (input, output, cmd)) =
  let cmd' = propagate_constants_cmd cmd in
  Prog (input, output, cmd')

(** Concatenate two [simple_cmd list] and removes the [Skip] commands; if the
    resulting list is empty, it returns a list with a single [Skip]. *)
let rec concat_simple_cmds simple_cmds1 simple_cmds2 =
  match List.filter (( <> ) SSkip) (simple_cmds1 @ simple_cmds2) with
  | [] -> [ SSkip ]
  | _ as simple_cmds -> simple_cmds

(** Given a [prog], it builds the corresponding maximal CFG. *)
let build_cfg (Prog (input, output, cmd)) =
  (* Inner recursive function that builds the CFG. The function assures that
  the returned CFG has always one initial node without incoming edges, and
  one final node without outgoing edges. *)
  let rec helper prev_cfg last_node cmd =
    let partial_cfg, last_node =
      match cmd with
      | Skip ->
          let skip_node = fresh_node last_node in
          ( pack
              (NodeSet.add skip_node prev_cfg.nodes)
              prev_cfg.edges skip_node skip_node
              (NodeMap.add skip_node [ SSkip ] prev_cfg.code_map),
            skip_node )
      | Assign (x, aexpr) ->
          let assign_node = fresh_node last_node in
          ( pack
              (NodeSet.add assign_node prev_cfg.nodes)
              prev_cfg.edges assign_node assign_node
              (NodeMap.add assign_node [ SAssign (x, aexpr) ] prev_cfg.code_map),
            assign_node )
      | Seq (cmd1, cmd2) ->
          let cfg1, last_node1 = helper prev_cfg last_node cmd1 in
          let simple_cmds1 = NodeMap.find last_node1 cfg1.code_map in
          (* Here the recursive call is made by passing the previous node of the last
          generated node, which means that it will be overwritten by the first generated
          node of the second command: this is what we want in order to have a maximal CFG,
          so that the last node of the first command and the first node of the second command
          are the same. Note that [simple_cmds1] is saved before the recursive call to correctly
          update the code map after the recursive call. *)
          let cfg2, last_node2 = helper cfg1 (prev_node last_node1) cmd2 in
          let code_map =
            NodeMap.update last_node1
              (function
                | Some simple_cmds2 ->
                    Some (concat_simple_cmds simple_cmds1 simple_cmds2)
                | None -> raise (CFGError "Unexpected missing node in code map"))
              cfg2.code_map
          in
          ( pack cfg2.nodes cfg2.edges cfg1.initial_node cfg2.final_node code_map,
            last_node2 )
      | IfThenElse (cond, then_cmd, else_cmd) ->
          let guard_node = fresh_node last_node in
          let cfg_then, last_node_then = helper prev_cfg guard_node then_cmd in
          let cfg_else, last_node_else =
            helper cfg_then last_node_then else_cmd
          in
          let join_node = fresh_node last_node_else in
          let nodes =
            cfg_else.nodes |> NodeSet.add guard_node |> NodeSet.add join_node
          in
          let edges =
            cfg_else.edges
            |> NodeMap.add guard_node
                 (Branch (cfg_then.initial_node, cfg_else.initial_node))
            |> NodeMap.add cfg_then.final_node (Next join_node)
            |> NodeMap.add cfg_else.final_node (Next join_node)
          in
          let code_map =
            cfg_else.code_map
            |> NodeMap.add guard_node [ SGuard cond ]
            |> NodeMap.add join_node [ SSkip ]
          in
          (pack nodes edges guard_node join_node code_map, join_node)
      | While (cond, body_cmd) ->
          let dummy_initial_node = fresh_node last_node in
          let guard_node = fresh_node dummy_initial_node in
          let cfg_body, last_node_body = helper prev_cfg guard_node body_cmd in
          let dummy_final_node = fresh_node last_node_body in
          let nodes =
            cfg_body.nodes
            |> NodeSet.add dummy_initial_node
            |> NodeSet.add guard_node
            |> NodeSet.add dummy_final_node
          in
          let edges =
            cfg_body.edges
            |> NodeMap.add dummy_initial_node (Next guard_node)
            |> NodeMap.add guard_node
                 (Branch (cfg_body.initial_node, dummy_final_node))
            |> NodeMap.add cfg_body.final_node (Next guard_node)
          in
          let code_map =
            cfg_body.code_map
            |> NodeMap.add guard_node [ SGuard cond ]
            |> NodeMap.add dummy_initial_node [ SSkip ]
            |> NodeMap.add dummy_final_node [ SSkip ]
          in
          ( pack nodes edges dummy_initial_node dummy_final_node code_map,
            dummy_final_node )
    in
    (partial_cfg prev_cfg.input prev_cfg.output, last_node)
  in
  helper (empty_cfg input output) invalid_node cmd |> fst

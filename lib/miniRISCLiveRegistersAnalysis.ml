open GenericCFG
open MiniRISCCFG
open MiniRISCInstructionSet
open MiniRISCRegUtils
open Utils

type node_regs_info = { used_regs : RegSet.t; defined_regs : RegSet.t }
(** Type for storing register information at each node. [used_regs] is the set
    of registers used by any instruction in the node and not defined before that
    instructions, whereas [defined_regs] is just the set of registers defined by
    the node *)

type cfg_regs_info = node_regs_info NodeMap.t
(** Type for storing register information for the whole CFG. Maps each node to
    its corresponding [node_regs_info] *)

type node_analysis_state = { in_regs : RegSet.t; out_regs : RegSet.t }
(** Type for storing the analysis state at each node. [in_regs] is the set of
    live registers before executing the node, whereas [out_regs] is the set of
    registers live after executing the node *)

type cfg_analysis_state = node_analysis_state NodeMap.t
(** Type for storing the analysis state for the whole CFG. Maps each node to its
    corresponding [node_analysis_state] *)

let empty_node_regs_info =
  { used_regs = RegSet.empty; defined_regs = RegSet.empty }

let node_bottom_analysis_state =
  { in_regs = RegSet.empty; out_regs = RegSet.empty }

(** Given a [cfg_regs_info], builds bottom element of the CPO for the analysis,
    meaning that each node has both [in_regs] and [out_regs] equal to empty set
*)
let bottom_analysis_state = NodeMap.map (fun _ -> node_bottom_analysis_state)

let rev_edge src dest =
  NodeMap.update dest (fun opt_from_nodes ->
      let from_nodes =
        match opt_from_nodes with
        | None -> [ src ]
        | Some from_nodes -> src :: from_nodes
      in
      Some from_nodes)

(** Returns the reversed edges of [cfg], which is a map from node to list of
    nodes *)
let rev_edges cfg =
  NodeMap.fold
    (fun src edge acc ->
      match edge with
      | Next dest -> rev_edge src dest acc
      | Branch (dest1, dest2) -> acc |> rev_edge src dest1 |> rev_edge src dest2)
    cfg.edges NodeMap.empty

let collect_node_regs_info =
  List.fold_left
    (fun { used_regs; defined_regs } instr ->
      {
        used_regs =
          RegSet.union used_regs
            (RegSet.filter
               (fun reg -> RegSet.mem reg defined_regs |> not)
               (using_regs instr));
        defined_regs =
          Option.fold ~none:defined_regs
            ~some:(fun reg -> RegSet.add reg defined_regs)
            (defining_reg instr);
      })
    empty_node_regs_info

(** Returns the registers information for the entire CFG *)
let collect_regs_info cfg = NodeMap.map collect_node_regs_info cfg.code_map

(** Compute the fixpoint of the live registers analysis for the given CFG.
    Returns a map from each node to its corresponding [node_analysis_state] at
    the fixpoint. The fixpoint is computed using a reversed breadth-first search
    (BFS) approach, propagating the live registers information backward through
    the CFG until no changes occur. This is done by maintaining a queue of nodes
    to visit, and for each node, computing the new [in_regs] and [out_regs]
    based on the previous state and the registers defined in the node. If a node
    has already been visited and its [out_regs] has not changed, it is not
    re-added to the queue *)
let rec live_regs_analysis cfg =
  let regs_info = collect_regs_info cfg in
  let rev_edges = rev_edges cfg in
  let rec rev_bfs queue visited_nodes analysis_state =
    match Queue.take_opt queue with
    | None -> analysis_state
    | Some (next_node_in_regs, node) -> (
        let out_regs = (NodeMap.find node analysis_state).out_regs in
        let out_regs' =
          if node = cfg.final_node then RegSet.singleton cfg.output
          else RegSet.union next_node_in_regs out_regs
        in
        if NodeSet.mem node visited_nodes && RegSet.equal out_regs out_regs'
        then rev_bfs queue visited_nodes analysis_state
        else
          let node_regs = NodeMap.find node regs_info in
          let in_regs' =
            RegSet.diff out_regs' node_regs.defined_regs
            |> RegSet.union node_regs.used_regs
          in
          let analysis_state' =
            analysis_state
            |> NodeMap.add node { in_regs = in_regs'; out_regs = out_regs' }
          in
          let visited_nodes' = NodeSet.add node visited_nodes in
          match NodeMap.find_opt node rev_edges with
          | None -> rev_bfs queue visited_nodes' analysis_state'
          | Some from_nodes ->
              List.iter
                (fun from_node -> Queue.add (in_regs', from_node) queue)
                from_nodes;
              rev_bfs queue visited_nodes' analysis_state')
  in
  let queue = Queue.create () in
  Queue.add (RegSet.empty, cfg.final_node) queue;
  rev_bfs queue NodeSet.empty (bottom_analysis_state regs_info)

exception CFGError of string

type node = Node of int

module Node = struct
  type t = node

  let compare (Node node_id1) (Node node_id2) = compare node_id1 node_id2
end

module NodeSet = Set.Make (Node)
module NodeMap = Map.Make (Node)

type neighbor = Next of node | Branch of node * node

type ('a, 'b) cfg = {
  nodes : NodeSet.t;
  edges : neighbor NodeMap.t;
  initial_node : node;
  final_node : node;
  code_map : 'a NodeMap.t;
  input : 'b;
  output : 'b;
}
(** A generic control flow graph (CFG) representation. ['a] is the type of code
    associated with each node, and ['b] is the type of input and output. *)

(** Returns the node generated before [Node node_id]*)
let prev_node (Node node_id) = Node (node_id - 1)

(** Returns a fresh node given the last generated node [Node last_node_id] *)
let fresh_node (Node last_node_id) =
  if last_node_id + 1 < 0 then raise (CFGError "An invalid node was generated")
  else Node (last_node_id + 1)

(** An invalid node to initialize CFGs and error handling *)
let invalid_node = Node (-1)

let pack nodes edges initial_node final_node code_map input output =
  { nodes; edges; initial_node; final_node; code_map; input; output }

(** Returns an empty CFG with just the input and output entity *)
let empty_cfg input output =
  pack NodeSet.empty NodeMap.empty invalid_node invalid_node NodeMap.empty input
    output

let string_of_cfg string_of_code string_of_in_out cfg =
  let string_of_code code =
    code |> string_of_code |> String.split_on_char '\n' |> String.concat "\n  "
    |> ( ^ ) "  "
  in
  let string_of_edge (Node src_node_id) =
    match NodeMap.find_opt (Node src_node_id) cfg.edges with
    | None -> string_of_int src_node_id
    | Some (Next (Node dest_node_id)) ->
        Printf.sprintf "%d -> %d" src_node_id dest_node_id
    | Some (Branch (Node dest_node_id1, Node dest_node_id2)) ->
        Printf.sprintf "%d -> %d %d" src_node_id dest_node_id1 dest_node_id2
  in
  let string_of_node node =
    Printf.sprintf "%s\n%s" (string_of_edge node)
      (string_of_code (NodeMap.find node cfg.code_map))
  in
  let (Node initial_node_id) = cfg.initial_node in
  let (Node final_node_id) = cfg.final_node in
  Printf.sprintf
    "%d nodes, %d edges:\n\
     Initial node: %d\n\
     Final node: %d\n\
     Input: %s\n\
     Output: %s\n\
     %s"
    (NodeSet.cardinal cfg.nodes)
    (NodeMap.cardinal cfg.edges)
    initial_node_id final_node_id
    (string_of_in_out cfg.input)
    (string_of_in_out cfg.output)
    (NodeSet.to_list cfg.nodes |> List.map string_of_node |> String.concat "\n")

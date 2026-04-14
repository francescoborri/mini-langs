open Utils

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

let dot_string_of_cfg string_of_code string_of_in_out string_of_node_header cfg
    =
  let dot_style =
    "    graph [\n\
    \        rankdir=TB\n\
    \        layout=dot\n\
    \    ]\n\
    \    node [\n\
    \        shape=record\n\
    \        style=\"filled,rounded\"\n\
    \        fontname=\"Courier\"\n\
    \        fontsize=13\n\
    \        colorscheme=\"paired12\"\n\
    \        color=2\n\
    \        fontcolor=2\n\
    \        fillcolor=1\n\
    \    ]\n\
    \    edge [\n\
    \        color=\"/greys9/7\"\n\
    \        fontname=\"Helvetica\"\n\
    \        fontsize=11\n\
    \        fontcolor=\"/greys9/7\"\n\
    \        arrowhead=vee\n\
    \    ]"
  in
  let branch_node_style =
    "shape=diamond margin=0.075 color=10 fontcolor=10 fillcolor=9"
  in
  let in_node_style = "shape=box color=4 fontcolor=4 fillcolor=3" in
  let out_node_style = "shape=box color=6 fontcolor=6 fillcolor=5" in
  let (Node initial_node_id) = cfg.initial_node in
  let (Node final_node_id) = cfg.final_node in
  let build_label header label =
    let escape_gt =
      String.to_seq
      >> Seq.flat_map (fun c ->
          if c = '>' then List.to_seq [ '\\'; '>' ] else Seq.return c)
      >> String.of_seq
    in
    match header with
    | None | Some "" -> label
    | Some header -> Printf.sprintf "{ %s | %s }" header (escape_gt label)
  in
  let branching_node (Node node_id) = Printf.sprintf "branch%d" node_id in
  let string_of_code = string_of_code >> String.escaped in
  let string_of_node node =
    let (Node node_id) = node in
    let code = NodeMap.find node cfg.code_map in
    let rev_code = List.rev code in
    match (rev_code, NodeMap.find_opt node cfg.edges) with
    | _, None | _, Some (Next _) ->
        let head = string_of_node_header node in
        let label = code |> string_of_code |> build_label head in
        Printf.sprintf "    %d [label=\"%s\"]" node_id label
    | [ last_instr ], Some (Branch _) ->
        let label = string_of_code code in
        Printf.sprintf "    %d [label=\"%s\"%s]" node_id label branch_node_style
    | last_instr :: rev_code, Some (Branch _) ->
        let header = string_of_node_header node in
        let node_label =
          rev_code |> List.rev |> string_of_code |> build_label header
        in
        let branch_label = string_of_code [ last_instr ] in
        let branching_node = branching_node (Node node_id) in
        Printf.sprintf "    %d [label=\"%s\"]\n    %s [label=\"%s\" %s]" node_id
          node_label branching_node branch_label branch_node_style
    | [], Some (Branch _) ->
        raise (CFGError "Attempting to print a branch node without code")
  in
  let string_of_edge node =
    let (Node src_node_id) = node in
    let code = NodeMap.find node cfg.code_map in
    let rev_code = List.rev code in
    match (rev_code, NodeMap.find_opt (Node src_node_id) cfg.edges) with
    | _, None -> ""
    | _, Some (Next (Node dest_node_id)) ->
        if src_node_id < dest_node_id then
          Printf.sprintf "    %d:s -> %d:n" src_node_id dest_node_id
        else Printf.sprintf "    %d -> %d" src_node_id dest_node_id
    | [ last_instr ], Some (Branch (Node dest_node_id1, Node dest_node_id2)) ->
        Printf.sprintf
          "    %d:sw -> %d:n [xlabel=\"true\"]\n\
          \    %d:se -> %d:n [xlabel=\"false\"]"
          src_node_id dest_node_id1 src_node_id dest_node_id2
    | ( last_instr :: rev_code,
        Some (Branch (Node dest_node_id1, Node dest_node_id2)) ) ->
        let branching_node = branching_node (Node src_node_id) in
        Printf.sprintf
          "    %d:s -> %s:n [style=dashed]\n\
          \    %s:sw -> %d:n [xlabel=\"true\"]\n\
          \    %s:se -> %d:n [xlabel=\"false\"]"
          src_node_id branching_node branching_node dest_node_id1 branching_node
          dest_node_id2
    | [], Some (Branch (Node dest_node_id1, Node dest_node_id2)) ->
        raise (CFGError "Attempting to print a branch node without code")
  in
  let edges_str =
    NodeSet.to_list cfg.nodes |> List.map string_of_edge
    |> List.filter (String.equal "" >> not)
    |> List.cons (Printf.sprintf "    in:s -> %d:n" initial_node_id)
    |> List.cons (Printf.sprintf "    %d:s -> out:n" final_node_id)
    |> String.concat "\n"
  in
  let nodes_str =
    NodeSet.to_list cfg.nodes |> List.map string_of_node
    |> List.cons
         (Printf.sprintf "    out [label=\"%s\" %s]"
            (string_of_in_out cfg.output)
            out_node_style)
    |> List.cons
         (Printf.sprintf "    in [label=\"%s\" %s]"
            (string_of_in_out cfg.input)
            in_node_style)
    |> String.concat "\n"
  in
  Printf.sprintf "digraph CFG {\n%s\n%s\n%s\n}" dot_style nodes_str edges_str

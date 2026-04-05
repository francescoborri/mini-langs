open Utils
open MiniImpParser
open MiniImpAST
open MiniImpCFG

let string_of_token = function
  | INT n -> Printf.sprintf "INT %d" n
  | BOOL b -> Printf.sprintf "BOOL %b" b
  | NAME x -> Printf.sprintf "NAME %s" x
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | LESS -> "LESS"
  | AND -> "AND"
  | NOT -> "NOT"
  | ASSIGN -> "ASSIGN"
  | SEQ -> "SEQ"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | SKIP -> "SKIP"
  | MAIN (input, output) -> Printf.sprintf "MAIN %s %s" input output
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EOF -> "EOF"

let rec string_of_cmd indent_level cmd =
  let indent = String.make (indent_level * 2) ' ' in
  match cmd with
  | Skip -> indent ^ "skip"
  | Assign (var, aexpr) ->
      Printf.sprintf "%s%s := %s" indent var (string_of_aexpr aexpr)
  | Seq (cmd1, cmd2) ->
      Printf.sprintf "%s;\n%s"
        (string_of_cmd indent_level cmd1)
        (string_of_cmd indent_level cmd2)
  | IfThenElse (bexpr, then_cmd, else_cmd) ->
      Printf.sprintf "%sif %s then (\n%s\n%s) else (\n%s\n%s)" indent
        (string_of_bexpr bexpr)
        (string_of_cmd (indent_level + 1) then_cmd)
        indent
        (string_of_cmd (indent_level + 1) else_cmd)
        indent
  | While (bexpr, body_cmd) ->
      Printf.sprintf "%swhile %s do (\n%s\n%s)" indent (string_of_bexpr bexpr)
        (string_of_cmd (indent_level + 1) body_cmd)
        indent

and string_of_aexpr = function
  | Int n -> string_of_int n
  | Var x -> x
  | Add (aexpr1, aexpr2) ->
      Printf.sprintf "(%s + %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)
  | Sub (aexpr1, aexpr2) ->
      Printf.sprintf "(%s - %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)
  | Mul (aexpr1, aexpr2) ->
      Printf.sprintf "(%s * %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)

and string_of_bexpr = function
  | Bool b -> string_of_bool b
  | LessThan (aexpr1, aexpr2) ->
      Printf.sprintf "(%s < %s)" (string_of_aexpr aexpr1)
        (string_of_aexpr aexpr2)
  | And (bexpr1, bexpr2) ->
      Printf.sprintf "(%s and %s)" (string_of_bexpr bexpr1)
        (string_of_bexpr bexpr2)
  | Not bexpr -> Printf.sprintf "(not %s)" (string_of_bexpr bexpr)

let string_of_expr = function
  | Aexpr aexpr -> string_of_aexpr aexpr
  | Bexpr bexpr -> string_of_bexpr bexpr

let string_of_ast (Prog (input, output, cmd)) =
  Printf.sprintf "def main with input %s output %s as\n%s" input output
    (string_of_cmd 1 cmd)

let dot_string_of_ast (Prog (input, output, cmd)) =
  let dot_style =
    "    graph [\n\
    \        rankdir=TB\n\
    \        layout=dot\n\n\
    \        ]\n\
    \    node [\n\
    \        shape=box\n\
    \        style=\"filled,rounded\"\n\
    \        fontname=\"Courier\"\n\
    \        fontsize=13\n\
    \        colorscheme=\"paired12\"\n\
    \    ]\n\
    \    edge [\n\
    \        color=\"/greys9/7\"\n\
    \        fontname=\"Helvetica\"\n\
    \        fontsize=11\n\
    \        fontcolor=\"/greys9/7\"\n\
    \        arrowhead=vee\n\
    \    ]"
  in
  let var_style =
    "color=\"/greys9/7\" fontcolor=\"/greys9/7\" fillcolor=\"/greys9/4\""
  in
  let aexpr_style = "color=4 fontcolor=4 fillcolor=3" in
  let bexpr_style = "color=10 fontcolor=10 fillcolor=9" in
  let cmd_style = "color=2 fontcolor=2 fillcolor=1" in
  let rec build_nodes_edges_aexpr next_node nodes edges = function
    | (Int _ | Var _) as atom ->
        let label =
          match atom with
          | Int n -> string_of_int n
          | Var x -> x
          | _ -> failwith "unreachable"
        in
        let node_str =
          Printf.sprintf "    %d [label=\"%s\" %s]" next_node label aexpr_style
        in
        (next_node + 1, node_str :: nodes, edges)
    | (Add (aexpr1, aexpr2) | Sub (aexpr1, aexpr2) | Mul (aexpr1, aexpr2)) as
      aexpr ->
        let node_id = next_node in
        let root_aexpr1 = node_id + 1 in
        let next_node, nodes, edges =
          build_nodes_edges_aexpr root_aexpr1 nodes edges aexpr1
        in
        let root_aexpr2 = next_node in
        let next_node, nodes, edges =
          build_nodes_edges_aexpr root_aexpr2 nodes edges aexpr2
        in
        let label =
          match aexpr with
          | Add _ -> "+"
          | Sub _ -> "-"
          | Mul _ -> "*"
          | _ -> failwith "unreachable"
        in
        let node_str =
          Printf.sprintf "    %d [label=\"%s\" %s]" node_id label aexpr_style
        in
        let edge1_str =
          Printf.sprintf "    %d:sw -> %d:n" node_id root_aexpr1
        in
        let edge2_str =
          Printf.sprintf "    %d:se -> %d:n" node_id root_aexpr2
        in
        (next_node + 1, node_str :: nodes, edge1_str :: edge2_str :: edges)
  and build_nodes_edges_bexpr next_node nodes edges = function
    | Bool b ->
        let node_str =
          Printf.sprintf "    %d [label=\"%b\" %s]" next_node b bexpr_style
        in
        (next_node + 1, node_str :: nodes, edges)
    | (LessThan _ | And _ | Not _) as bexpr ->
        let node_id = next_node in
        let root_aexpr1 = node_id + 1 in
        let next_node, nodes, edges =
          match bexpr with
          | LessThan (aexpr1, _) ->
              build_nodes_edges_aexpr root_aexpr1 nodes edges aexpr1
          | And (bexpr1, _) ->
              build_nodes_edges_bexpr root_aexpr1 nodes edges bexpr1
          | Not bexpr -> build_nodes_edges_bexpr root_aexpr1 nodes edges bexpr
          | _ -> failwith "unreachable"
        in
        let root_aexpr2 = next_node in
        let next_node, nodes, edges =
          match bexpr with
          | LessThan (_, aexpr2) ->
              build_nodes_edges_aexpr root_aexpr2 nodes edges aexpr2
          | And (_, bexpr2) ->
              build_nodes_edges_bexpr root_aexpr2 nodes edges bexpr2
          | Not _ -> (next_node, nodes, edges)
          | _ -> failwith "unreachable"
        in
        let label =
          match bexpr with
          | LessThan _ -> "<"
          | And _ -> "and"
          | Not _ -> "not"
          | _ -> failwith "unreachable"
        in
        let node_str =
          Printf.sprintf "    %d [label=\"%s\" %s]" node_id label bexpr_style
        in
        let edges =
          match bexpr with
          | Not _ ->
              let edge_str =
                Printf.sprintf "    %d:s -> %d:n" node_id root_aexpr1
              in
              edge_str :: edges
          | _ ->
              let edge1_str =
                Printf.sprintf "    %d:sw -> %d:n" node_id root_aexpr1
              in
              let edge2_str =
                Printf.sprintf "    %d:se -> %d:n" node_id root_aexpr2
              in
              edge1_str :: edge2_str :: edges
        in
        (next_node + 1, node_str :: nodes, edges)
  and build_nodes_edges_cmd next_node nodes edges = function
    | Skip ->
        let node_str =
          Printf.sprintf "    %d [label=\"skip\" %s]" next_node cmd_style
        in
        (next_node + 1, node_str :: nodes, edges)
    | Assign (var, aexpr) ->
        let node_id = next_node in
        let var_node_id = node_id + 1 in
        let root_aexpr = var_node_id + 1 in
        let next_node, nodes, edges =
          build_nodes_edges_aexpr root_aexpr nodes edges aexpr
        in
        let node_str =
          Printf.sprintf "    %d [label=\":=\" %s]" node_id cmd_style
        in
        let var_node_str =
          Printf.sprintf "    %d [label=\"%s\" %s]" var_node_id var var_style
        in
        let edge1_str =
          Printf.sprintf "    %d:sw -> %d:n" node_id var_node_id
        in
        let edge2_str = Printf.sprintf "    %d:se -> %d:n" node_id root_aexpr in
        ( next_node + 1,
          node_str :: var_node_str :: nodes,
          edge1_str :: edge2_str :: edges )
    | Seq (cmd1, cmd2) ->
        let node_id = next_node in
        let root_cmd1 = node_id + 1 in
        let next_node, nodes, edges =
          build_nodes_edges_cmd root_cmd1 nodes edges cmd1
        in
        let root_cmd2 = next_node in
        let next_node, nodes, edges =
          build_nodes_edges_cmd root_cmd2 nodes edges cmd2
        in
        let node_str =
          Printf.sprintf "    %d [label=\";\" %s]" node_id cmd_style
        in
        let edge1_str = Printf.sprintf "    %d:sw -> %d:n" node_id root_cmd1 in
        let edge2_str = Printf.sprintf "    %d:se -> %d:n" node_id root_cmd2 in
        (next_node + 1, node_str :: nodes, edge1_str :: edge2_str :: edges)
    | IfThenElse (bexpr, then_cmd, else_cmd) ->
        let node_id = next_node in
        let bexpr_root = node_id + 1 in
        let next_node, nodes, edges =
          build_nodes_edges_bexpr bexpr_root nodes edges bexpr
        in
        let then_cmd_root = next_node in
        let next_node, nodes, edges =
          build_nodes_edges_cmd then_cmd_root nodes edges then_cmd
        in
        let else_cmd_root = next_node in
        let next_node, nodes, edges =
          build_nodes_edges_cmd else_cmd_root nodes edges else_cmd
        in
        let node_str =
          Printf.sprintf "    %d [label=\"if\" %s]" node_id cmd_style
        in
        let edge1_str = Printf.sprintf "    %d:sw -> %d:n" node_id bexpr_root in
        let edge2_str =
          Printf.sprintf "    %d:s -> %d:n [xlabel=\"then\"]" node_id
            then_cmd_root
        in
        let edge3_str =
          Printf.sprintf "    %d:se -> %d:n [xlabel=\"else\"]" node_id
            else_cmd_root
        in
        ( next_node + 1,
          node_str :: nodes,
          edge1_str :: edge2_str :: edge3_str :: edges )
    | While (bexpr, body_cmd) ->
        let node_id = next_node in
        let bexpr_root = node_id + 1 in
        let next_node, nodes, edges =
          build_nodes_edges_bexpr bexpr_root nodes edges bexpr
        in
        let body_cmd_root = next_node in
        let next_node, nodes, edges =
          build_nodes_edges_cmd body_cmd_root nodes edges body_cmd
        in
        let node_str =
          Printf.sprintf "    %d [label=\"while\" %s]" node_id cmd_style
        in
        let edge1_str = Printf.sprintf "    %d:sw -> %d:n" node_id bexpr_root in
        let edge2_str =
          Printf.sprintf "    %d:se -> %d:n" node_id body_cmd_root
        in
        (next_node + 1, node_str :: nodes, edge1_str :: edge2_str :: edges)
  in
  let main_node_id = 0 in
  let root_node = main_node_id + 1 in
  let _, nodes, edges = build_nodes_edges_cmd root_node [] [] cmd in
  let nodes =
    Printf.sprintf "    %d [label=\"main %s %s\" %s]" main_node_id input output
      var_style
    :: nodes
  in
  let edges =
    Printf.sprintf "    %d:s -> %d:n" main_node_id root_node :: edges
  in
  let nodes_str = String.concat "\n" nodes in
  let edges_str = String.concat "\n" edges in
  Printf.sprintf "digraph G {\n%s\n%s\n%s\n}\n" dot_style nodes_str edges_str

let string_of_simple_cmd = function
  | SSkip -> "skip"
  | SAssign (x, aexpr) -> Printf.sprintf "%s := %s" x (string_of_aexpr aexpr)
  | SGuard bexpr -> Printf.sprintf "%s?" (string_of_bexpr bexpr)

let dot_string_of_cfg =
  GenericCFG.dot_string_of_cfg
    (List.map string_of_simple_cmd >> String.concat "\n")
    id
    (fun _ -> None)

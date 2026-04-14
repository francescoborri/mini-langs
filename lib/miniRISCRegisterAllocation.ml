open GenericCFG
open MiniRISCInstructionSet
open MiniRISCCFG
open MiniRISCLiveRegistersAnalysis
open MiniRISCRegUtils

exception RegisterAllocationError of string

(** Cost of reading a register. It is used as metric for the spill cost
    calculation *)
let read_cost = 1.0

(** Cost of writing to a register. It is used as metric for the spill cost
    calculation *)
let write_cost = 3.0

(** First address to use for spilled registers. For readability purposes, it
    starts from a high address to avoid confusion with integer immediate values
*)
let first_addr = 1023

let invalid_color = -1
let first_color = 0
let next_color color = color + 1

module ColorSet = Set.Make (struct
  type t = int

  let compare = compare
end)

(** Searches for the first available color not belonging to [color_set] and
    being less than [num_colors]. If there is no such color, returns [None] *)
let first_available_color color_set num_colors =
  let rec helper color =
    if color >= num_colors then None
    else if ColorSet.mem color color_set then helper (next_color color)
    else Some color
  in
  helper first_color

type interference_graph_node = {
  reg : reg;
  degree : int;
  spill_cost : float;
  color : int;
}
(** A node in the interference graph, representing a register and its properties
    used for the Chaitin-Briggs algorithm *)

module PriorityNodeSet (P : sig
  type t

  val priority : interference_graph_node -> t
end) =
Set.Make (struct
  type t = interference_graph_node

  let compare node1 node2 =
    match compare (P.priority node1) (P.priority node2) with
    | 0 -> compare node1.reg node2.reg
    | comparison -> comparison
end)

module DegreePrioritySet = PriorityNodeSet (struct
  type t = int

  let priority node = node.degree
end)

module SpillCostPrioritySet = PriorityNodeSet (struct
  type t = float

  let priority node = node.spill_cost
end)

type neighbors_map = RegSet.t RegMap.t
(** Represent the edges of an interference graph, mapping nodes to their
    neighbors *)

type interference_graph = {
  degree_priority_set : DegreePrioritySet.t;
  spill_cost_priority_set : SpillCostPrioritySet.t;
  edges : neighbors_map;
}
(** The interference graph, containing the degree and spill cost priority sets
    for efficient node selection during the simplify phase, and the edges for
    updating neighbors' properties when a node is removed *)

let pack_node reg degree spill_cost color = { reg; degree; spill_cost; color }

let pack_graph degree_priority_set spill_cost_priority_set edges =
  { degree_priority_set; spill_cost_priority_set; edges }

(** Returns the set of register appearing the CFG together with the spill cost,
    which is given by spill_cost(reg) = read_cost * num_uses(reg) + write_cost *
    num_defs(reg). This value will be later divided by the degree + 1 of the
    node in the interference graph to get the spill metric used for spill
    decisions in the Chaitin-Briggs algorithm. *)
let collect_regs_with_spill_cost cfg =
  let lookup_cost reg spill_cost_map =
    Option.value (RegMap.find_opt reg spill_cost_map) ~default:0.0
  in
  let spill_cost_map =
    NodeMap.fold
      (fun node instrs spill_cost_map ->
        List.fold_left
          (fun spill_cost_map instr ->
            let spill_cost_map =
              match defining_reg instr with
              | None -> spill_cost_map
              | Some reg ->
                  let cost = lookup_cost reg spill_cost_map in
                  RegMap.add reg (cost +. write_cost) spill_cost_map
            in
            RegSet.fold
              (fun reg spill_cost_map ->
                let cost = lookup_cost reg spill_cost_map in
                RegMap.add reg (cost +. read_cost) spill_cost_map)
              (using_regs instr) spill_cost_map)
          spill_cost_map instrs)
      cfg.code_map RegMap.empty
  in
  let regs =
    RegMap.fold
      (fun reg _ acc -> RegSet.add reg acc)
      spill_cost_map RegSet.empty
  in
  (regs, spill_cost_map)

(** Adds an undirected edge between [reg1] and [reg2] in the interference graph
    represented by [edges]. If [reg1] and [reg2] are the same register, no edge
    is added. *)
let add_edge reg1 reg2 edges =
  if reg1 = reg2 then edges
  else
    let add_edge_directed src dest edges =
      RegMap.update src
        (function
          | None -> Some (RegSet.singleton dest)
          | Some neighbors -> Some (RegSet.add dest neighbors))
        edges
    in
    edges |> add_edge_directed reg1 reg2 |> add_edge_directed reg2 reg1

(** Builds the edges of a complete interference graph, meaning that liveness
    analysis is not exploited to derive more precise information about live
    ranges: every node is connected to every other node (except himself) *)
let build_standard_interference_graph cfg =
  let regs, spill_cost_map = collect_regs_with_spill_cost cfg in
  ( RegSet.fold
      (fun reg edges ->
        let neighbors = RegSet.remove reg regs in
        RegMap.add reg neighbors edges)
      regs RegMap.empty,
    spill_cost_map )

(** Builds the edges of an optimzed interference graph, exploting the liveness
    analysis to derive the live ranges and add edges only between registers that
    are live at the same time *)
let build_optimized_interference_graph cfg =
  let live_regs_info = live_regs_analysis cfg in
  let regs, spill_cost_map = collect_regs_with_spill_cost cfg in
  let edges =
    RegSet.fold
      (fun reg edges -> RegMap.add reg RegSet.empty edges)
      regs RegMap.empty
  in
  ( NodeMap.fold
      (fun node instrs edges ->
        let live_now = (NodeMap.find node live_regs_info).out_regs in
        List.fold_right
          (fun instr (live_now, edges) ->
            let live_now, edges =
              match defining_reg instr with
              | None -> (live_now, edges)
              | Some reg ->
                  ( RegSet.remove reg live_now,
                    RegSet.fold
                      (fun live_reg edges -> add_edge reg live_reg edges)
                      live_now edges )
            in
            let live_now = RegSet.union live_now (using_regs instr) in
            (live_now, edges))
          instrs (live_now, edges)
        |> snd)
      cfg.code_map edges,
    spill_cost_map )

(** Builds the interference graph using the provided [building_fun] to get the
    edges and spill cost map, and then creating the degree and spill cost
    priority sets for the Chaitin-Briggs algorithm. The spill metric for a
    register is (read_cost * num_uses(reg) + write_cost * num_defs(reg)) /
    (degree + 1) *)
let build_interference_graph cfg building_fun =
  let edges, spill_cost_map = building_fun cfg in
  (* If the output register is not present in the spill cost map, then it doesn't
  appear anywhere in the program, meaning that the output variable is undefined
  and the program should fail at runtime. We can't always guarantee this behaviour
  as the output register may be used as a color for other registers, leading to a 
  different semantics. To avoid this, we have to stop the compilation and raise an 
  undefined variable error, even if the undefined variables check is not set *)
  if not (RegMap.mem cfg.output spill_cost_map) then
    raise
      (MiniImpLib.DefinedVariablesAnalysis.UndefinedVariable
         "Undefined output variable");
  let degree_priority_set, spill_cost_priority_set =
    RegMap.fold
      (fun reg neighbors (degree_set, spill_cost_set) ->
        let degree = RegSet.cardinal neighbors in
        let spill_cost =
          match RegMap.find_opt reg spill_cost_map with
          | Some cost -> cost /. float_of_int (degree + 1)
          | None ->
              raise
                (RegisterAllocationError
                   "Detected a register that should have been collected")
        in
        let color = invalid_color in
        let node = pack_node reg degree spill_cost color in
        ( DegreePrioritySet.add node degree_set,
          SpillCostPrioritySet.add node spill_cost_set ))
      edges
      (DegreePrioritySet.empty, SpillCostPrioritySet.empty)
  in
  pack_graph degree_priority_set spill_cost_priority_set edges

(** Removes [node] from an [intereference_graph] but only from the degree and
    spill cost priority sets, taking care also to update the degree and the
    spill cost of the neighbors of the evicted node. This function is useful in
    the simplify phase of the Chaitin-Briggs algorithm, because we just need to
    remember the nodes left on the graph, thus we can avoid to update the edges
    which would be more expensive *)
let soft_remove_node node
    { degree_priority_set; spill_cost_priority_set; edges } =
  let updating_neighbors = RegMap.find node.reg edges in
  let updating_neighbors_fun neighbor =
    if RegSet.mem neighbor.reg updating_neighbors then
      let degree = neighbor.degree in
      let spill_cost = neighbor.spill_cost in
      if degree = 0 then
        raise
          (RegisterAllocationError "The degree of a node cannot be negative")
      else
        {
          neighbor with
          degree = degree - 1;
          spill_cost =
            spill_cost *. float_of_int (degree + 1) /. float_of_int degree;
        }
    else neighbor
  in
  let degree_priority_set =
    degree_priority_set
    |> DegreePrioritySet.filter (fun other -> other.reg <> node.reg)
    |> DegreePrioritySet.map updating_neighbors_fun
  in
  let spill_cost_priority_set =
    spill_cost_priority_set
    |> SpillCostPrioritySet.filter (fun other -> other.reg <> node.reg)
    |> SpillCostPrioritySet.map updating_neighbors_fun
  in
  pack_graph degree_priority_set spill_cost_priority_set edges

(** Re-adds [node] in [interference_graph] after it was removed in the simplify
    phase. This function is useful in the select phase of the Chaitin-Briggs
    algorithm, and it is sufficient to add the node in the degree set because we
    just have to keep track of which node is present on the graph togeter with
    its color; here the set priority is not important, the choice of the degree
    set was arbitrary *)
let soft_readd_node node interference_graph =
  let degree_priority_set =
    DegreePrioritySet.add node interference_graph.degree_priority_set
  in
  { interference_graph with degree_priority_set }

(** Completely removes [node] from [interference_graph], updating both sets and
    the edges. This function is used after an uncolorable node is found in the
    select phase of the Chaitin-Briggs algorithm, which means that we need to
    spill it and safely ignore it in the subsequent run of the algorithm *)
let hard_remove_node node interference_graph =
  let interference_graph = soft_remove_node node interference_graph in
  let updating_neighbors = RegMap.find node.reg interference_graph.edges in
  let edges =
    interference_graph.edges |> RegMap.remove node.reg
    |> RegMap.mapi (fun other other_neighbors ->
        if RegSet.mem other updating_neighbors then
          RegSet.remove node.reg other_neighbors
        else other_neighbors)
  in
  { interference_graph with edges }

(** Implementation of the Chaitin-Briggs algorithm for global register
    allocation. [num_colors] is the number of available colors (registers) and
    [interference_graph] is the interference graph. It returns a mapping from
    registers to their assigned color (or physical register) and the set of
    spilled registers. The algorithm consists of:

    - two simplify phases, where nodes with degree less than the number of
      colors are removed from the graph and pushed on a stack, and if there are
      no such nodes, the node with the lowest spill metric is removed and pushed
      on the stack;
    - a select phase, where nodes are popped from the stack and colored.

    If a node cannot be colored because all its neighbors have different colors,
    it is marked for spilling and removed from the graph for the next iteration
    of the algorithm. *)
let chaitin_briggs_algorithm num_colors interference_graph =
  (* [initial_interference_graph] is the original interference graph which is
      maintained unchanged during all phases except for the hard removal of uncolorable
      nodes, in order to restart the algorithm with the correct graph after a spill decision *)
  let rec simplify_by_degree interference_graph initial_interference_graph
      spilled_regs stack =
    match
      DegreePrioritySet.min_elt_opt interference_graph.degree_priority_set
    with
    | None ->
        select interference_graph initial_interference_graph spilled_regs stack
    | Some node when node.degree < num_colors ->
        let interference_graph = soft_remove_node node interference_graph in
        Stack.push node stack;
        simplify_by_degree interference_graph initial_interference_graph
          spilled_regs stack
    | Some _ ->
        simplify_by_spill_metric interference_graph initial_interference_graph
          spilled_regs stack
  and simplify_by_spill_metric interference_graph initial_interference_graph
      spilled_regs stack =
    match
      SpillCostPrioritySet.min_elt_opt
        interference_graph.spill_cost_priority_set
    with
    | None ->
        raise
          (RegisterAllocationError
             "In the simplify phase using spill metric there should be at \
              least one node")
    | Some node ->
        let interference_graph = soft_remove_node node interference_graph in
        Stack.push node stack;
        simplify_by_degree interference_graph initial_interference_graph
          spilled_regs stack
  and select interference_graph initial_interference_graph spilled_regs stack =
    match Stack.pop_opt stack with
    | None -> (interference_graph, spilled_regs)
    | Some node -> (
        let neighgors = RegMap.find node.reg interference_graph.edges in
        let colored_neighbors =
          DegreePrioritySet.filter
            (fun other -> RegSet.mem other.reg neighgors)
            interference_graph.degree_priority_set
        in
        let neighbors_colors =
          DegreePrioritySet.fold
            (fun other acc -> ColorSet.add other.color acc)
            colored_neighbors ColorSet.empty
        in
        match first_available_color neighbors_colors num_colors with
        | None ->
            let new_initial_interference_graph =
              hard_remove_node node initial_interference_graph
            in
            let spilled_regs = RegSet.add node.reg spilled_regs in
            let stack = Stack.create () in
            simplify_by_degree new_initial_interference_graph
              new_initial_interference_graph spilled_regs stack
        | Some color ->
            let node = { node with color } in
            let interference_graph = soft_readd_node node interference_graph in
            select interference_graph initial_interference_graph spilled_regs
              stack)
  in
  let stack = Stack.create () in
  let { degree_priority_set }, spilled_regs =
    simplify_by_degree interference_graph interference_graph RegSet.empty stack
  in
  (* The register coloring is built in order to guarantee that every register
      can be used (except for special register A and B which are reserved for spilling) *)
  let reg_coloring =
    DegreePrioritySet.fold
      (fun node acc ->
        let mapped_reg =
          if node.color = invalid_color then
            raise
              (RegisterAllocationError
                 "Detected a node that should have been colored")
          else if node.color = 0 then RegIn
          else if node.color = 1 then RegOut
          else Reg (node.color - 2)
        in
        RegMap.add node.reg mapped_reg acc)
      degree_priority_set RegMap.empty
  in
  (reg_coloring, spilled_regs)

let replace_reg reg_map reg =
  Option.value (RegMap.find_opt reg reg_map) ~default:reg

(** Replaces the registers in [instr] according to [reg_map]. If
    [force_dest_to_reg_b] is true, the destination register of the instruction
    is forced to be the B register, which is useful for the spill code
    generation *)
let replace_instr reg_map ?(force_dest_to_reg_b = false) = function
  | Nop -> Nop
  | BinRegOp (op, src1, src2, dest) ->
      let src1 = replace_reg reg_map src1 in
      let src2 = replace_reg reg_map src2 in
      let dest =
        if force_dest_to_reg_b then RegB else replace_reg reg_map dest
      in
      BinRegOp (op, src1, src2, dest)
  | BinImmOp (op, src, imm, dest) ->
      let src = replace_reg reg_map src in
      let dest =
        if force_dest_to_reg_b then RegB else replace_reg reg_map dest
      in
      BinImmOp (op, src, imm, dest)
  | UnOp (op, src, dest) ->
      let src = replace_reg reg_map src in
      let dest =
        if force_dest_to_reg_b then RegB else replace_reg reg_map dest
      in
      UnOp (op, src, dest)
  | Load (src, dest) ->
      let src = replace_reg reg_map src in
      let dest =
        if force_dest_to_reg_b then RegB else replace_reg reg_map dest
      in
      Load (src, dest)
  | LoadImm (imm, dest) ->
      let dest =
        if force_dest_to_reg_b then RegB else replace_reg reg_map dest
      in
      LoadImm (imm, dest)
  | Store (src, dest) ->
      let src = replace_reg reg_map src in
      let dest =
        if force_dest_to_reg_b then RegB else replace_reg reg_map dest
      in
      Store (src, dest)
  | Jump label -> Jump label
  | CondJump (src, then_label, else_label) ->
      let src = replace_reg reg_map src in
      CondJump (src, then_label, else_label)

let color_cfg reg_coloring cfg =
  let colored_code_map =
    NodeMap.map
      (fun instrs -> List.map (replace_instr reg_coloring) instrs)
      cfg.code_map
  in
  { cfg with code_map = colored_code_map }

let spill_instr reg_addr_map instr =
  let spilling_using_regs =
    instr |> using_regs
    |> RegSet.filter (fun reg -> RegMap.mem reg reg_addr_map)
    |> RegSet.elements
  in
  let spilling_defining_reg =
    Option.bind (defining_reg instr) (fun reg ->
        if RegMap.mem reg reg_addr_map then Some reg else None)
  in
  let load_instrs, reg_map =
    match spilling_using_regs with
    | [] -> ([], RegMap.empty)
    | [ reg ] ->
        let addr = RegMap.find reg reg_addr_map in
        ([ LoadImm (addr, RegA); Load (RegA, RegA) ], RegMap.singleton reg RegA)
    | [ reg1; reg2 ] ->
        let addr1 = RegMap.find reg1 reg_addr_map in
        let addr2 = RegMap.find reg2 reg_addr_map in
        let reg_map =
          RegMap.empty |> RegMap.add reg1 RegA |> RegMap.add reg2 RegB
        in
        ( [
            LoadImm (addr1, RegA);
            Load (RegA, RegA);
            LoadImm (addr2, RegB);
            Load (RegB, RegB);
          ],
          reg_map )
    | _ ->
        raise
          (RegisterAllocationError
             "An instruction cannot have more than two source registers")
  in
  let store_instr, reg_map =
    match spilling_defining_reg with
    | None -> ([], reg_map)
    | Some reg ->
        let addr = RegMap.find reg reg_addr_map in
        ([ LoadImm (addr, RegA); Store (RegB, RegA) ], reg_map)
  in
  let force_dest_to_reg_b = Option.is_some spilling_defining_reg in
  load_instrs @ (replace_instr reg_map ~force_dest_to_reg_b instr :: store_instr)

let spill_cfg spilled_regs cfg =
  let reg_addr_map, _ =
    RegSet.fold
      (fun reg (map, last_addr) ->
        (RegMap.add reg (last_addr + 1) map, last_addr + 1))
      spilled_regs (RegMap.empty, first_addr)
  in
  let spilled_code_map =
    NodeMap.map
      (fun instrs ->
        instrs |> List.map (spill_instr reg_addr_map) |> List.flatten)
      cfg.code_map
  in
  let reg_in_addr = RegMap.find_opt RegIn reg_addr_map in
  let reg_out_addr = RegMap.find_opt RegOut reg_addr_map in
  ({ cfg with code_map = spilled_code_map }, reg_in_addr, reg_out_addr)

(** As in and out register can be either colored to different physical registers
    or spilled to memory, this function handle this scenario by possibly adding
    some instructions to move the value from/to the in/out register to/from the
    corresponding memory address *)
let restore_reg_in_out reg_coloring reg_in_addr reg_out_addr cfg =
  let cfg =
    match (reg_in_addr, RegMap.find_opt RegIn reg_coloring) with
    (* It may happen that the in register is unused and thus it is not colored nor spilled *)
    | None, None | None, Some RegIn -> cfg
    | Some addr, None ->
        let store_instrs = [ LoadImm (addr, RegA); Store (RegIn, RegA) ] in
        let code_map =
          NodeMap.update cfg.initial_node
            (function
              | None ->
                  raise
                    (RegisterAllocationError
                       "The initial node should have at least one instruction")
              | Some [ Nop ] -> Some store_instrs
              | Some instrs -> Some (store_instrs @ instrs))
            cfg.code_map
        in
        { cfg with code_map }
    | None, Some reg ->
        let copy_instr = UnOp (Copy, RegIn, reg) in
        let code_map =
          NodeMap.update cfg.initial_node
            (function
              | None ->
                  raise
                    (RegisterAllocationError
                       "The initial node should have at least one instruction")
              | Some [ Nop ] -> Some [ copy_instr ]
              | Some instrs -> Some (copy_instr :: instrs))
            cfg.code_map
        in
        { cfg with code_map }
    | Some _, Some _ ->
        raise
          (RegisterAllocationError
             "The in register cannot be both spilled and colored")
  in
  match (reg_out_addr, RegMap.find_opt RegOut reg_coloring) with
  | None, None ->
      raise
        (RegisterAllocationError
           "The out register should be one of spilled or colored")
  | None, Some RegOut -> cfg
  | Some addr, None ->
      let load_instrs = [ LoadImm (addr, RegA); Load (RegA, RegOut) ] in
      let code_map =
        NodeMap.update cfg.final_node
          (function
            | None ->
                raise
                  (RegisterAllocationError
                     "The final node should have at least one instruction")
            | Some [ Nop ] -> Some load_instrs
            | Some instrs -> Some (instrs @ load_instrs))
          cfg.code_map
      in
      { cfg with code_map }
  | None, Some reg ->
      let copy_instr = UnOp (Copy, reg, RegOut) in
      let code_map =
        NodeMap.update cfg.final_node
          (function
            | None ->
                raise
                  (RegisterAllocationError
                     "The final node should have at least one instruction")
            | Some [ Nop ] -> Some [ copy_instr ]
            | Some instrs -> Some (instrs @ [ copy_instr ]))
          cfg.code_map
      in
      { cfg with code_map }
  | Some _, Some _ ->
      raise
        (RegisterAllocationError
           "The out register cannot be both spilled and colored")

(** Orchestrates the register allocation process by building the interference
    graph using [building_function], running the Chaitin-Briggs algorithm to get
    the register coloring and the spilled registers, spilling the CFG according
    to the spilled registers, coloring the spilled CFG according to the register
    coloring and finally restoring the in and out register if they were spilled.
    Returns the register-allocated CFG together with the set of spilled
    registers and the register coloring, for debugging purposes *)
let reg_alloc num_regs cfg building_function =
  (* A and B register cannot be used for coloring because they are reserved for
      spill code generation *)
  let num_regs = num_regs - 2 in
  let interference_graph = build_interference_graph cfg building_function in
  let reg_coloring, spilled_regs =
    chaitin_briggs_algorithm num_regs interference_graph
  in
  let spilled_cfg, reg_in_addr, reg_out_addr = spill_cfg spilled_regs cfg in
  let colored_cfg = color_cfg reg_coloring spilled_cfg in
  ( restore_reg_in_out reg_coloring reg_in_addr reg_out_addr colored_cfg,
    spilled_regs,
    reg_coloring )

let standard_reg_alloc num_regs cfg =
  reg_alloc num_regs cfg build_standard_interference_graph

let optimized_reg_alloc num_regs cfg =
  reg_alloc num_regs cfg build_optimized_interference_graph

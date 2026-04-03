open Utils
open Utils.ColorText

let num_regs = ref None
let src = ref ""
let out = ref ""
let check_undef_vars = ref false
let liveness_optimization = ref false
let propagate_constant = ref false
let tokens_file = ref ""
let ast_file = ref ""
let mimp_cfg_dot_file = ref ""
let mrisc_cfg_dot_file = ref ""
let mrisc_cfg_with_jumps_dot_file = ref ""
let allocated_mrisc_cfg_dot_file = ref ""
let opt_print_mrisc_live_regs = ref false
let opt_print_reg_alloc_info = ref false
let anon_cnt = ref 0

let anon_fun anon_arg =
  incr anon_cnt;
  match !anon_cnt with
  | 1 -> num_regs := int_of_string_opt anon_arg
  | 2 -> src := anon_arg
  | _ -> ()

let speclist =
  [
    ( [ "num_regs" ],
      Arg.String anon_fun,
      "Available number of registers in the target architecture (at least 4)" );
    ([ "src" ], Arg.String anon_fun, "Source file to execute");
    ([ "-o" ], Arg.Set_string out, "Output file to write the compiled code to");
    ( [ "-u"; "--check-undef-vars" ],
      Arg.Set check_undef_vars,
      "Check for undefined variables in the input program and report an error \
       if any are found" );
    ( [ "-O1"; "--liveness-optimization" ],
      Arg.Set liveness_optimization,
      "Enable optimization using static liveness analysis" );
    ( [ "-O2"; "--propagate-constant" ],
      Arg.Set propagate_constant,
      "Enable constant propagation" );
    ( [ "-pt"; "--print-tokens" ],
      Arg.Set_string tokens_file,
      "Print the list of tokens produced by the lexer in a human-readable \
       format and write it to the specified file" );
    ( [ "-pa"; "--print-ast" ],
      Arg.Set_string ast_file,
      "Print the AST in a human-readable format and write it to the specified \
       file" );
    ( [ "-pmi"; "--print-mimp-cfg" ],
      Arg.Set_string mimp_cfg_dot_file,
      "Render the MiniImp CFG in dot format and write it to the specified file"
    );
    ( [ "-pmr"; "--print-mrisc-cfg" ],
      Arg.Set_string mrisc_cfg_dot_file,
      "Render the MiniRISC CFG in dot format and write it to the specified file"
    );
    ( [ "-pmrj"; "--print-mrisc-cfg-with-jumps" ],
      Arg.Set_string mrisc_cfg_with_jumps_dot_file,
      "Render the MiniRISC CFG with jump instructions in dot format and write \
       it to the specified file" );
    ( [ "-pmra"; "--print-allocated-mrisc-cfg" ],
      Arg.Set_string allocated_mrisc_cfg_dot_file,
      "Render the final MiniRISC CFG after register allocation in dot format \
       and write it to the specified file" );
    ( [ "-plr"; "--print-mrisc-live-regs" ],
      Arg.Set opt_print_mrisc_live_regs,
      "Print the in and out live registers at each program point" );
    ( [ "-pra"; "--print-reg-alloc-info" ],
      Arg.Set opt_print_reg_alloc_info,
      "Print the set of spilled registers and the register coloring after \
       register allocation" );
  ]

let usage_msg = "Usage: mimpc <num_regs> <src> -o <out> [...]"

let print_tokens filename =
  List.map MiniImpLib.Utils.string_of_token
  >> String.concat "\n" >> write_to_file filename

let print_ast filename =
  MiniImpLib.Utils.string_of_ast >> write_to_file filename

let print_mini_imp_cfg filename =
  MiniImpLib.Utils.dot_string_of_cfg >> write_to_file filename

let print_mini_risc_cfg filename =
  MiniRISCLib.Utils.dot_string_of_cfg >> write_to_file filename

let print_mini_risc_cfg_with_jumps filename =
  MiniRISCLib.Utils.dot_string_of_cfg_with_jumps >> write_to_file filename

let print_mrisc_live_regs mini_risc_cfg_with_jumps =
  let live_regs =
    MiniRISCLib.LiveRegistersAnalysis.live_regs_analysis
      mini_risc_cfg_with_jumps
  in
  MiniRISCLib.Utils.string_of_live_regs mini_risc_cfg_with_jumps live_regs
  |> print_endline

let print_reg_alloc_info spilled_regs reg_coloring =
  print_endline (MiniRISCLib.Utils.string_of_spilled_regs spilled_regs);
  print_endline (MiniRISCLib.Utils.string_of_reg_coloring reg_coloring)

let () =
  let speclist = expand_speclist speclist in
  Arg.parse speclist anon_fun usage_msg;
  let bad_num_regs =
    match !num_regs with
    | None -> true
    | Some num_regs when num_regs < 4 -> true
    | _ -> false
  in
  if !src = "" || !out = "" || bad_num_regs || !anon_cnt <> 2 then
    Arg.usage speclist usage_msg
  else
    try
      let num_regs = Option.get !num_regs in
      if !tokens_file <> "" then
        !src |> MiniImpLib.scan |> print_tokens !tokens_file;
      let ast = MiniImpLib.parse !src in
      let ast =
        if !propagate_constant then MiniImpLib.propagate_constants ast else ast
      in
      if !ast_file <> "" then print_ast !ast_file ast;
      let mimp_cfg = MiniImpLib.build_cfg ast in
      if !mimp_cfg_dot_file <> "" then
        print_mini_imp_cfg !mimp_cfg_dot_file mimp_cfg;
      if !check_undef_vars then MiniImpLib.check_undef_vars mimp_cfg;
      let mrisc_cfg = MiniRISCLib.translate_cfg mimp_cfg in
      if !mrisc_cfg_dot_file <> "" then
        print_mini_risc_cfg !mrisc_cfg_dot_file mrisc_cfg;
      let mrisc_cfg = MiniRISCLib.add_jump_instrs mrisc_cfg in
      if !mrisc_cfg_with_jumps_dot_file <> "" then
        print_mini_risc_cfg_with_jumps !mrisc_cfg_with_jumps_dot_file mrisc_cfg;
      if !opt_print_mrisc_live_regs then print_mrisc_live_regs mrisc_cfg;
      let mrisc_cfg, spilled_regs, reg_coloring =
        if !liveness_optimization then
          MiniRISCLib.optimized_reg_alloc num_regs mrisc_cfg
        else MiniRISCLib.standard_reg_alloc num_regs mrisc_cfg
      in
      if !allocated_mrisc_cfg_dot_file <> "" then
        print_mini_risc_cfg_with_jumps !allocated_mrisc_cfg_dot_file mrisc_cfg;
      if !opt_print_reg_alloc_info then
        print_reg_alloc_info spilled_regs reg_coloring;
      let target_code = MiniRISCLib.generate_target_code mrisc_cfg in
      write_to_file !out target_code;
      Printf.printf "%s Successfully compiled %s to %s\n" (color_text green "🗸")
        !src !out
    with
    | Arg.Bad _ -> Arg.usage speclist usage_msg
    | MiniImpLib.Lexer.LexicalError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Lexical error]") msg
    | MiniImpLib.Parser.Error -> prerr_endline (color_text red "[Syntax error]")
    | GenericCFG.CFGError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[CFG error]") msg
    | MiniImpLib.DefinedVariablesAnalysis.UndefinedVariable msg ->
        Printf.eprintf "%s %s\n"
          (color_text red "[Undefined variable(s) error]")
          msg
    | MiniRISCLib.RegisterAllocation.RegisterAllocationError msg ->
        Printf.eprintf "%s %s\n"
          (color_text red "[Register allocation error]")
          msg
    | Sys_error msg ->
        Printf.eprintf "%s %s\n" (color_text red "[IO error]") msg
    | Failure msg -> Printf.eprintf "%s %s\n" (color_text red "[Error]") msg

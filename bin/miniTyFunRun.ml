open Utils
open Utils.ColorText

let src = ref ""
let tokens_file = ref ""
let ast_file = ref ""
let anon_cnt = ref 0

let anon_fun anon_arg =
  incr anon_cnt;
  src := anon_arg

let speclist =
  [
    ([ "src" ], Arg.String anon_fun, "Source file to execute");
    ( [ "-pt"; "--print-tokens" ],
      Arg.Set_string tokens_file,
      "Print the list of tokens produced by the lexer in a human-readable \
       format and write it to the specified file" );
    ( [ "-pa"; "--print-ast" ],
      Arg.Set_string ast_file,
      "Print the AST in a human-readable format and write it to the specified \
       file" );
  ]

let usage_msg = "Usage: mtfun <src> [--tokens] [--ast]"

let print_tokens filename =
  List.map MiniTyFunLib.Utils.string_of_token
  >> String.concat "\n" >> write_to_file filename

let print_ast filename =
  MiniTyFunLib.Utils.string_of_ast >> write_to_file filename

let () =
  let speclist = expand_speclist speclist in
  Arg.parse speclist anon_fun usage_msg;
  if !src = "" || !anon_cnt <> 1 then Arg.usage speclist usage_msg
  else
    try
      if !tokens_file <> "" then
        !src |> MiniTyFunLib.scan |> print_tokens !tokens_file;
      let ast = MiniTyFunLib.parse !src in
      if !ast_file <> "" then print_ast !ast_file ast;
      let () = MiniTyFunLib.run_type_check ast in
      let input = read_int () in
      let output = MiniTyFunLib.run ast input in
      print_endline (string_of_int output)
    with
    | Arg.Bad _ -> Arg.usage speclist usage_msg
    | MiniTyFunLib.Lexer.LexicalError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Lexical error]") msg
    | MiniTyFunLib.Parser.Error ->
        prerr_endline (color_text red "[Syntax error]")
    | MiniTyFunLib.TypeChecker.TypeError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Type error]") msg
    | MiniFunLib.Interpreter.RuntimeError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Runtime error]") msg
    | Sys_error msg ->
        Printf.eprintf "%s %s\n" (color_text red "[IO error]") msg
    | Failure msg -> Printf.eprintf "%s %s\n" (color_text red "[Error]") msg

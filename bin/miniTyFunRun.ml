open Utils.ColorText

let src = ref ""
let opt_print_tokens = ref false
let opt_print_ast = ref false
let anon_cnt = ref 0

let anon_fun anon_arg =
  incr anon_cnt;
  src := anon_arg

let speclist =
  [
    ("src", Arg.String anon_fun, "Source file to execute");
    ( "--tokens",
      Arg.Set opt_print_tokens,
      "Print the list of tokens returned by the scanner" );
    ( "--ast",
      Arg.Set opt_print_ast,
      "Print the parsed program bracketed according to the AST returned by the \
       parser" );
  ]

let usage_msg = "Usage: mtfun <src> [--tokens] [--ast]"

let print_tokens tokens =
  List.iter
    (fun token -> print_endline (MiniTyFunLib.Utils.string_of_token token))
    tokens

let print_ast ast = print_endline (MiniTyFunLib.Utils.string_of_ast ast)

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !src = "" || !anon_cnt <> 1 then Arg.usage speclist usage_msg
  else
    try
      if !opt_print_tokens then print_tokens (MiniTyFunLib.scan !src);
      let ast = MiniTyFunLib.parse !src in
      if !opt_print_ast then print_ast ast;
      let () = MiniTyFunLib.run_type_check ast in
      let input = read_int () in
      let output = MiniTyFunLib.run ast input in
      print_endline (string_of_int output)
    with
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

open Utils
open Utils.ColorText
open MiniImpLib

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

let usage_msg = "Usage: mimp <src> [--tokens] [--ast]"
let print_tokens = List.iter (MiniImpLib.Utils.string_of_token >> print_endline)
let print_ast = MiniImpLib.Utils.string_of_ast >> print_endline

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !src = "" || !anon_cnt <> 1 then Arg.usage speclist usage_msg
  else
    try
      if !opt_print_tokens then !src |> MiniImpLib.scan |> print_tokens;
      let ast = MiniImpLib.parse !src in
      if !opt_print_ast then print_ast ast;
      let input = read_int () in
      let output = MiniImpLib.run ast input in
      Printf.printf "%d\n" output
    with
    | Lexer.LexicalError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Lexical error]") msg
    | Parser.Error -> prerr_endline (color_text red "[Syntax error]")
    | Interpreter.RuntimeError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Runtime error]") msg
    | Sys_error msg ->
        Printf.eprintf "%s %s\n" (color_text red "[IO error]") msg
    | Failure msg -> Printf.eprintf "%s %s\n" (color_text red "[Error]") msg

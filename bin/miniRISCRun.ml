open Utils
open Utils.ColorText
open MiniRISCLib

let num_regs = ref None
let src = ref ""
let anon_cnt = ref 0

let anon_fun anon_arg =
  incr anon_cnt;
  match !anon_cnt with
  | 1 -> num_regs := int_of_string_opt anon_arg
  | 2 -> src := anon_arg
  | _ -> ()

let speclist =
  [
    ( "num_regs",
      Arg.String anon_fun,
      "Available number of registers in the target architecture (at least 4)" );
    ("src", Arg.String anon_fun, "Source file to execute");
  ]

let usage_msg = "Usage: mrisc <num_regs> <src>"

let () =
  Arg.parse speclist anon_fun usage_msg;
  let bad_num_regs =
    match !num_regs with
    | None -> true
    | Some num_regs when num_regs < 4 -> true
    | _ -> false
  in
  if !src = "" || bad_num_regs || !anon_cnt <> 2 then
    Arg.usage speclist usage_msg
  else
    try
      let num_regs = Option.get !num_regs in
      let ast = MiniRISCLib.parse !src in
      let input = read_int () in
      let output = MiniRISCLib.run ast num_regs input in
      Printf.printf "%d\n" output
    with
    | Arg.Bad _ -> Arg.usage speclist usage_msg
    | Lexer.LexicalError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Lexical error]") msg
    | Parser.Error -> prerr_endline (color_text red "[Syntax error]")
    | Interpreter.RuntimeError msg ->
        Printf.eprintf "%s %s\n" (color_text red "[Runtime error]") msg
    | Sys_error msg ->
        Printf.eprintf "%s %s\n" (color_text red "[IO error]") msg
    | Failure msg -> Printf.eprintf "%s %s\n" (color_text red "[Error]") msg

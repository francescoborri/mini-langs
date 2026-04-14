open Var
open GenericCFG
open MiniImpAST
open MiniImpLexer
open MiniImpParser
open MiniImpInterpreter
open MiniImpCFG
open MiniImpDefinedVariablesAnalysis
open MiniImpUtils
module AST = MiniImpAST
module Lexer = MiniImpLexer
module Parser = MiniImpParser
module Interpreter = MiniImpInterpreter
module CFG = MiniImpCFG
module DefinedVariablesAnalysis = MiniImpDefinedVariablesAnalysis
module Utils = MiniImpUtils

let scan src =
  let lexbuf = Lexing.from_channel (open_in src) in
  let rec helper lexbuf acc =
    let token = Lexer.read lexbuf in
    match token with
    | Parser.EOF -> List.rev acc
    | _ -> helper lexbuf (token :: acc)
  in
  helper lexbuf []

let parse src =
  let lexbuf = Lexing.from_channel (open_in src) in
  Parser.prog Lexer.read lexbuf

let run = Interpreter.run
let propagate_constants = CFG.propagate_constants
let build_cfg = CFG.build_cfg

let check_undef_vars cfg =
  match DefinedVariablesAnalysis.analyze cfg with
  | [] -> ()
  | [ { undef_var; error_loc = Expr expr } ] ->
      raise
        (UndefinedVariable
           (Printf.sprintf "Undefined variable '%s' used in '%s'" undef_var
              (MiniImpUtils.string_of_expr expr)))
  | [ { undef_var; error_loc = Output output } ] ->
      raise
        (UndefinedVariable
           (Printf.sprintf "Undefined output variable '%s'" undef_var))
  | errors ->
      let msg =
        errors
        |> List.map (fun { undef_var; error_loc } ->
            match error_loc with
            | Expr expr ->
                Printf.sprintf "  - variable '%s' in expression '%s'" undef_var
                  (MiniImpUtils.string_of_expr expr)
            | Output output ->
                Printf.sprintf "  - output variable '%s'" undef_var)
        |> String.concat "\n"
      in
      raise (UndefinedVariable ("There were undefined variables:\n" ^ msg))

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

let run_defined_vars_analysis cfg =
  match DefinedVariablesAnalysis.analyze cfg with
  | [] -> ()
  | [ { undef_var; expr } ] ->
      raise
        (UndefinedVariable
           (Printf.sprintf "Undefined variable '%s' used in '%s'" undef_var
              (Utils.string_of_expr expr)))
  | errors ->
      let msg =
        errors
        |> List.map (fun { undef_var; expr } ->
            Printf.sprintf "  - variable '%s' in expression '%s'" undef_var
              (Utils.string_of_expr expr))
        |> String.concat "\n"
      in
      raise (UndefinedVariable ("There were undefined variables:\n" ^ msg))

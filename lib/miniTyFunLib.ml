open MiniTyFunAST
open MiniTyFunLexer
open MiniTyFunParser
open MiniTyFunTypeChecker
open MiniTyFunUtils
module AST = MiniTyFunAST
module Lexer = MiniTyFunLexer
module Parser = MiniTyFunParser
module TypeChecker = MiniTyFunTypeChecker
module Utils = MiniTyFunUtils

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

let run_type_check term =
  match type_check term with
  | Left (TFun (TInt, TInt)) -> ()
  | Left ty ->
      raise
        (TypeError
           (Printf.sprintf "Expected type %s, but got %s"
              (string_of_type (TFun (TInt, TInt)))
              (string_of_type ty)))
  | Right msg -> raise (TypeError msg)

let run ast input =
  let rec drop_types = function
    | MiniTyFunAST.Int n -> MiniFunLib.AST.Int n
    | MiniTyFunAST.Bool b -> MiniFunLib.AST.Bool b
    | MiniTyFunAST.Var x -> MiniFunLib.AST.Var x
    | MiniTyFunAST.Fun (x, _, body) -> MiniFunLib.AST.Fun (x, drop_types body)
    | MiniTyFunAST.App (term1, term2) ->
        MiniFunLib.AST.App (drop_types term1, drop_types term2)
    | MiniTyFunAST.Add (term1, term2) ->
        MiniFunLib.AST.Add (drop_types term1, drop_types term2)
    | MiniTyFunAST.Sub (term1, term2) ->
        MiniFunLib.AST.Sub (drop_types term1, drop_types term2)
    | MiniTyFunAST.Mul (term1, term2) ->
        MiniFunLib.AST.Mul (drop_types term1, drop_types term2)
    | MiniTyFunAST.Equal (term1, term2) ->
        MiniFunLib.AST.Equal (drop_types term1, drop_types term2)
    | MiniTyFunAST.LessThan (term1, term2) ->
        MiniFunLib.AST.LessThan (drop_types term1, drop_types term2)
    | MiniTyFunAST.And (term1, term2) ->
        MiniFunLib.AST.And (drop_types term1, drop_types term2)
    | MiniTyFunAST.Or (term1, term2) ->
        MiniFunLib.AST.Or (drop_types term1, drop_types term2)
    | MiniTyFunAST.Not t -> MiniFunLib.AST.Not (drop_types t)
    | MiniTyFunAST.IfThenElse (cond, term1, term2) ->
        MiniFunLib.AST.IfThenElse
          (drop_types cond, drop_types term1, drop_types term2)
    | MiniTyFunAST.Let (x, term1, term2) ->
        MiniFunLib.AST.Let (x, drop_types term1, drop_types term2)
    | MiniTyFunAST.LetFun (f, x, _, body, term) ->
        MiniFunLib.AST.LetFun (f, x, drop_types body, drop_types term)
  in
  MiniFunLib.run (drop_types ast) input

open MiniFunLib
open MiniFunLib.AST

let test = TestUtils.test_predicate "miniFunInterpreter"

let fail =
  TestUtils.test_failure
    (fun exn -> match exn with Interpreter.RuntimeError _ -> true | _ -> false)
    "miniFunInterpreter"

let validate = TestUtils.validate Interpreter.run

let () =
  test "identity" (Fun ("x", Var "x")) (validate 42 42);

  test "add_const" (Fun ("x", Add (Var "x", Int 3))) (validate 7 10);

  test "mul with let"
    (Fun ("x", Let ("y", Int 2, Mul (Var "x", Var "y"))))
    (validate 6 12);

  test "if less than true"
    (Fun ("x", IfThenElse (LessThan (Var "x", Int 3), Int 1, Int 0)))
    (validate 2 1);

  test "if less than false"
    (Fun ("x", IfThenElse (LessThan (Var "x", Int 3), Int 1, Int 0)))
    (validate 3 0);

  test "and or"
    (Fun
       ( "x",
         IfThenElse
           ( And (LessThan (Var "x", Int 10), LessThan (Var "x", Int 5)),
             Int 1,
             Int 0 ) ))
    (validate 3 1);

  test "closure capture"
    (Let ("y", Int 5, Fun ("x", Add (Var "x", Var "y"))))
    (validate 10 15);

  test "shadowing"
    (Fun ("x", Let ("y", Int 5, Add (Var "x", Var "y"))))
    (validate 3 8);

  test "application"
    (Fun ("x", App (Fun ("y", Mul (Var "y", Var "x")), Int 2)))
    (validate 7 14);

  let fact =
    LetFun
      ( "f",
        "n",
        IfThenElse
          ( LessThan (Var "n", Int 2),
            Var "n",
            Mul (Var "n", App (Var "f", Sub (Var "n", Int 1))) ),
        Var "f" )
  in
  test "factorial" fact (validate 5 120);

  test "factorial with 0" fact (validate 0 0);

  fail "boolean function (error)"
    (Fun ("x", And (Var "x", Bool true)))
    (validate 0 0)

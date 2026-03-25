open MiniImpLib
open MiniImpLib.AST

let test = TestUtils.test_predicate "miniImpInterpreter"

let validate = TestUtils.validate MiniImpLib.run

let () =
  test "increment"
    (Prog ("x", "y", Seq (Assign ("y", Add (Var "x", Int 1)), Skip)))
    (validate 5 6);

  test "if true"
    (Prog
       ( "x",
         "y",
         IfThenElse
           (LessThan (Var "x", Int 10), Assign ("y", Int 1), Assign ("y", Int 0))
       ))
    (validate 5 1);

  test "if false"
    (Prog
       ( "x",
         "y",
         IfThenElse
           (LessThan (Var "x", Int 10), Assign ("y", Int 1), Assign ("y", Int 0))
       ))
    (validate 15 0);

  test "while"
    (Prog
       ( "x",
         "y",
         Seq
           ( Assign ("y", Int 0),
             While
               (LessThan (Var "y", Var "x"), Assign ("y", Add (Var "y", Int 1)))
           ) ))
    (validate 3 3);

  test "arithmetic"
    (Prog
       ( "x",
         "y",
         Seq
           (Assign ("y", Add (Mul (Int 2, Var "x"), Sub (Int 10, Int 3))), Skip)
       ))
    (validate 4 ((2 * 4) + (10 - 3)))

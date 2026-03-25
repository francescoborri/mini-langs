open MiniTyFunLib.AST
open MiniTyFunLib.TypeChecker

let test = TestUtils.test_predicate "miniTyFunInterpreter"

let well_typed ty term = type_check term = Left ty
let wrongly_typed term = Either.is_right (type_check term)

let () =
  test "int literal" (Int 42) (well_typed TInt);

  test "bool literal" (Bool true) (well_typed TBool);

  test "binary add" (Add (Int 1, Int 2)) (well_typed TInt);

  test "binary add (type error)" (Add (Int 1, Bool true)) wrongly_typed;

  test "unary not (type error)" (Not (Int 1)) wrongly_typed;

  test "if-then-else"
    (IfThenElse (Bool true, Int 1, Int 2))
    (well_typed TInt);

  test "if cond not bool (type error)"
    (IfThenElse (Int 0, Int 1, Int 2))
    wrongly_typed;

  test "let binding"
    (Let ("x", Int 10, Add (Var "x", Int 5)))
    (well_typed TInt);

  test "unbound var (type error)" (Var "y") wrongly_typed;

  test "function application"
    (App (Fun ("x", TInt, Add (Var "x", Int 1)), Int 2))
    (well_typed TInt);

  test "function application (arg type mismatch)"
    (App (Fun ("x", TBool, IfThenElse (Var "x", Bool true, Bool false)), Int 3))
    wrongly_typed;

  test "letfun recursive"
    (LetFun
       ("f", "x", TFun (TInt, TInt), Add (Var "x", Int 1), App (Var "f", Int 3)))
    (well_typed TInt);

  test "letfun body mismatch (type error)"
    (LetFun
       ( "f",
         "x",
         TFun (TBool, TInt),
         IfThenElse (Var "x", Int 1, Int 2),
         App (Var "f", Int 3) ))
    wrongly_typed

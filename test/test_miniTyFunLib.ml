open MiniTyFunLib

let validate input expected_output src =
  let ast = parse (TestUtils.programs_folder ^ "miniTyFun/" ^ src) in
  TypeChecker.type_check ast = Left (AST.TFun (AST.TInt, AST.TInt))
  && run ast input = expected_output

let test src input expected_output =
  TestUtils.test_predicate "miniTyFunLib"
    (Printf.sprintf "%s %d -> %d" src input expected_output)
    src
    (validate input expected_output)

let () =
  test "arithm_prec.mtfun" 4 2;
  test "fib.mtfun" 5 8;
  test "fun_app_1.mtfun" 3 6;
  test "fun_app_2.mtfun" 2 6;
  test "fun_app_3.mtfun" 3 6;
  test "if_plus.mtfun" 0 2;
  test "add.mtfun" 5 6;
  test "sub.mtfun" 5 3;
  test "mul.mtfun" 4 12;
  test "eq.mtfun" 0 1;
  test "less.mtfun" 9 1;
  test "and.mtfun" 1 1;
  test "or.mtfun" 3 0;
  test "not.mtfun" 5 1;
  test "nested_if.mtfun" 0 2;
  test "let_const_fun.mtfun" 3 8;
  test "let_shadow.mtfun" 3 13;
  test "hof_apply.mtfun" 4 7;
  test "compose.mtfun" 3 7;
  test "double.mtfun" 4 8;
  test "tricky_prec.mtfun" 0 7;
  test "paren_prec.mtfun" 0 9;
  test "nested_letfun.mtfun" 3 7;
  test "factorial_letfun.mtfun" 5 120;
  test "const_fun.mtfun" 10 42;
  test "complex_ops.mtfun" 5 12;
  test "equality_compose.mtfun" 4 1;
  test "pow2.mtfun" 4 16;
  test "bool_to_int.mtfun" 0 99;
  test "nested_app.mtfun" 3 5;
  test "let_in_app.mtfun" 3 8;
  test "multi_arg_sim.mtfun" 5 10;
  test "zero_test.mtfun" 0 0;
  test "identity.mtfun" 9 9

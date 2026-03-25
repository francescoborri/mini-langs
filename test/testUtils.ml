open Utils.ColorText

let programs_folder = Sys.getcwd () ^ "/../../../programs/"

let generic_test module_name test_name test =
  try
    test ();
    Printf.printf "%s  %s - %s\n"
      (color_text green "[Passed 🗸]")
      module_name test_name
  with exn ->
    Printf.printf "%s  %s - %s --> %s\n"
      (color_text red "[Failed ✗]")
      module_name test_name (Printexc.to_string exn)

let test_predicate module_name test_name term predicate =
  generic_test module_name test_name (fun () -> assert (predicate term))

let test_failure check_failure module_name test_name term predicate =
  generic_test module_name test_name (fun () ->
      try
        let _ = predicate term in
        assert false
      with exn ->
        assert (check_failure exn))

let validate execute input expected_output program =
  (execute program input) = expected_output

module To_test = struct
  let run_length = Exercises.Run_length_encoding.run_length
end

let run_length_test_cases =
  let check name expected input =
    Alcotest.(check (list (pair int string)))
      name expected (To_test.run_length input)
  in
  let test_regular _ =
    check "regular"
      [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  [ Alcotest.test_case "regular" `Quick test_regular ]

let () =
  let open Alcotest in
  run "Exercises" [ ("run_length", run_length_test_cases) ]

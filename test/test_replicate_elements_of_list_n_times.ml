module To_test = struct
  let rep =
    Exercises.Replicate_elements_of_list_n_times
    .replicate_elements_of_list_n_times
end

let replicate_elements_of_list_n_times_test_cases =
  let check name expected input times =
    Alcotest.(check (list string)) name expected (To_test.rep input times)
  in
  let test_regular _ =
    check "regular"
      [ "a"; "a"; "b"; "b"; "c"; "c"; "d"; "d"; "e"; "e" ]
      [ "a"; "b"; "c"; "d"; "e" ]
      2
  in
  let test_empty _ = check "empty " [] [] 5 in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises"
    [
      ( "replicate_elements_of_list_n_times",
        replicate_elements_of_list_n_times_test_cases );
    ]

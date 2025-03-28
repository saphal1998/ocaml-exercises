module To_test = struct
  let last_two = Exercises.Last_two_elements_of_list.last_two
end

let last_two_test_cases =
  let check name expected input =
    Alcotest.(check (option (pair int int)))
      name expected (To_test.last_two input)
  in
  let test_regular _ = check "regular" (Some (2, 3)) [ 1; 2; 3 ] in
  let test_one _ = check "one" None [ 1 ] in
  let test_empty _ = check "empty" None [] in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "one" `Quick test_one;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("last_two", last_two_test_cases) ]

module To_test = struct
  let dup = Exercises.Duplicate_elements_of_list.duplicate_elements_of_list
end

let duplicate_elements_of_list_test_cases =
  let check name expected input =
    Alcotest.(check (list string)) name expected (To_test.dup input)
  in
  let test_regular _ =
    check "regular"
      [ "a"; "a"; "b"; "b"; "c"; "c"; "d"; "d"; "e"; "e" ]
      [ "a"; "b"; "c"; "d"; "e" ]
  in
  let test_empty _ = check "empty " [] [] in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises"
    [ ("duplicate_elements_of_list", duplicate_elements_of_list_test_cases) ]

module To_test = struct
  let drop = Exercises.Drop_nth_element_of_list.drop_nth_element_of_list
end

let drop_nth_element_of_list_test_cases =
  let check name expected input n =
    Alcotest.(check (list string)) name expected (To_test.drop input n)
  in
  let test_regular _ =
    check "regular" [ "a"; "c"; "e" ] [ "a"; "b"; "c"; "d"; "e" ] 1
  in
  let test_empty _ = check "empty " [] [] 3 in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises"
    [ ("drop_nth_element_of_list", drop_nth_element_of_list_test_cases) ]

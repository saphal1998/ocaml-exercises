module To_test = struct
  let remove_at =
    Exercises.Remove_kth_element_of_list.remove_kth_element_of_list
  (* Assuming your module and function name *)
end

let remove_at_test_cases =
  let check name expected input k =
    Alcotest.(check (list string)) name expected (To_test.remove_at input k)
  in
  let test_remove_middle _ =
    check "remove middle" [ "a"; "c"; "d" ] [ "a"; "b"; "c"; "d" ] 1
  in
  let test_remove_first _ =
    check "remove first" [ "b"; "c"; "d" ] [ "a"; "b"; "c"; "d" ] 0
  in
  let test_remove_last _ =
    check "remove last" [ "a"; "b"; "c" ] [ "a"; "b"; "c"; "d" ] 3
  in
  let test_remove_out_of_bounds_positive _ =
    check "remove out of bounds positive" [ "a"; "b"; "c"; "d" ]
      [ "a"; "b"; "c"; "d" ] 4
  in
  let test_remove_out_of_bounds_negative _ =
    check "remove out of bounds negative" [ "a"; "b"; "c"; "d" ]
      [ "a"; "b"; "c"; "d" ] (-1)
  in
  let test_remove_empty_list _ = check "remove empty list" [] [] 0 in
  [
    Alcotest.test_case "remove middle" `Quick test_remove_middle;
    Alcotest.test_case "remove first" `Quick test_remove_first;
    Alcotest.test_case "remove last" `Quick test_remove_last;
    Alcotest.test_case "remove out of bounds positive" `Quick
      test_remove_out_of_bounds_positive;
    Alcotest.test_case "remove out of bounds negative" `Quick
      test_remove_out_of_bounds_negative;
    Alcotest.test_case "remove empty list" `Quick test_remove_empty_list;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("remove_at", remove_at_test_cases) ]

module To_test = struct
  let at = Exercises.Nth_element_of_list.at
end

let at_test_cases =
  let check name expected idx input =
    Alcotest.(check (option int)) name expected (To_test.at idx input)
  in
  let test_0 _ = check "0" (Some 1) 0 [ 1; 2; 3 ] in
  let test_out_of_bounds _ = check "out_of_bounds" None 5 [ 1; 2; 3 ] in
  let test_negative_idx _ = check "negative idx" None (-1) [ 1; 2; 3 ] in
  [
    Alcotest.test_case "regular" `Quick test_0;
    Alcotest.test_case "out_of_bounds" `Quick test_out_of_bounds;
    Alcotest.test_case "negative idx" `Quick test_negative_idx;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("at", at_test_cases) ]

module To_test = struct
  let split = Exercises.Split.split
end

let split_test_cases =
  let check name expected input n =
    Alcotest.(check (pair (list string) (list string)))
      name expected (To_test.split input n)
  in
  let test_regular _ =
    check "regular"
      ([ "a"; "b" ], [ "c"; "d"; "e" ])
      [ "a"; "b"; "c"; "d"; "e" ]
      2
  in
  let test_empty _ = check "empty" ([], []) [] 3 in
  let test_split_at_0 _ =
    check "split at 0" ([], [ "a"; "b"; "c" ]) [ "a"; "b"; "c" ] 0
  in
  let test_split_at_end _ =
    check "split at end" ([ "a"; "b"; "c" ], []) [ "a"; "b"; "c" ] 3
  in
  let test_split_past_end _ =
    check "split past end" ([ "a"; "b"; "c" ], []) [ "a"; "b"; "c" ] 4
  in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
    Alcotest.test_case "split at 0" `Quick test_split_at_0;
    Alcotest.test_case "split at end" `Quick test_split_at_end;
    Alcotest.test_case "split past end" `Quick test_split_past_end;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("split", split_test_cases) ]

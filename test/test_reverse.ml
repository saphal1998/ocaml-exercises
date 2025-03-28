module To_test = struct
  let reverse = Exercises.Reverse_list.reverse
end

let reverse_test_cases =
  let check name expected input =
    Alcotest.(check (list int)) name expected (To_test.reverse input)
  in
  let test_regular _ = check "regular" [ 3; 2; 1 ] [ 1; 2; 3 ] in
  let test_empty _ = check "empty" [] [] in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("reverse", reverse_test_cases) ]

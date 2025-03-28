module To_test = struct
  let length = Exercises.Length_of_list.length
end

let length_test_cases =
  let check name expected input =
    Alcotest.(check int) name expected (To_test.length input)
  in
  let test_empty _ = check "empty" 0 [] in
  let test_regular _ = check "regular" 3 [ 1; 2; 3 ] in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("length", length_test_cases) ]

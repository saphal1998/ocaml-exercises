module To_test = struct
  let tail_of_list = Exercises.Tail_of_list.last
end

let last_test_cases =
  let check name expected input =
    Alcotest.(check (option string)) name expected (To_test.tail_of_list input)
  in
  let test_regular _ = check "regular" (Some "c") [ "a"; "b"; "c" ] in
  let test_empty _ = check "empty" None [] in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("last", last_test_cases) ]

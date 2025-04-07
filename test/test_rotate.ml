module To_test = struct
  let rotate = Exercises.Rotate.rotate
end

let rotate_test_cases =
  let check name expected input n =
    Alcotest.(check (list string)) name expected (To_test.rotate input n)
  in
  let test_regular_positive _ =
    check "regular positive"
      [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
      [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]
      3
  in
  let test_empty_list _ = check "empty list" [] [] 3 in
  let test_rotate_zero _ =
    check "rotate zero"
      [ "a"; "b"; "c"; "d"; "e" ]
      [ "a"; "b"; "c"; "d"; "e" ]
      0
  in
  let test_rotate_length _ =
    check "rotate length"
      [ "a"; "b"; "c"; "d"; "e" ]
      [ "a"; "b"; "c"; "d"; "e" ]
      5
  in
  [
    Alcotest.test_case "regular positive" `Quick test_regular_positive;
    Alcotest.test_case "empty list" `Quick test_empty_list;
    Alcotest.test_case "rotate zero" `Quick test_rotate_zero;
    Alcotest.test_case "rotate length" `Quick test_rotate_length;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("rotate", rotate_test_cases) ]

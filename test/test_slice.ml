module To_test = struct
  let slice = Exercises.Slice.slice
end

let slice_test_cases =
  let check name expected input first last =
    Alcotest.(check (list string))
      name expected
      (To_test.slice input first last)
  in
  let test_regular _ =
    check "regular" [ "c"; "d"; "e" ] [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ] 2 4
  in
  let test_empty_input _ = check "empty input" [] [] 0 2 in
  let test_first_greater_than_last _ =
    check "first > last" [] [ "a"; "b"; "c" ] 2 1
  in
  let test_first_at_start _ =
    check "first at start" [ "a"; "b" ] [ "a"; "b"; "c" ] 0 1
  in
  let test_last_at_end _ =
    check "last at end" [ "b"; "c" ] [ "a"; "b"; "c" ] 1 2
  in
  let test_first_and_last_same _ =
    check "first == last" [ "b" ] [ "a"; "b"; "c" ] 1 1
  in
  let test_first_out_of_bounds _ =
    check "first out of bounds" [ "c" ] [ "a"; "b"; "c" ] 2 5
  in
  let test_last_out_of_bounds _ =
    check "last out of bounds" [ "b"; "c" ] [ "a"; "b"; "c" ] 1 5
  in
  let test_both_out_of_bounds _ =
    check "both out of bounds" [] [ "a"; "b"; "c" ] 3 5
  in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty input" `Quick test_empty_input;
    Alcotest.test_case "first > last" `Quick test_first_greater_than_last;
    Alcotest.test_case "first at start" `Quick test_first_at_start;
    Alcotest.test_case "last at end" `Quick test_last_at_end;
    Alcotest.test_case "first == last" `Quick test_first_and_last_same;
    Alcotest.test_case "first out of bounds" `Quick test_first_out_of_bounds;
    Alcotest.test_case "last out of bounds" `Quick test_last_out_of_bounds;
    Alcotest.test_case "both out of bounds" `Quick test_both_out_of_bounds;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("slice", slice_test_cases) ]

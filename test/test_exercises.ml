module To_test = struct
  let tail_of_list = Exercises.Tail_of_list.last
  let last_two = Exercises.Last_two_elements_of_list.last_two
  let at = Exercises.Nth_element_of_list.at
end

let test_last_of_list _ =
  Alcotest.(check (option string))
    "last" (Some "c")
    (To_test.tail_of_list [ "a"; "b"; "c" ])

let test_last_of_list_empty _ =
  Alcotest.(check (option string)) "last" None (To_test.tail_of_list [])

let test_last_two_regular _ =
  Alcotest.(check (option (pair int int)))
    "last_two"
    (Some (2, 3))
    (To_test.last_two [ 1; 2; 3 ])

let test_last_two_one_element _ =
  Alcotest.(check (option (pair int int)))
    "last_two" None (To_test.last_two [ 1 ])

let test_last_two_no_element _ =
  Alcotest.(check (option (pair int int))) "last_two" None (To_test.last_two [])

let test_at_0_element _ =
  Alcotest.(check (option int)) "at" (Some 1) (To_test.at 0 [ 1; 2; 3 ])

let test_at_out_of_bounds_element _ =
  Alcotest.(check (option int)) "at" None (To_test.at 5 [ 1; 2; 3 ])

let test_at_negative_idx_element _ =
  Alcotest.(check (option int)) "at" None (To_test.at (-1) [ 1; 2; 3 ])

let () =
  let open Alcotest in
  run "Exercises"
    [
      ( "last",
        [
          test_case "regular" `Quick test_last_of_list;
          test_case "empty" `Quick test_last_of_list_empty;
        ] );
      ( "last_two",
        [
          test_case "regular" `Quick test_last_two_regular;
          test_case "one" `Quick test_last_two_one_element;
          test_case "empty" `Quick test_last_two_no_element;
        ] );
      ( "at",
        [
          test_case "regular" `Quick test_at_0_element;
          test_case "out_of_bounds" `Quick test_at_out_of_bounds_element;
          test_case "negative idx" `Quick test_at_negative_idx_element;
        ] );
    ]

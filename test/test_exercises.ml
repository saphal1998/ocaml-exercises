module To_test = struct
  let tail_of_list = Exercises.Tail_of_list.last
  let last_two = Exercises.Last_two_elements_of_list.last_two
  let at = Exercises.Nth_element_of_list.at
  let length = Exercises.Length_of_list.length
  let reverse = Exercises.Reverse_list.reverse
  let palindrome = Exercises.Palindrome.palindrome
  let flatten = Exercises.Flatten_list.flatten
  let compress = Exercises.Compress.compress
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

let test_list_length_empty _ =
  Alcotest.(check int) "length" 0 (To_test.length [])

let test_list_length_regular _ =
  Alcotest.(check int) "length" 3 (To_test.length [ 1; 2; 3 ])

let test_list_reverse_regular _ =
  Alcotest.(check (list int))
    "reverse" [ 3; 2; 1 ]
    (To_test.reverse [ 1; 2; 3 ])

let test_list_reverse_empty _ =
  Alcotest.(check (list int)) "reverse" [] (To_test.reverse [])

let test_list_is_palindrome _ =
  Alcotest.(check bool) "palindrome" true (To_test.palindrome [ 1; 2; 1 ])

let test_list_is_palindrome_one _ =
  Alcotest.(check bool) "palindrome" true (To_test.palindrome [ 1 ])

let test_list_is_palindrome_empty _ =
  Alcotest.(check bool) "palindrome" true (To_test.palindrome [])

let test_flatten_list _ =
  Alcotest.(check (list string))
    "flatten"
    [ "a"; "b"; "c"; "d"; "e" ]
    (To_test.flatten
       [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ])

let test_flatten_all_ones _ =
  Alcotest.(check (list string))
    "flatten"
    [ "a"; "b"; "c"; "d"; "e" ]
    (To_test.flatten [ One "a"; One "b"; One "c"; One "d"; One "e" ])

let test_compress _ =
  Alcotest.(check (list string))
    "compress"
    [ "a"; "b"; "c"; "d"; "e" ]
    (To_test.compress
       [
         "a";
         "a";
         "a";
         "b";
         "c";
         "d";
         "d";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
         "e";
       ])

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
      ( "length",
        [
          test_case "regular" `Quick test_list_length_regular;
          test_case "empty" `Quick test_list_length_empty;
        ] );
      ( "reverse",
        [
          test_case "regular" `Quick test_list_reverse_regular;
          test_case "empty" `Quick test_list_reverse_empty;
        ] );
      ( "palindrome",
        [
          test_case "regular" `Quick test_list_is_palindrome;
          test_case "empty" `Quick test_list_is_palindrome_empty;
          test_case "one" `Quick test_list_is_palindrome_one;
        ] );
      ( "flatten",
        [
          test_case "regular" `Quick test_flatten_list;
          test_case "ones" `Quick test_flatten_all_ones;
        ] );
      ("compress", [ test_case "regular" `Quick test_compress ]);
    ]

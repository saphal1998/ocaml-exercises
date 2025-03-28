module To_test = struct
  let decode_rle = Exercises.Decode_rle.decode
end

let decode_rle_test_cases =
  let check name expected input =
    Alcotest.(check (list string)) name expected (To_test.decode_rle input)
  in
  let test_empty _ = check "empty" [] [] in
  let test_single _ = check "single" [ "x" ] [ Exercises.Decode_rle.One "x" ] in
  let test_multiple _ =
    check "multiple"
      [ "x"; "x"; "y"; "z"; "z"; "z"; "z"; "w" ]
      [
        Exercises.Decode_rle.Many (2, "x");
        Exercises.Decode_rle.One "y";
        Exercises.Decode_rle.Many (4, "z");
        Exercises.Decode_rle.One "w";
      ]
  in
  let test_mixed _ =
    check "mixed"
      [ "a"; "b"; "b"; "b"; "c"; "c"; "c"; "c"; "d" ]
      [
        Exercises.Decode_rle.One "a";
        Exercises.Decode_rle.Many (3, "b");
        Exercises.Decode_rle.Many (4, "c");
        Exercises.Decode_rle.One "d";
      ]
  in
  [
    Alcotest.test_case "empty" `Quick test_empty;
    Alcotest.test_case "single" `Quick test_single;
    Alcotest.test_case "multiple" `Quick test_multiple;
    Alcotest.test_case "mixed" `Quick test_mixed;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("decode_rle", decode_rle_test_cases) ]

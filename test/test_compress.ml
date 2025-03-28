module To_test = struct
  let compress = Exercises.Compress.compress
end

let compress_test_cases =
  let check name expected input =
    Alcotest.(check (list string)) name expected (To_test.compress input)
  in
  let test_regular _ =
    check "regular"
      [ "a"; "b"; "c"; "d"; "e" ]
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
      ]
  in
  [ Alcotest.test_case "regular" `Quick test_regular ]

let () =
  let open Alcotest in
  run "Exercises" [ ("compress", compress_test_cases) ]

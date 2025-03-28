module To_test = struct
  let pack = Exercises.Pack_consecutive_duplicates.pack
end

let pack_test_cases =
  let check name expected input =
    Alcotest.(check (list (list string))) name expected (To_test.pack input)
  in
  let test_regular _ =
    check "regular"
      [
        [ "a"; "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d"; "d" ];
        [ "e"; "e"; "e"; "e" ];
      ]
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
  in
  [ Alcotest.test_case "regular" `Quick test_regular ]

let () =
  let open Alcotest in
  run "Exercises" [ ("pack", pack_test_cases) ]

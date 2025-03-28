module To_test = struct
  let flatten = Exercises.Flatten_list.flatten
end

let flatten_test_cases =
  let check name expected input =
    Alcotest.(check (list string)) name expected (To_test.flatten input)
  in
  let test_regular _ =
    check "regular"
      [ "a"; "b"; "c"; "d"; "e" ]
      [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  in
  let test_ones _ =
    check "ones"
      [ "a"; "b"; "c"; "d"; "e" ]
      [ One "a"; One "b"; One "c"; One "d"; One "e" ]
  in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "ones" `Quick test_ones;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("flatten", flatten_test_cases) ]

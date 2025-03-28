module To_test = struct
  let modified_rle = Exercises.Modified_rle.mod_rle
end

let modified_rle_test_cases =
  let modified_rle_to_string_list rle_list =
    List.map
      (fun rle ->
        match rle with
        | Exercises.Modified_rle.One x -> "One " ^ x
        | Many (count, x) -> Printf.sprintf "Many (%d, %s)" count x)
      rle_list
  in
  let check name expected input =
    Alcotest.(check (list string))
      name expected
      (modified_rle_to_string_list (To_test.modified_rle input))
  in
  let test_regular _ =
    check "regular"
      [
        "Many (4, a)";
        "One b";
        "Many (2, c)";
        "Many (2, a)";
        "One d";
        "Many (4, e)";
      ]
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  [ Alcotest.test_case "regular" `Quick test_regular ]

let () =
  let open Alcotest in
  run "Exercises" [ ("modified_rle", modified_rle_test_cases) ]

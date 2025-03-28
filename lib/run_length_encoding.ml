let rec run_length_helper acc curr curr_count lst =
  match lst with
  | [] -> acc @ [ (curr_count, curr) ]
  | x :: rest ->
      if x = curr then run_length_helper acc curr (curr_count + 1) rest
      else run_length_helper (acc @ [ (curr_count, curr) ]) x 1 rest

let run_length lst =
  match lst with
  | [] -> []
  | [ x ] -> [ (1, x) ]
  | x :: rest -> run_length_helper [] x 1 rest

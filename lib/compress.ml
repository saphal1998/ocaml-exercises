let rec compress_helper acc current lst =
  match lst with
  | [] -> acc
  | x :: rest ->
      if current = x then compress_helper acc current rest
      else compress_helper (acc @ [ x ]) x rest

let compress lst =
  match lst with
  | [] -> []
  | [ x ] -> [ x ]
  | x :: rest -> compress_helper [ x ] x rest

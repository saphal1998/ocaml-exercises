let rotate lst n =
  let rec aux acc idx lst =
    match lst with
    | [] -> (List.rev acc, [])
    | hd :: tl as l ->
        if idx = 0 then (List.rev acc, l) else aux (hd :: acc) (idx - 1) tl
  in
  let first, last = aux [] n lst in
  last @ first

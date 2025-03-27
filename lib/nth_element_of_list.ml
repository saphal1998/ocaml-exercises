let rec at idx lst =
  let get_first lst = match lst with [] -> None | x :: _ -> Some x in
  let rest lst =
    match lst with [] | [ _ ] -> None | _ :: remain -> Some remain
  in
  match idx with
  | 0 -> get_first lst
  | x when x < 0 -> None
  | x when x > 0 ->
      at (idx - 1) (match rest lst with Some l -> l | None -> [])
  | _ -> None

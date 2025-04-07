let rec split_helper lst curr curr_idx split_pt =
  match lst with
  | [] -> (List.rev curr, [])
  | hd :: tl ->
      if curr_idx < split_pt then
        split_helper tl (hd :: curr) (curr_idx + 1) split_pt
      else (List.rev curr, lst)

let split lst n = split_helper lst [] 0 n

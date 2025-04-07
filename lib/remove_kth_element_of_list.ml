let remove_kth_element_of_list lst n =
  let rec aux lst lst_new curr k =
    match lst with
    | [] -> List.rev lst_new
    | hd :: tl ->
        if curr = k then aux tl lst_new (curr + 1) k
        else aux tl (hd :: lst_new) (curr + 1) k
  in
  aux lst [] 0 n

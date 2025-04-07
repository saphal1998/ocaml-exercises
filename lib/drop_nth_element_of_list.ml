let rec drop_nth_element_of_list_helper original reduced curr n =
  match original with
  | [] -> reduced
  | hd :: tl ->
      if curr = n then drop_nth_element_of_list_helper tl reduced 0 n
      else drop_nth_element_of_list_helper tl (reduced @ [ hd ]) (curr + 1) n

let drop_nth_element_of_list lst n = drop_nth_element_of_list_helper lst [] 0 n

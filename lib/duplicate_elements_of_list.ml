let rec duplicate_elements_of_list lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd :: hd :: duplicate_elements_of_list tl

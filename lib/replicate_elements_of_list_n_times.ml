let rec replicate el n =
  match n with x when x > 0 -> el :: replicate el (x - 1) | _ -> []

let rec replicate_elements_of_list_n_times lst n =
  match lst with
  | [] -> []
  | hd :: tl -> replicate hd n @ replicate_elements_of_list_n_times tl n

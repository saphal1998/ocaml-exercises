let rec equal l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _ :: _ | _ :: _, [] -> false
  | l1 :: rest1, l2 :: rest2 -> l1 = l2 && equal rest1 rest2

let palindrome lst =
  let rev = Reverse_list.reverse lst in
  equal lst rev

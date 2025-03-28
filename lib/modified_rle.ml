type 'a rle = One of 'a | Many of int * 'a

let record el el_count =
  match el_count with
  | 0 -> None
  | 1 -> Some (One el)
  | x -> if x > 0 then Some (Many (el_count, el)) else None

let unwrap el = match el with Some x -> [ x ] | None -> []

let rec mod_rle_helper acc curr curr_count lst =
  match lst with
  | [] -> acc @ unwrap (record curr curr_count)
  | hd :: tl ->
      if hd = curr then mod_rle_helper acc curr (curr_count + 1) tl
      else mod_rle_helper (acc @ unwrap (record curr curr_count)) hd 1 tl

let mod_rle lst =
  match lst with
  | [] -> []
  | [ x ] -> [ One x ]
  | hd :: tl -> mod_rle_helper [] hd 1 tl

type 'a rle = One of 'a | Many of int * 'a

let rec make count x =
  match count with
  | 0 -> []
  | rest -> if rest > 0 then [ x ] @ make (count - 1) x else []

let expand el = match el with One x -> [ x ] | Many (count, x) -> make count x

let rec decode lst =
  match lst with [] -> [] | hd :: tl -> expand hd @ decode tl

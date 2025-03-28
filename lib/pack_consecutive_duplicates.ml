let rec pack_helper current el rest =
  match rest with
  | [] -> [ current ]
  | hd :: tl ->
      if hd = el then pack_helper (hd :: current) hd tl
      else [ current ] @ pack_helper [ hd ] hd tl

let pack lst =
  match lst with
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | x :: rest -> pack_helper [ x ] x rest

let rec slice_helper lst curr first last =
  match lst with
  | [] -> []
  | hd :: tl ->
      if curr >= first && curr <= last then
        [ hd ] @ slice_helper tl (curr + 1) first last
      else if curr < first then slice_helper tl (curr + 1) first last
      else []

let slice lst first last =
  if last < first then [] else slice_helper lst 0 first last

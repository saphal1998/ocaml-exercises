let rec helper acc lst =
  match lst with [] -> acc | _ :: rest -> helper (acc + 1) rest

let length lst = helper 0 lst

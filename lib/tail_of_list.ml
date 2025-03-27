let rec last lst =
  match lst with [] -> None | [ x ] -> Some x | _ :: rest -> last rest

type 'a node = One of 'a | Many of 'a node list

let rec flatten lst =
  match lst with
  | [] -> []
  | One x :: rest -> x :: flatten rest
  | Many x :: rest -> flatten x @ flatten rest

let rec reverse lst =
  match lst with [] -> [] | x :: rest -> reverse rest @ [ x ]

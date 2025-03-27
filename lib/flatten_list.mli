type 'a node = One of 'a | Many of 'a node list

val flatten : 'a node list -> 'a list

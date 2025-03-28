type 'a rle = One of 'a | Many of int * 'a

val decode : 'a rle list -> 'a list

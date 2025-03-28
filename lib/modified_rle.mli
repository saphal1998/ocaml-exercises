type 'a rle = One of 'a | Many of int * 'a

val mod_rle : 'a list -> 'a rle list

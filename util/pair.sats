
datatype pair (a:t@ype, b:t@ype) = Pair of (a, b)

//fun {a,b:t@ype} fprint_pair (FILEref, pair (a, b)): void
//overload fprint with fprint_pair

fun {a,b:t@ype} pair_print (pair (a, b), a -> void, b -> void): void
overload show with pair_print

fun {a,b:t@ype} pair_fst (pair (a, b)): a
fun {a,b:t@ype} pair_snd (pair (a, b)): b

overload fst with pair_fst
overload snd with pair_snd
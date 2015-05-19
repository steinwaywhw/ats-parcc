staload "./util.sats"


datatype pair (a:t@ype, b:t@ype) = Pair of (a, b)


fun {a,b:t@ype} pair_show (pair (a, b), a -> void, b -> void): void

fun {a,b:t@ype} pair_fst (pair (a, b)): a
fun {a,b:t@ype} pair_snd (pair (a, b)): b


overload show with pair_show
overload fst with pair_fst
overload snd with pair_snd
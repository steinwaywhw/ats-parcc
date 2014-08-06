staload "token.sats"

datatype pair (a:t@ype, b:t@ype) = Pair of (a, b)

fun fprint_pair_token (FILEref, pair (token, token)): void
overload fprint with fprint_pair_token
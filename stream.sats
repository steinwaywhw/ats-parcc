staload "maybe.sats"

datatype stream (a:t@ype) =
	| Cons of (a, lazy (stream a))
	| Nil of ()


fun {a:t@ype} head (lazy (stream a)): maybe a
fun {a:t@ype} take (lazy (stream a), int): lazy (stream a)
fun {a:t@ype} tail (lazy (stream a)): lazy (stream a)
fun {a:t@ype} drop (lazy (stream a), int): lazy (stream a)
fun {a:t@ype} get (lazy (stream a), int): maybe a

fun {a,b:t@ype} {r:t@ype} zip (lazy (stream a), lazy (stream b), (a, b) -<cloref1> r): lazy (stream r)
fun {a:t@ype} filter (lazy (stream a), a -<cloref1> bool): lazy (stream a)
fun {a:t@ype} interleave (lazy (stream a), lazy (stream a)): lazy (stream a)
fun {a:t@ype} merge (lazy (stream a), lazy (stream a), (a, a) -<cloref1> int): lazy (stream a)
fun {a:t@ype} {b:t@ype} map (lazy (stream a), a -<cloref1> b): lazy (stream b)
fun {a:t@ype} {b:t@ype} foldr (lazy (stream a), b, (a, b) -<cloref1> b): b 

fun {a:t@ype} fprint_stream (out: FILEref, s: lazy (stream a), len: int, f: (FILEref, a) -> void): void
overload fprint with fprint_stream
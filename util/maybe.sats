staload "./util.sats"

datatype maybe (a:t@ype) = 
	| Nothing of ()
	| Just of (a)

fun {a,b:t@ype} maybe_bind (maybe a, a -> b): maybe b
fun {a:t@ype} maybe_show (maybe a, a -> void): void

//overload bind with maybe_bind
overload show with maybe_show

////

fun maybe_is_nothing {a:t@ype} (maybe a): bool
fun maybe_is_just {a:t@ype} (maybe a): bool
fun {a:t@ype} maybe_unjust (maybe a): a


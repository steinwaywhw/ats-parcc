

datatype maybe (a:t@ype) = 
	| Nothing of ()
	| Just of (a)


fun maybe_is_nothing {a:t@ype} (maybe a): bool
fun maybe_is_just {a:t@ype} (maybe a): bool
fun {a:t@ype} maybe_unjust (maybe a): a
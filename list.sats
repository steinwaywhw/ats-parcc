staload "maybe.sats"

datatype list (a:t@ype) =
	| Cons of (a, list a)
	| Nil  of ()

fun {a:t@ype} empty  (list (a)): bool
fun {a:t@ype} append (list (a), a): list (a)
fun {a:t@ype} head   (list (a)): maybe (a)
fun {a:t@ype} tail   (list (a)): list (a)
fun {a:t@ype} drop   (list (a), int): list (a)
fun {a:t@ype} concat (list (a), list (a)): list (a)

fun {a:t@ype} {b:t@ype} map (list a, a -> b): list b 
fun {a:t@ype} {b:t@ype} foldl (b, list a, (a, b) -> b): b
fun {a:t@ype} {b:t@ype} foldr (list a, b, (a, b) -> b): b


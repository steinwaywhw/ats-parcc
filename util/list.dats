staload "util/list.sats"
staload "util/maybe.sats"
#include "share/atspre_staload.hats"

implement {a} list_empty  (xs) = 
	case+ xs of 
	| Cons _ => false
	| Nil  _ => true

implement {a} list_append (xs, x) = Cons (x, xs)
implement {a} list_head (xs) = 
	case+ xs of 
	| Cons (x, _) => Just (x)
	| Nil _		  => Nothing ()

implement {a} list_tail (xs) =
	case+ xs of 
	| Cons (_, xs) => xs 
	| Nil () 	   => Nil ()

implement {a} list_drop (xs, i) = 
	if list_empty xs || i = 0
	then xs 
	else list_drop (list_tail xs, i - 1)

implement {a} list_concat (xs, ys) = 
	if list_empty xs 
	then ys 
	else list_concat (list_tail xs, list_append (ys, maybe_unjust (list_head xs))) 

implement {a} {b} list_map (xs, f) =
	case+ xs of 
	| Nil () => Nil ()
	| Cons (x, xs) => Cons (f x, list_map (xs, f))

implement {a} {b} list_foldr (xs, base, f) =
	case+ xs of 
	| Nil () => base
	| Cons (x, xs) => f (x, list_foldr (xs, base, f)) 

implement {a} {b} list_foldl (xs, base, f) =
	case+ xs of 
	| Nil () => base
	| Cons (x, xs) => list_foldl (xs, f (x, base), f)

implement {a} list_take (xs, len) = 
	if list_empty xs || len = 0
	then Nil ()
	else Cons (maybe_unjust (list_head xs), list_take (list_tail xs, len - 1))

implement {a} list_filter (xs, f) =
	case+ xs of 
	| Nil () => Nil ()
	| Cons (x, xs) => 
		if f x
		then list_filter (xs, f)
		else Cons (x, list_filter (xs, f))

implement {a} list_foreach (xs, f) =
	case+ xs of 
	| Nil () => ()
	| Cons (x, xs) => list_foreach (xs, f) where { val _ = f(x) }

implement {a,b} {r} list_zip (xs, ys, f) = 
	if list_empty xs || list_empty ys
	then Nil ()
	else Cons (f (maybe_unjust (list_head xs), maybe_unjust (list_head ys)), zip (list_tail xs, list_tail ys, f))

implement list_toint (xs, base) = let 
	fun loop (current: int, rest: list (int)): int = 
		case+ xs of 
		| Nil () => current
		| Cons (x, xs) => loop (current * base + x, xs)
in 
	loop (0, xs)
end
#include "share/atspre_staload.hats"
staload "util/list.sats"
staload "util/maybe.sats"

implement {a} list_empty  (xs) = 
	case+ xs of 
	| Cons _ => false
	| Nil  _ => true

#define :: Cons

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
	case+ list_head xs of 
	| Just x  => list_concat (list_tail xs, list_append (ys, x))
	| Nothing => ys

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
	if len = 0
	then Nil ()
	else case+ list_head xs of 
		| Just x  => Cons (x, list_take (list_tail xs, len - 1))
		| Nothing => Nil ()

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
	case+ list_head xs of
	| Nothing => Nil ()
	| Just x => 
		case+ list_head ys of 
		| Nothing => Nil ()
		| Just y => Cons (f (x, y), list_zip (list_tail xs, list_tail ys, f))

implement list_len (xs) = 
	if list_empty xs
	then 0
	else 1 + list_len (list_tail xs)

implement list_reverse (xs) = 
	case+ xs of 
	| x :: xs => list_reverse (xs)
	| Nil ()  => Nil ()

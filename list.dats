staload "list.sats"
staload "maybe.sats"

implement {a} empty  (xs) = 
	case+ xs of 
	| Cons _ => false
	| Nil  _ => true

implement {a} append (xs, x) = Cons (x, xs)
implement {a} head (xs) = 
	case+ xs of 
	| Cons (x, _) => Just (x)
	| Nil _		  => Nothing ()

implement {a} tail (xs) =
	case+ xs of 
	| Cons (_, xs) => xs 
	| Nil () 	   => Nil ()

implement {a} drop (xs, i) = 
	if empty xs || i = 0
	then xs 
	else drop (tail xs, i - 1)

implement {a} concat (xs, ys) = 
	if empty xs 
	then ys 
	else concat (tail xs, append (ys, h)) 
		where {
			val- Just (h) = head (xs)
		}

implement {a} {b} map (xs, f) =
	case+ xs of 
	| Nil () => Nil ()
	| Cons (x, xs) => Cons (f x, map (xs, f))

implement {a} {b} foldr (xs, base, f) =
	case+ xs of 
	| Nil () => base
	| Cons (x, xs) => f (x, foldr (xs, base, f)) 

implement {a} {b} foldl (base, xs, f) =
	case+ xs of 
	| Nil () => base
	| Cons (x, xs) => foldl (f (x, base), xs, f)
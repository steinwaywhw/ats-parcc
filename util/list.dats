#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "./util.sats"
staload "./list.sats"
staload "./maybe.sats"

#define :: Cons

implement {a} list_eq (xs, ys, f) = 
	case+ (xs, ys) of 
	| (x::xs, y::ys) => if f(x, y) then true else false 
	| (Nil(), Nil()) => true 
	| (_, _) => false

implement {a} list_is_prefix (xs, pre, f) = 
	case+ (xs, pre) of 
	| (x::xs, p::pre) => if f(x,p) then list_is_prefix (xs, pre, f) else false
	| (_, Nil ()) => true 
	| (_, _) => false

implement {a} list_find (xs, obj, cmp) = 
	case+ xs of 
	| Nil _ => Nothing ()
	| x :: xs => 
		if cmp (x, obj)
		then Just 0
		else maybe_bind (list_find (xs, obj, cmp), lam x => x + 1)

implement {a} list_get (xs, index) = 
	if (index < 0) 
	then Nothing ()
	else if (index = 0)
	then list_head xs
	else list_get (list_tail xs, index - 1)

implement list_empty {a} (xs) = 
	case+ xs of 
	| Cons _ => false
	| Nil  _ => true

implement {a} list_len  (xs) = 
	case+ xs of 
	| Nil () => 0
	| Cons (x, xs) => list_len (xs) + 1

implement {a} list_prepend (xs, x) = Cons (x, xs)
implement {a} list_append (xs, c) = 
	case+ xs of 
	| Nil () => c :: Nil ()
	| x :: xs => x :: list_append (xs, c)

implement {a} list_head (xs) = 
	case+ xs of 
	| Cons (x, _) => Just (x)
	| Nil _		  => Nothing ()

implement {a} list_tail (xs) =
	case+ xs of 
	| Cons (_, xs) => xs 
	| Nil () 	   => Nil ()

implement {a} list_drop (xs, i) = 
	if list_empty xs || i <= 0
	then xs 
	else list_drop (list_tail xs, i - 1)

implement {a} list_concat (xs, ys) =  
	case+ ys of 
	| y :: ys => list_concat (list_append (xs, y), ys)
	| Nil _   => xs 

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
	if len <= 0
	then Nil ()
	else case+ list_head xs of 
		| Just x  => Cons (x, list_take (list_tail xs, len - 1))
		| Nothing => Nil ()

implement {a} list_filter (xs, f) =
	case+ xs of 
	| Nil () => Nil ()
	| Cons (x, xs) => 
		if f x
		then Cons (x, list_filter (xs, f))
		else list_filter (xs, f)

implement {a} list_filter_clo (xs, f) =
	case+ xs of 
	| Nil () => Nil ()
	| Cons (x, xs) => 
		if f x
		then Cons (x, list_filter_clo (xs, f))
		else list_filter_clo (xs, f)

implement {a} list_foreach (xs, f) =
	case+ xs of 
	| Nil () => ()
	| Cons (x, xs) => list_foreach (xs, f) where { val _ = f x }


implement {a} list_foreach_clo (xs, f) =
	case+ xs of 
	| Nil () => ()
	| Cons (x, xs) => list_foreach_clo (xs, f) where { val _ = f x }

implement {a,b} {r} list_zip (xs, ys, f) = 
	case+ list_head xs of
	| Nothing () => Nil ()
	| Just x => 
		case+ list_head ys of 
		| Nothing () => Nil ()
		| Just y => Cons (f (x, y), list_zip (list_tail xs, list_tail ys, f))

implement {a} list_reverse (xs) = 
	case+ xs of 
	| x :: xs => list_append (list_reverse (xs), x)
	| Nil ()  => Nil ()

implement {a} list_show (xs, f) = 
	case+ xs of 
	| Nil _      => show "nil"
	| x :: xs    => () where {
		val _ = f x 
		val _ = show ":"
		val _ = list_show (xs, f)
	}

implement list_show_int (xs) = list_show<int> (xs, lam x => show x)
implement list_show_char (xs) = list_show<char> (xs, lam x => show x)
implement list_show_string (xs) = list_show<string> (xs, lam x => show x)


	


////

staload _ = "maybe.dats"

implement main0 () = () where {
	val xs = 1 :: 2 :: Nil() : list int 
	val ys = 'c' :: 'd' :: Nil () : list char
	val zs = "asda" :: "asdddd" :: Nil () : list string
	val seprator = "\n-----------------------\n" : string
	val _ = show xs
	val _ = show seprator
	val _ = show ys 
	val _ = show seprator
	val _ = show zs

	val _ = show seprator
	val _ = show (list_append (xs, 9))
	val _ = show seprator
	val _ = bind (list_head xs, lam x => 0 where { val _ = print_int x })
	val _ = show seprator
	val _ = show (list_tail xs)
	val _ = show seprator
	val _ = show (list_drop (xs, 1))
	val _ = show seprator
	val _ = show (list_drop (xs, 5))
	val _ = show seprator
	val _ = show (list_take (xs, 1))
	val _ = show seprator
	val _ = show (list_take (xs, 5))
	val _ = show seprator
	val _ = show (list_concat (xs, 2 :: 3 :: Nil()))
	val _ = show seprator
	val _ = show (list_filter (xs, lam x => x != 2))
	val _ = show seprator
	val _ = foreach (xs, lam x => show (2 * x))
	val _ = show seprator
	val _ = show (list_reverse (xs))
	val _ = show seprator
}
#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "./util.sats"
staload "./stream.sats"
staload "./maybe.sats"


#define :: Cons

implement {a} stream_interleave (xs, ys) =
	case+ !xs of
	| x :: xs => $delay x :: stream_interleave (ys, xs)
	| Nil ()  => ys
 
implement {a} stream_merge (xs, ys, f) = 
	case+ !xs of 
	| Nil _ => ys
	| x :: xs0 =>
		case+ !ys of 
		| Nil _ => $delay Nil ()
		| y :: ys0 => 
			if f (x, y) < 0
			then $delay x :: stream_merge (xs0, ys, f)
			else $delay y :: stream_merge (xs, ys0, f)

implement {a} stream_head (xs) = 
	case+ !xs of
	| x :: _ => Just x
	| Nil () => Nothing () 

implement {a} stream_tail (xs) = 
	case+ !xs of
	| _ :: xs => xs
	| Nil _   => $delay Nil ()
 
implement {a} stream_drop (xs, n) =
	if n <= 0
	then xs 
	else stream_drop (stream_tail (xs), n-1)

implement {a} stream_take (xs, n) = 
	if n <= 0 
	then $delay (Nil ())
	else case+ !xs of
		| Nil ()  => $delay Nil ()
		| x :: xs => $delay x :: stream_take (stream_tail (xs), n-1)
 
implement {a} stream_get (xs, n) = 
	case+ !xs of 
	| Nil ()  => Nothing ()
	| x :: xs =>
		if n = 0 
		then Just x
		else stream_get (xs, n-1)
 
implement {a,b} {r} stream_zip (xs, ys, f) = 
	case+ !xs of 
	| Nil ()  => $delay Nil ()
	| x :: xs =>
		case+ !ys of 
		| Nil ()  => $delay Nil ()
		| y :: ys => $delay f (x, y) :: stream_zip (xs, ys, f)
 
implement {a} stream_filter (xs, f) = 
	case+ !xs of 
	| Nil _   => $delay Nil ()
	| x :: xs =>
		if f x 
		then $delay x :: stream_filter (xs, f)
		else stream_filter (xs, f)
 
implement {a} {b} stream_map (xs, f) = 
	case+ !xs of 
	| Nil ()  => $delay Nil () 
	| x :: xs => $delay f(x) :: stream_map (xs, f)

implement {a} {b} stream_foldr (xs, base, f) = 
	case+ !xs of 
	| Cons (x, xs) => f(x, stream_foldr (xs, base, f))
	| Nil ()       => base 

implement {a} {b} stream_foldl (xs, base, f) = 
	case+ !xs of 
	| Cons (x, xs) => stream_foldl (xs, f (x, base), f)
	| Nil ()       => base 

implement stream_empty {a} (xs) = 
	case+ !xs of 
	| Nil () => true
	| _      => false

implement {a,b} {r} stream_zip (xs, ys, f) =
	case+ !xs of 
	| Nil _ => $delay Nil ()
	| x :: xs =>
		case+ !ys of 
		| Nil _ => $delay Nil ()
		| y :: ys => $delay f (x, y) :: stream_zip (xs, ys, f)

implement {a} stream_foreach (xs, f) =
	case+ !xs of 
	| Nil () => ()
	| x :: xs => stream_foreach (xs, f) where { val _ = f x }

implement {a} stream_show (xs, len, f) = 
	case+ !xs of 
	| Nil _ => show "nil"
	| _ when len <= 0 => show "nil"
	| x :: xs => () where {
		val _ = f x 
		val _ = show ":"
		val _ = stream_show<a> (xs, len - 1, f)
	}

implement stream_show_int (s, len) = stream_show<int> (s, len, lam x => show x)
implement stream_show_char (s, len) = stream_show<char> (s, len, lam x => show x)

//implement main0 () = ()
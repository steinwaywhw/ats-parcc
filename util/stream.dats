staload "util/stream.sats"
staload "util/maybe.sats"

#define :: Cons

implement {a} stream_interleave (xs, ys) = 
	$delay (
		case+ !xs of
		| Cons (x, xs) => (x :: stream_interleave (ys, xs))
		| Nil () => !ys
	)
 
implement {a} stream_merge (xs, ys, f) = 
	$delay (
		let
			val- Cons (x, xs0) = !xs
			val- Cons (y, ys0) = !ys
		in if f (x, y) < 0 
			then Cons (x, stream_merge (xs0, ys, f)) 
			else Cons (y, stream_merge (ys0, xs, f))
		end
	)

implement {a} stream_head (xs) = case+ !xs of
	| Cons (x, xs) => Just (x)
	| Nil () => Nothing ()
 
implement {a} stream_tail (xs) = case+ !xs of
	| Cons (_, xs) => xs
	| Nil () => $delay (Nil ())
 
implement {a} stream_drop (xs, n) =
	if n <= 0
	then xs 
	else stream_drop (stream_tail (xs), n-1)

implement {a} stream_take (xs, n) = 
	if n <= 0 
	then $delay (Nil ())
	else case+ stream_head (xs) of
		| Nothing () => $delay (Nil ())
		| Just (x) => $delay (x :: stream_take (stream_tail (xs), n-1))
 
implement {a} stream_get (xs, n) = case+ !xs of 
	| Nil () => Nothing ()
	| Cons (_, _) =>
		if n = 0 
		then stream_head (xs)
		else stream_get (stream_tail (xs), n-1)
 
implement {a,b} {r} stream_zip (xs, ys, f) = 
	case+ stream_head (xs) of 
	| Nothing () => $delay Nil ()
	| Just (x) =>
		case+ stream_head (ys) of 
		| Nothing () => $delay Nil ()
		| Just (y) => $delay (f (x, y) :: stream_zip (stream_tail (xs), stream_tail (ys), f))
 
implement {a} stream_filter (xs, f) = 
	case+ stream_head xs of 
	| Nothing () => $delay Nil ()
	| Just (x) =>
		if f (x) 
		then $delay (x :: stream_filter (stream_tail (xs), f))
		else stream_filter (stream_tail (xs), f)
 
implement {a} fprint_stream (out, xs, len, f) = case+ !xs of 
	| Nil () => fprint (out, "nil")
	| Cons (x, xs) => 
		if len > 0 
		then () where {
			val _ = f (out, x)
			val _ = fprint (out, ":")
			val _ = fprint (out, xs, len-1, f)
		}

implement {a} {b} stream_map (xs, f) = case+ !xs of 
	| Nil () => $delay Nil () 
	| Cons (x, xs) => $delay (f(x) :: stream_map (xs, f))

implement {a} {b} stream_foldr (xs, base, f) = 
	case+ !xs of 
	| Cons (x, xs) => f(x, stream_foldr (xs, base, f))
	| Nil () => base 

implement {a} {b} stream_foldl (xs, base, f) = 
	case+ !xs of 
	| Cons (x, xs) => stream_foldl (xs, f (x, base), f)
	| Nil () => base 

implement {a} stream_empty (xs) = 
	case+ !xs of 
	| Nil () => true
	| _ => false

implement {a} stream_append (xs, x) = $delay Cons (x, xs)

implement {a} stream_concat (xs, ys) =
	case+ !xs of 
	| Nil () => ys
	| Cons (x, xs) => stream_concat (xs, $delay Cons (x, ys))

implement {a,b} {r} stream_zip (xs, ys, f) =
	if stream_empty xs || stream_empty ys 
	then $delay Nil ()
	else $delay Cons (
		f (maybe_unjust (stream_head xs), maybe_unjust (stream_head ys)), 
		stream_zip (stream_tail xs, stream_tail ys, f))

implement {a} stream_foreach (xs, f) =
	case+ !xs of 
	| Nil () => ()
	| Cons (x, xs) => stream_foreach (xs, f) where { val _ = f x }

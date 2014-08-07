#define ATS_DYNLOADFLAG 0

staload "stream.sats"
staload "maybe.sats"

#define :: Cons

implement {a} interleave (xs, ys) = 
	$delay (
		case+ !xs of
		| Cons (x, xs) => (x :: interleave (ys, xs))
		| Nil () => !ys
	)
 
implement {a} merge (xs, ys, f) = 
	$delay (
		let
			val- Cons (x, xs0) = !xs
			val- Cons (y, ys0) = !ys
		in if f (x, y) < 0 
			then Cons (x, merge (xs0, ys, f)) 
			else Cons (y, merge (ys0, xs, f))
		end
	)

implement {a} head (xs) = case+ !xs of
	| Cons (x, xs) => Just (x)
	| Nil () => Nothing ()
 
implement {a} tail (xs) = case+ !xs of
	| Cons (_, xs) => xs
	| Nil () => $delay (Nil ())
 
implement {a} drop (xs, n) =
	if n <= 0
	then xs 
	else drop (tail (xs), n-1)

implement {a} take (xs, n) = 
	if n <= 0 
	then $delay (Nil ())
	else case+ head (xs) of
		| Nothing () => $delay (Nil ())
		| Just (x) => $delay (x :: take (tail (xs), n-1))
 
implement {a} get (xs, n) = case+ !xs of 
	| Nil () => Nothing ()
	| Cons (_, _) =>
		if n = 0 
		then head (xs)
		else get (tail (xs), n-1)
 
implement {a,b} {r} zip (xs, ys, f) = 
	case+ head (xs) of 
	| Nothing () => $delay Nil ()
	| Just (x) =>
		case+ head (ys) of 
		| Nothing () => $delay Nil ()
		| Just (y) => $delay (f (x, y) :: zip (tail (xs), tail (ys), f))
 
implement {a} filter (xs, f) = 
	case+ head xs of 
	| Nothing () => $delay Nil ()
	| Just (x) =>
		if f (x) 
		then $delay (x :: filter (tail (xs), f))
		else filter (tail (xs), f)
 
implement {a} fprint_stream (out, xs, len, f) = case+ !xs of 
	| Nil () => fprint (out, "nil")
	| Cons (x, xs) => 
		if len > 0 
		then () where {
			val _ = f (out, x)
			val _ = fprint (out, ":")
			val _ = fprint (out, xs, len-1, f)
		}

implement {a} {b} map (xs, f) = case+ !xs of 
	| Nil () => $delay Nil () 
	| Cons (x, xs) => $delay (f(x) :: map (xs, f))

implement {a} {b} foldr (xs, base, f) = 
	case+ !xs of 
	| Cons (x, xs) => f(x, foldr (xs, base, f))
	| Nil () => base 

#include "share/atspre_staload.hats"

staload "parcc.sats"
staload "util/pair.sats"
staload "util/maybe.sats"
staload "lexing/token.sats"
staload sm = "util/stream.sats"
staload "util/list.sats"
staload "string/string.sats"
staload "file/location.sats"
staload "util/unit.sats"

staload _ = "util/stream.dats"
staload _ = "file/location.dats"
staload _ = "util/list.dats"
staload _ = "util/pair.dats"

#define :: Cons

//
// pargen
//
implement {i} {o} succeed (ret) = 
	lam input => Success (ret, input)

implement {i} {o} fail () = 
	lam input => Failure (input)



//
// parcom
//
implement {i} {o} alt (a, b) = 
	lam (input) => 
		case+ apply (a, input) of 
		| Success (ret, input) => Success (ret, input)
		| Failure _ => apply (b, input)
		

implement {i} {o1,o2} seq (a, b) = 
	bind (a, 
		lam (x) => 
			bind (b, lam (y) => succeed (Pair (x, y))))

implement {i} {o} sat (p, f) = 
	lam (input) => 
		case+ apply (p, input) of 
		| Failure _ => Failure (input) 
		| Success (ret, rest) =>
			if f (ret)
			then Success (ret, rest) 
			else Failure (input)

implement {i} {o} opt (p) =
	lam (input) => 
		case+ apply (p, input) of 
		| Success (ret, rest) => Success (Just (ret), rest)
		| Failure (input) => Success (Nothing (), input)

implement {i} {o} rpt1 (p) = red (seq (p, rpt0 p), lam x => fst x :: snd x)

implement {i} {o} rpt0 (p) = 
	lam (input) =>
		case+ apply (p, input) of 
		| Failure (input) => Success (Nil (), input)
		| Success (ret, rest) => Success (ret :: xs, rest) where {
			val Success (xs, rest) = apply (rpt0 p, rest)
		} 

implement {i} {o} rptn (p, n) = 
	if n <= 0
	then succeed (Nil ())
	else bind (p, 
		lam (x) =>
			bind (rptn (p, n-1), lam (y) => succeed (x :: y)))

implement {i} {o} apply (p, s) = p (s)

implement {i} {o1,o2} bind (p, f) = 
	lam (input) => 
		case+ apply (p, input) of 
			| Success (ret, input) => apply (f (ret), input)
			| Failure (input) => Failure (input)


implement {i, o} {r} red (p, f) = bind (p, lam x => succeed (f x))


implement {i} {o} skip (p) = bind (p, lam x => succeed (Unit ()))


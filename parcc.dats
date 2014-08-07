#include "share/atspre_staload.hats"

staload "parcc.sats"
staload "util/pair.sats"
staload "util/maybe.sats"
staload "lexing/token.sats"
staload sm = "util/stream.sats"
staload "util/list.sats"
staload "string/string.sats"
staload "file/location.sats"

staload _ = "util/stream.dats"
staload _ = "file/location.dats"
staload _ = "util/list.dats"
staload _ = "util/pair.dats"

#define :: Cons

//
// pargen
//
implement {i} {o} succeed (ret) = 
	lam (input) => Success (ret, input)



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

implement {i} {o} rpt1 (p) = 
	bind (p, 
		lam (x) =>
			bind (rpt0 p, lam (y) => succeed (x :: y)))

implement {i} {o} rpt0 (p) = 
	bind (opt (rpt1 p), lam (x) => 
		case+ x of 
		| Just (x) => succeed (x)
		| Nothing () => succeed (Nil ())
	)

implement {i} {o} apply (p, s) = p (s)

implement {i} {o1,o2} bind (p, f) = 
	lam (input) => 
		case+ apply (p, input) of 
			| Success (ret, input) => apply (f (ret), input)
			| Failure (input) => Failure (input)


implement {i, o} {r} red (p, f) = 
	bind (p, lam (x) => succeed (f x))


implement lit_char (match) = 
	lam (sm) => 
		case+ !sm of
		| $sm.Nil () => Failure (sm)
		| $sm.Cons (p, rest) => 
			if fst p = match
			then Success (token_make (TChar (fst p), snd p), rest)
			else Failure ($delay ($sm.Cons (p, rest)))
		

implement lit_string (match) = let 
	fun genpar (index: int):<cloref1> parser (lazy ($sm.stream (pair (char, location))), location) =
		if index = len (match) - 1
		then red (lit_char match[index], lam (x) => token_get_location x)
		else red (
				seq (lit_char match[index], genpar (index+1)), 
				lam (x) => loc where {
					val Pair (tk, loc) = x 
					val range = range_merge (location_range (token_get_location tk), location_range loc)
					val loc = Loc (location_file loc, range)
				}
			)
in 
	red (genpar (0), lam (x) => token_make (TString (match), x))
end
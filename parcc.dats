staload "parcc.sats"
staload "pair.sats"
staload "maybe.sats"
staload "token.sats"
staload sm = "stream.sats"
staload "list.sats"
staload "string.sats"
staload "location.sats"

staload _ = "stream.dats"
staload _ = "location.dats"
staload _ = "list.dats"

#include "share/atspre_staload.hats"

#define ATS_DYNLOADFLAG 0
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


implement lit_char (match) = 
	lam (sm) => 
		case+ !sm of
		| $sm.Nil () => Failure (sm)
		| $sm.Cons (p, rest) => let
			val Pair (c, loc) = p 
		in 
			if c = match
			then Success (TChar (c, loc), rest)
			else Failure ($delay ($sm.Cons (p, rest)))
		end

implement lit_string (match) = let 
	fun genpar (index: int):<cloref1> parser (lazy ($sm.stream (pair (char, location))), location) =
		if index = $extfcall (int, "strlen", match) - 1
		then bind (lit_char match[index], lam (x) => succeed (get_token_location x))
		else bind (
				seq (lit_char match[index], genpar (index+1)), 
				lam (x) => ret where {
					val Pair (tk, loc) = x 
					val range = range_merge (location_range (get_token_location tk), location_range loc)
					val loc = Loc (location_file loc, range)
					val ret = succeed (loc)
				}
			)
in 
	bind (genpar (0), lam (x) => succeed (TString (match, x)))
end
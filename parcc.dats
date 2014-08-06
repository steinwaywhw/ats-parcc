staload "parcc.sats"
staload "pair.sats"
staload "maybe.sats"
staload "token.sats"
staload sm = "stream.sats"
staload "list.sats"
staload "string.sats"

staload _ = "stream.dats"
staload _ = "location.dats"
staload _ = "list.dats"


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

implement lit_string (match) = 
	lam (sm) => let 
		val original = sm 
		val len = strlen (match)

		var min: position
		var max: position

		fun loop (index: int): bool = 
			if index = len 
			then true
			else case+ !sm of 
				| Nil () => false 
				| Cons (x, xs) => let 
					val Pair (ch, pos) = x 
					val _ = if index = 0 then min := pos else if compare (min, pos) > 0 then min := pos
					val _ = if index = 0 then max := pos else if compare (max, pos) < 0 then max := pos
				in
					(x = match[index]) && loop (index + 1)
				end
	in 
		if loop (0)
		then Success (TString (match), FileRange ())

staload "parcc.sats"
staload "pair.sats"
staload "maybe.sats"
staload "token.sats"
staload sm = "stream.sats"

staload _ = "stream.dats"
staload _ = "location.dats"

assume parser (i, o) = lazy ($sm.stream i) -<cloref1> result (i, o)


//
// pargen
//
implement succeed {i} {o} (ret) = 
	lam (input) =<cloref1> Success (ret, input)



//
// parcom
//
implement alt {i} {o} (a, b) = 
	lam (input) =<cloref1> 
		case+ apply (a, input) of 
		| Success (ret, input) => Success (ret, input)
		| Failure _ => apply (b, input)
		

implement seq {i} {o1,o2} (a, b) = 
	bind (a, 
		lam (x) =<cloref1> 
			bind (b, lam (y) =<cloref1> succeed (Pair (x, y))))

implement sat {i} {o} (p, f) = 
	lam (input) =<cloref1> 
		case+ apply (p, input) of 
		| Failure _ => Failure (input) 
		| Success (ret, rest) =>
			if f (ret)
			then Success (ret, rest) 
			else Failure (input)

implement opt {i} {o} (p) =
	lam (input) =<cloref1> 
		case+ apply (p, input) of 
		| Success (ret, rest) => Success (Just (ret), rest)
		| Failure (input) => Success (Nothing (), input)

implement rpt1 {i} {o} (p) = seq (p, p^+)
implement rpt0 {i} {o} (p) = (p^+)^?

implement apply {i} {o} (p, s) = p (s)

implement bind {i} {o1,o2} (p, f) = 
	lam (input) =<cloref1> 
		case+ apply (p, input) of 
			| Success (ret, input) => apply (f (ret), input)
			| Failure (input) => Failure (input)


implement lit_char (match) = 
	lam (sm) =<cloref1> 
		case+ !sm of
		| $sm.Nil () => Failure (sm)
		| $sm.Cons (p, rest) => let
			val Pair (c, loc) = p 
		in 
			if c = match
			then Success (TChar (c, loc), rest)
			else Failure ($delay ($sm.Cons (p, rest)))
		end


staload "./prelude.sats"
staload "./parcc.sats"
staload st = "./string.sats"
staload sm = "./stream.sats"
staload ut = "./unit.sats"
staload pr = "./pair.sats"

assume parser (i, o) = string -<cloref1> result (i, o)


//
// pargen
//
implement succeed {i} {o} (ret) = 
	lam (sm) =<cloref1> Success (ret, sm)

implement literal {i} (match) = 	
	lam (str) =<cloref1> let 
		val len = min (strlen match, strlen str)
		var head = substr (str, 0, len)
		val tail = substr (str, len)
	in 
		if equal ($sm.head (sm), match) then
			Success ($ut.Unit (), $sm.tail (sm))
		else 
			Failure (sm)
	end

//
// parcom
//
implement alt {i} {o} (a, b) = 
	lam (s) =<cloref1> 
		let 
			val r = apply (a, s)
		in case+ r of 
			| Success (_, _) => r
			| Failure (_) => apply (b, s)
		end

implement seq {i} {o1,o2} (a, b) = 
	bind (a, 
		lam (x) =<cloref1> 
			bind (b, lam (y) =<cloref1> succeed ($pr.Pair (x, y))))

implement apply {i} {o} (p, s) = p (s)

implement bind {i} {o1,o2} (p, f) = 
	lam (sm) =<cloref1> 
		case+ apply (p, sm) of 
			| Success (ret, sm) => apply (f (ret), sm)
			| Failure (sm) => Failure (sm)
		
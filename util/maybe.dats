staload "./util.sats"
staload "./maybe.sats"
#define ATS_DYNLOADFLAG 0

implement {a,b} maybe_bind (m, f) = 
	case+ m of 
	| Nothing _ => Nothing ()
	| Just (x)  => Just (f x)

implement {a} maybe_show (m, f) = 
	case+ m of 
	| Nothing _ => show "Nothing"
	| Just x 	=> () where {
		val () = show "Just ("
		val () = f x
		val () = show ")"
	}



////
implement maybe_is_nothing {a} (m) = 
	case+ m of 
	| Nothing _ => true
	| Just _ => false

implement maybe_is_just {a} (m) = ~ maybe_is_nothing (m)

implement {a} maybe_unjust (m) = v where { val- Just (v) = m }

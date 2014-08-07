staload "util/maybe.sats"


implement maybe_is_nothing {a} (m) = 
	case+ m of 
	| Nothing _ => true
	| Just _ => false

implement maybe_is_just {a} (m) = ~ maybe_is_nothing (m)

implement {a} maybe_unjust (m) = v where { val- Just (v) = m }
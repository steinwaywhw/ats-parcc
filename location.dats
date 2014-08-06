#define ATS_DYNLOADFLAG 0

staload "location.sats"

implement fprint_position (out, pos) = 
	case+ pos of Pos (line, col) => $extfcall (void, "fprintf", out, "%d:%d", line, col)

implement fprint_location (out, loc) = 
	case+ loc of
	| None ()  				 => fprint (out, "none")
	| Stdin () 			 	 => fprint (out, "stdin")
	| FilePos (path, a) 	 => () where {
		val _ = $extfcall (void, "fprintf", out, "%s [", path)
		val _ = fprint (out, a)
		val _ = fprint (out, "]")
	}
	| FileRange (path, a, b) => () where {
		val _ = $extfcall (void, "fprintf", out, "%s [", path)
		val _ = fprint (out, a)
		val _ = fprint (out, " - ")
		val _ = fprint (out, b)
		val _ = fprint (out, "]")
	}

implement compare_pos_pos (p1, p2) = let 
	val Pos (l1, c1) = p1
	val Pos (l2, c2) = p2
in 
	if l1 != l2
	then l1 - l2
	else p1 - p2
end

implement isnone (x:location): bool = 
	case+ x of 
	| None _ => true
	| _ => false
implement isstdin (x:location): bool = 
	case+ x of 
	| Stdin _ => true
	| _ => false
implement ispos (x:location): bool = 
	case+ x of 
	| FilePos _ => true
	| _ => false
implement isrange (x:location): bool = 
	case+ x of 
	| FileRange _ => true
	| _ => false

implement eq_loc_loc (x, y) = 
	case+ x of
	| None _ => isnone y
	| Stdin _ => isstdin y 
	| FilePos _ => ispos y
	| FileRange _ => isrange y

end (* LOCAL IN END*)

implement merge (x, y) = 
	
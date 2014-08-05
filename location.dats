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
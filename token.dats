staload "libc/SATS/stdio.sats"
staload "token.sats"
staload "location.sats"
staload _ = "location.dats"

#define ATS_DYNLOADFLAG 0

implement fprint_token (out, t) = 
	case+ t of 
	| TComment (c, l) => () where {
		val _ = $extfcall (void, "fprintf", out, "TComment (%s) at ", c)
		val _ = fprint (out, l)
	}
	| TSpace (l) => () where {
		val _ = fprint (out, "TSpace at ")
		val _ = fprint (out, l)
	}
	| TChar (c, l) => () where {
		val _ = $extfcall (void, "fprintf", out, "TChar (%c) at ", c)
		val _ = fprint (out, l)
	}
	| TString (s, l) => () where {
		val _ = $extfcall (void, "fprintf", out, "TString (%s) at ", s)
		val _ = fprint (out, l)
	}
	| TId (id, l) => () where {
		val _ = $extfcall (void, "fprintf", out, "TId (%s) at ", id)
		val _ = fprint (out, l)
	}
	| TInt (i, l) => () where {
		val _ = $extfcall (void, "fprintf", out, "TInt (%d) at ", i)
		val _ = fprint (out, l)
	}


implement get_token_file (t) = location_file (get_token_location t)
implement get_token_range (t) = location_range (get_token_location t)
implement get_token_location (t) = 
	case+ t of 
	| TComment (_, l) 	=> l 
	| TSpace (l) 		=> l 
	| TChar (_, l) 		=> l
	| TString (_, l) 	=> l 
	| TId (_, l) 		=> l 
	| TInt (_, l) 		=> l
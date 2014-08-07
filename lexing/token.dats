staload "libc/SATS/stdio.sats"
staload "lexing/token.sats"
staload "file/location.sats"

staload _ = "file/location.dats"

assume token = '{node = tokennode, loc = location}

implement fprint_token (out, t) = () where {
	val _ = fprint (out, t.node)
	val _ = fprint (out, " at ")
	val _ = fprint (out, t.loc)
}

implement fprint_tokennode (out, t) =
	case+ t of 
	| TComment s => $extfcall (void, "fprintf", out, "TComment (%s)", s)
	| TSpace  () => fprint (out, "TSpace")
	| TChar    c => $extfcall (void, "fprintf", out, "TChar (%c)", c)
	| TString  s => $extfcall (void, "fprintf", out, "TString (%s)", s)
	| TId 	  id => $extfcall (void, "fprintf", out, "TId (%s)", id)
	| TInt 	   i => $extfcall (void, "fprintf", out, "TInt (%d)", i)

implement token_get_file (t) = location_file t.loc
implement token_get_range (t) = location_range t.loc
implement token_get_location (t) = t.loc
implement token_get_node (t) = t.node
implement token_make (t, loc) = '{node = t, loc = loc}


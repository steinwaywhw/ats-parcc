staload "libc/SATS/stdio.sats"
staload "lexing/token.sats"
staload "file/location.sats"
staload "util/maybe.sats"
staload "util/util.sats"
staload "util/list.sats"

#include "share/atspre_staload.hats"

staload _ = "util/list.dats"
staload _ = "file/location.dats"
staload _ = "util/maybe.dats"

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

implement fprint_token_list (out, ts, len) = 
	if empty (ts) || len = 0
	then fprint (out, "nil")
	else fprint_token_list (out, tail ts, len - 1) where {
		val _ = fprint_token (out, maybe_unjust (head ts))
		val _ = fprint (out, '\n')
	}
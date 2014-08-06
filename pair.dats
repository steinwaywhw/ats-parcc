#define ATS_DYNLOADFLAG 0
staload "pair.sats"
staload "token.sats"
staload _ = "token.dats"

implement fprint_pair_token (out, p) = () where {
	val _ = fprint (out, "Pair (")
	val Pair (a, b) = p
	val _ = fprint (out, a)
	val _ = fprint (out, ", ")
	val _ = fprint (out, b)
	val _ = fprint (out, ")")
} 
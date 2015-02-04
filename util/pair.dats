staload "util/util.sats"
staload "util/pair.sats"
#define ATS_DYNLOADFLAG 0

(*implement {a,b} fprint_pair (out, p) = () where {
	val _ = fprint (out, "Pair (")
	val Pair (a, b) = p
//	val _ = fprint<a> (out, a)
	val _ = fprint (out, ", ")
//	val _ = fprint<b> (out, b)
	val _ = fprint (out, ")")
} 
*)

implement {a,b} pair_print (p, fa, fb) = 
{
	val _ = show "("
	val _ = fa (fst p)
	val _ = show ", "
	val _ = fb (snd p)
	val _ = show ")"
}

implement {a,b} pair_fst (p) = f where { val Pair (f, _) = p }
implement {a,b} pair_snd (p) = s where { val Pair (_, s) = p }
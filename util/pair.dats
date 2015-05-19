staload "./util.sats"
staload "./pair.sats"
#define ATS_DYNLOADFLAG 0


implement {a,b} pair_show (p, fa, fb) = () where {
	val _ = show "Pair ("
	val _ = fa (fst p)
	val _ = show ", "
	val _ = fb (snd p)
	val _ = show ")"
}

implement {a,b} pair_fst (p) = f where { val Pair (f, _) = p }
implement {a,b} pair_snd (p) = s where { val Pair (_, s) = p }
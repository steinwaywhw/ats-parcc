#include "share/atspre_staload.hats"

staload "file.sats"
staload "pair.sats"
staload "location.sats"
staload "libc/SATS/stdio.sats"
staload sm = "stream.sats"
staload "parcc.sats"
staload "token.sats"

staload _ = "token.dats"
staload _ = "file.dats"
staload _ = "stream.dats"
staload _ = "location.dats"
staload _ = "parcc.dats"
staload _ = "pair.dats"

extern fun test (path: string): void
implement test (path) = () where {
	val sm = filestream (path)

	val lex = seq (lit_char '#', lit_char 'i')
	val t = apply (lex, sm)

	fun p (out: FILEref, p: pair (char, location)): void = () where {
		val Pair (c, loc) = p 
		val _ = fprint (out, c)
		val _ = fprint (out, " ")
		val _ = fprint (out, loc)
		val _ = fprint (out, "\n")
	}
	
	//val _ = $sm.fprint<pair(char, location)> (stdout_ref, sm, 2, p)
	val- Success (v, _) = t
	val _ = fprint (stdout_ref, v)
}



implement main0 () = () where {
	val _ = test("./_test.dats")
}
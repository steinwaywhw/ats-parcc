#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0
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
	val sm = append_location (append_position (filestream path, Pos (1, 1)), path)

	val lex = alt (literal "#incccc", literal "#include") 
	val lex = alt (lex, literal "hhh")
	val t = apply (lex, sm)

	fun p (out: FILEref, p: pair (char, location)): void = () where {
		val Pair (c, loc) = p 
		val _ = fprint (out, c)
		val _ = fprint (out, " ")
		val _ = fprint (out, loc)
		val _ = fprint (out, "\n")
	}

	val _ = case+ t of 
		| Success (v, _) => fprint (stdout_ref, v)
		| Failure (v) => $sm.fprint<pair(char, location)> (stdout_ref, v, 10, p)
}



implement main0 () = () where {
	val _ = test("./_test.dats")
}
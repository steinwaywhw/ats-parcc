#include "share/atspre_staload.hats"
staload "file/file.sats"
staload "util/pair.sats"
staload "file/location.sats"
staload "libc/SATS/stdio.sats"
staload sm = "util/stream.sats"
staload "parcc.sats"
staload "lexing/token.sats"

staload _ = "lexing/token.dats"
staload _ = "file/file.dats"
staload _ = "util/stream.dats"
staload _ = "file/location.dats"
staload _ = "parcc.dats"
staload _ = "util/pair.dats"

dynload "dynload.dats"


extern fun test (path: string): void
implement test (path) = () where {
	val sm = file_append_location (file_append_position (file_get_stream path, Pos (1, 1)), path)

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
	val _ = test("./test/test.dats")
}
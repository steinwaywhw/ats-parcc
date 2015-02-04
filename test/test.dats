#include "share/atspre_staload.hats"
staload "libc/SATS/stdio.sats"

staload "parcc.sats"
staload "lexing/token.sats"
staload "lexing/lexcc.sats"
staload "file/file.sats"
staload "file/location.sats"
staload "util/pair.sats"
staload "util/stream.sats"
staload "util/util.sats"

staload _ = "parcc.dats"
staload _ = "lexing/token.dats"
staload _ = "file/file.dats"
staload _ = "file/location.dats"
staload _ = "util/stream.dats"
staload _ = "util/pair.dats"
staload _ = "util/maybe.dats"
staload _ = "util/list.dats"
staload _ = "lexing/token.dats"
staload _ = "lexing/lexcc.dats"

dynload "dynload.dats"




extern fun test (path: string): void

implement test (path) = () where {
//	val sm = file_append_location (file_append_position (file_get_stream path, Pos (1, 1)), path)

	val sm = file_get_stream path

	val id = red (alpha()^*, lam (x) => string_unexplode x)
	val ws = skip whitespace()^*
	val key = literal "lam"
	val operator = literal "=" <|> literal "=>" <|> literal "++"


	fun p (out: FILEref, p: pair (char, location)): void = () where {
		val Pair (c, loc) = p 
		val _ = fprint (out, c)
		val _ = fprint (out, " ")
		val _ = fprint (out, loc)
		val _ = fprint (out, "\n")
	}

	val _ = case+ t of 
		| Success (v, _) => fprint (stdout_ref, v)
		| Failure (v) => fprint<pair(char, location)> (stdout_ref, v, 10, p)
}



implement main0 () = () where {
	val _ = test("./test/1.test")
}
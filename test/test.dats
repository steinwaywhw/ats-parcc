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




val ops = charin "+-*/"
val number = red (digit()^+, lam x => foldr (x, 0, lam x, y => x + 10 * y))




extern fun test (path: string): void

implement test (path) = () where {

	val sm = file_get_stream path


	val _ = show (apply (skipws ops() <|> skipws number() <|> skipws number())
	
}


implement main0 () = () where {
	val _ = test("./test/1.test")
}
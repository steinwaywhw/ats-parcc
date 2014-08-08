#include "share/atspre_staload.hats"

staload "lexing/lexcc.sats"
staload sm = "util/stream.sats"
staload "util/util.sats"
staload "util/pair.sats"
staload "util/list.sats"
staload "string/string.sats"
staload "file/location.sats"
staload "parcc.sats"

staload _ = "util/list.dats"
staload _ = "util/pair.dats"
staload _ = "parcc.dats"

implement lit_char (match) = sat (anychar (), lam x => fst x = match)
		
implement lit_string (match) = let 
	fun genpar (index: int):<cloref1> parser (charloc_stream, location) =
		if index = len (match) - 1
		then red (lit_char match[index], lam x => snd x)
		else red (
				seq (lit_char match[index], genpar (index+1)), 
				lam (x) => loc where {
					val Pair (chloc, loc) = x 
					val range = range_merge (location_range (snd chloc), location_range loc)
					val loc = Loc (location_file loc, range)
				}
			)
in 
	red (genpar (0), lam (x) => Pair (match, x))
end

implement charin (charset) = sat (anychar (), lam x => string_find (charset, fst x) >= 0)

implement anychar () =
	lam input => 
		case+ !input of
		| $sm.Nil () => Failure (input)
		| $sm.Cons (x, rest) => Success (x, rest)

implement digit () = 
	red (
		sat (anychar (), lam x => isdigit (fst x)),
		lam x => Pair (fst x - '0', snd x))

implement xdigit () = 
	red (
		sat (anychar (), lam x => isxdigit (fst x)),
		lam x => 
			if isdigit (fst x)
			then Pair (fst x - '0', snd x)
			else Pair (tolower (fst x) - 'a' + 10, snd x)
		)


implement alpha () = sat (anychar (), lam x => isalpha (fst x))
implement spacetab () = sat (anychar (), lam x => isblank (fst x))
implement printable () = sat (anychar (), lam x => isprint (fst x))
implement newline () = literal '\n'
implement whitespace () = sat (anychar (), lam x => isspace (fst x))
implement charnotin (charset) = sat (anychar (), lam x => string_find (charset, fst x) < 0)

implement escape () = let 
	fun merge (a:location, b:location): location =
		Loc (location_file a, range_merge (location_range a, location_range b))

	fun mergepair (x: pair (charloc, charloc)): location = merge (snd (fst x), snd (snd x))

	fun projdigit (xs: list (pair (int, location))): list (int) = map (xs, lam x => fst x)
	fun projloc (xs: list (pair (int, location))): list (location) = map (xs, lam x => snd x)

	val case1 = 
		red (seq (literal '\\', charin ("abfnrtv\\\'\"?")), 
			lam x => 
				case+ fst (snd x) of
				| 'a' => Pair ('\a', mergepair x)
				| 'b' => Pair ('\b', mergepair x)
				| 'f' => Pair ('\f', mergepair x)
				| 'n' => Pair ('\n', mergepair x)
				| 'r' => Pair ('\r', mergepair x)
				| 't' => Pair ('\t', mergepair x)
				| 'v' => Pair ('\v', mergepair x)
				| ch  => Pair (ch, mergepair x)
		)
	
	val case2 = let 
		val p = seq (literal '\\', rptn (digit (), 3))
		val p = red (p, lam x => let 
				val ch = int2char0 (list_toint (projdigit (snd x), 8))
				val begpos = range_begin (location_range (snd (fst x)))
				val begpos = Pos (position_line begpos, position_col begpos + 1)
				val endpos = range_end (location_range (snd (fst x)))
				val endpos = Pos (position_line endpos, position_col endpos + 3)
			in Pair (ch, Loc (location_file (snd (fst x)), Range (begpos, endpos))) end)

		in p end

	val case3 = let 
		val p = seq (literal "\\x", rptn (xdigit (), 2))
		val p = red (p, lam x => let 
				val ch = int2char0 (list_toint (projdigit (snd x), 16))
				val begpos = range_begin (location_range (snd (fst x)))
				val begpos = Pos (position_line begpos, position_col begpos + 2)
				val endpos = range_end (location_range (snd (fst x)))
				val endpos = Pos (position_line endpos, position_col endpos + 2)
			in Pair (ch, Loc (location_file (snd (fst x)), Range (begpos, endpos))) end)
		in p end

in alt (case1, alt (case2, case3)) end


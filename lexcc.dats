#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "util/util.sats"
staload "util/string.sats"
staload "util/list.sats"
staload sm = "util/stream.sats"
staload "util/pair.sats"
staload "util/unit.sats"

staload "parcc.sats"
staload "lexcc.sats"

staload _ = "parcc.dats"
staload _ = "util/list.dats"
staload _ = "util/pair.dats"

//staload "file/location.sats"

#define :: Cons 

implement anychar () =
	lam input => 
		case+ !input of
		| $sm.Nil () => Failure (input)
		| $sm.Cons (x, rest) => Success (x, rest)

implement lit_char (match) = sat (anychar (), lam x => x = match)

implement lit_string (match) = let 
	fun genpar (index: int):<cloref1> lexer unit =
		if index = len (match) - 1
		then skip (lit_char match[index])
		else skip (seq (skip (lit_char match[index]), genpar (index+1)))
in 
	red (genpar (0), lam _ => match)
end


implement charin (charset) = sat (anychar (), lam x => string_find (charset, string_from_char x) >= 0)
implement charnotin (charset) = sat (anychar (), lam x => string_find (charset, string_from_char x) < 0)

implement digit () = sat (anychar (), lam x => isdigit x)
implement xdigit () = sat (anychar (), lam x => isxdigit x)
implement alpha () = sat (anychar (), lam x => isalpha x)
implement spacetab () = sat (anychar (), lam x => isblank x)
implement printable () = sat (anychar (), lam x => isprint x)
implement newline () = lit_char '\n'
implement whitespace () = sat (anychar (), lam x => isspace x)

implement escape () = let 
	val case1 = 
		red (
			seq (literal '\\', charin ("abfnrtv\\\'\"\?`")), 
			lam x => 
				case+ snd x of
				| 'a' => '\a'
				| 'b' => '\b'
				| 'f' => '\f'
				| 'n' => '\n'
				| 'r' => '\r'
				| 't' => '\t'
				| 'v' => '\v'
				| ch  => ch
			)
	
	val ds = red (rptn (digit(), 3), lam xs => map(xs, lam x => x-'0')): lexer (list int)
	val case2 =  
		red (
			seq (literal '\\', ds), 
			lam x => 
				case- snd x of 
				| x :: y :: z :: Nil () => int2char0 (x*64 + y*8 + z)
			)

	val xds = red (rptn (xdigit(), 2), lam xs => map (xs, lam x => if isdigit x then x-'0' else tolower(x)-'a'+10)): lexer (list int)
	val case3 = 
		red (
			seq (literal "\\x", xds),
			lam x => 
				case- snd x of 
				| x :: y :: Nil () => int2char0 (x * 16 + y)
			)

in 
	alt (case1, alt (case2, case3)) 
end


implement {o} skipws (l) = 
	red (seq (l, skip (rpt0 (spacetab ()))), lam x => fst x)

implement alphadigit () = 
	alt (alpha (), digit ())

implement alphas () =
	red (rpt1 (alpha ()), lam x => string_unexplode x)

implement digits () =
	red (rpt1 (digit ()), lam x => string_unexplode x)

implement xdigits () =
	red (rpt1 (xdigit ()), lam x => string_unexplode x)

implement alphadigits () = 		
	red (rpt1 (alphadigit ()), lam x => string_unexplode x)


implement char_single_quote () = 
	red (
		seq (literal '\'', 
			seq (alt (escape (), printable ()), 
				literal '\'')),
		lam x => fst (snd x)
		)

implement string_double_quote () = let
	val content = red (rpt0 (alt (charnotin "\n\\\"", escape())), lam x => string_unexplode x)
	val str = seq (literal '"', seq (content, literal '"'))
in 
	red (str, lam x => fst (snd x))
end 

implement string_backtip () = let
	val content = red (rpt0 (alt (charnotin "\n\\`", escape())), lam x => string_unexplode x)
	val str = seq (literal '`', seq (content, literal '`'))
in 
	red (str, lam x => fst (snd x))
end 

implement string_multiline () = let
	val backtips = red (sat (rpt1 (literal '`'), lam xs => len (xs) != 3), lam x => string_unexplode x)
	val nonbacktips = red (rpt1 (alt (charnotin "\\`", escape())), lam x => string_unexplode x)
	val content = red (rpt0 (alt (backtips, nonbacktips)), lam xs => string_join (xs, ""))
	val str = seq (literal "```", seq (content, literal "```"))
in 
	red (str, lam x => fst (snd x))
end 

implement unsigned_int_dec () =
	alt (
		red (
			literal '0', 
			lam x => 0), 
		red (
			seq (charin "123456789", digits ()), 
			lam x => string_to_int_unsigned (prepend (snd x, fst x))))       

implement signed_int_dec () = 
	red (
		seq (
			red (alt (literal '+', literal '-'), lam x => if x = '+' then 1 else ~1),
			unsigned_int_dec ()),
		lam x => let 
			val a = fst x : int 
			val b = snd x : int 
		in a * b end)

implement unsigned_int_hex () = let 
	fun loop (s: string, ret: int): int = 
		if empty s 
		then ret 
		else 
			if isdigit (head s)
			then loop (tail s, (head s) - '0' + 16 * ret)
			else loop (tail s, tolower (head s) - 'a' + 10 + 16 * ret)
in 
	red (xdigits (), lam x => loop (x, 0))
end

implement unsigned_int_bin () = let 
	val str = red (rpt1 (charin "01"), lam x => string_unexplode x) : lexer string 
	fun loop (s: string, ret: int): int = 
		if empty s 
		then ret 
		else loop (tail s, (head s) - '0' + 2 * ret)
in 
	red (str, lam x => loop (x, 0))
end

implement unsigned_int_oct () = let
	val str = red (rpt1 (charin "01234567"), lam x => string_unexplode x) : lexer string 
	fun loop (s: string, ret: int): int = 
		if empty s 
		then ret 
		else loop (tail s, (head s) - '0' + 8 * ret)
in 
	red (str, lam x => loop (x, 0))
end

implement exponent () = 
	red (
		seq (
			alt (literal 'e', literal 'E'), 
			alt (signed_int_dec (), unsigned_int_dec ())), 
		lam x => snd x)

implement unsigned_double_dec () = let 
	val p = digits () :: literal "." :: digits () :: Nil () : list (lexer string)
in
	red (
		seq (digits (), 
			seq (literal ".", digits ())),
		lam xs => string_to_double_unsigned (string_join (fst xs :: "." :: snd (snd xs) :: Nil (), "")))
end

implement signed_double_dec () =
	red (
		seq (
			red (
				alt (literal '+', literal '-'), 
				lam x => if x = '+' then 1.0 else ~1.0), 
			unsigned_double_dec ()), 
		lam x => let 
			val a = fst x : double 
			val b = snd x : double 
		in a * b end)

implement boolean () = 
	red (
		alt (literal "true", literal "false"), 
		lam x => if x = "true" then true else false)

implement symbol () = charin "!#$%&*+-./<=>?@\\^|~"
implement symbols () = red (rpt1 (symbol ()), lam x => string_unexplode x)


staload _ = "util/stream.dats"
staload "util/convert.sats"

implement main0 () = () where {
	val sep = "\n==========================\n"
	val _ = show (apply (literal 'c', string_to_stream "cdefg"))
	val _ = show sep 
	val _ = show (apply (literal "hello", string_to_stream "hello"))
	val _ = show sep 
	val _ = show (apply (anychar (), string_to_stream "abcde"))
	val _ = show sep 
	val _ = show (apply (charin "cde", string_to_stream "eab"))
	val _ = show sep 
	val _ = show (apply (charnotin "cde", string_to_stream "eab"))
	val _ = show sep 
	val _ = show (apply (digit (), string_to_stream "123"))
	val _ = show sep 
	val _ = show (apply (xdigit (), string_to_stream "a1"))
	val _ = show sep 
	val _ = show (apply (alpha (), string_to_stream "a1"))
	val _ = show sep 
	val _ = show (apply (spacetab (), string_to_stream "\t"))
	val _ = show sep 
	val _ = show (apply (escape (), string_to_stream "\\t"))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"abcde\""))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"\\tbcde\""))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"\tbcde\""))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"a\"bcde\""))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"abc\\\"de\""))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"abc\nde\""))
	val _ = show sep 
	val _ = show (apply (string_double_quote (), string_to_stream "\"abc'de\""))
	val _ = show sep 
	val _ = show (apply (string_backtip (), string_to_stream "`abcde`"))
	val _ = show sep 
	val _ = show (apply (string_backtip (), string_to_stream "`\"abcde`"))
	val _ = show sep 
	val _ = show (apply (string_backtip (), string_to_stream "`\\abcde`"))
	val _ = show sep 
	val _ = show (apply (string_backtip (), string_to_stream "``abcde`"))
	val _ = show sep 
	val _ = show (apply (string_backtip (), string_to_stream "`ab`cde`"))
	val _ = show sep 
	val _ = show (apply (string_backtip (), string_to_stream "`\\`abcde`"))
	val _ = show sep 
	val _ = show (apply (string_multiline (), string_to_stream "```abc\nde`fs``sa````dasd```asd```"))
	val _ = show sep 
	val _ = show (apply (signed_int_dec (), string_to_stream "-109"))
	val _ = show sep 
	val _ = show (apply (unsigned_int_hex (), string_to_stream "0fF"))
	val _ = show sep 
	val _ = show (apply (unsigned_int_bin (), string_to_stream "101"))
	val _ = show sep 
	val _ = show (apply (unsigned_int_oct (), string_to_stream "0777"))
	val _ = show sep 
	val _ = show (apply (exponent (), string_to_stream "e+10"))
	val _ = show sep 
	val _ = show (apply (unsigned_double_dec (), string_to_stream "10.900"))
	val _ = show sep 
	val _ = show (apply (signed_double_dec (), string_to_stream "-10.900"))
	val _ = show sep 
	val _ = show (apply (boolean (), string_to_stream "true"))
	val _ = show sep 
}


////
implement lit_char (match) = sat (anychar (), lam x => x = match)
		
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

implement charin (charset) = 
	sat (anychar (), lam x => string_find (charset, fst x) >= 0)



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


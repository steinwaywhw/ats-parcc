#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"

#include "atsutils.hats"
staload "list.sats"
staload "stream.sats"
staload "string.sats"
staload "unit.sats"

staload "parser.sats"

staload "parcc.sats"
staload _ = "parcc.dats"


#define :: StreamCons 
#define nil StreamNil

local 

assume input_t = stream char

in 

implement parser_char () = 
	parser_encode (lam (input, cont) => 
		case+ !input of 
		| x :: xs => cont (x, xs)
		| nil _   => $raise ParsingException ("parser_char"))

implement parser_eof () = 
	parser_encode (lam (input, cont) => 
		case+ !input of 
		| x :: xs => $raise ParsingException ("parser_eof")
		| nil _ => cont (Unit (), $delay nil ()))

end

implement parser_alpha () = 
	parser_char () \parcc_sat (lam c => isalpha c)

implement parser_digit () = 
	parser_char () \parcc_sat (lam c => isdigit c)

implement parser_xdigit () = 
	parser_char () \parcc_sat (lam c => isxdigit c)

implement parser_printable () = 
	parser_char () \parcc_sat (lam c => isprint c)

implement parser_space () = 
	parser_lit_char (' ')

implement parser_spaces () = 
	parcc_rpt1 (parser_space ()) \parcc_map (lam xs => string_unexplode xs)

implement parser_alphadigit () = 
	parser_char () \parcc_sat (lam c => isalnum c)

implement parser_lowercase () = 
	parser_char () \parcc_sat (lam c => islower c)

implement parser_uppercase () = 
	parser_char () \parcc_sat (lam c => isupper c)

implement parser_lit_char (lit) = 
	parser_char () \parcc_sat (lam (c:char) => lit = c)

implement parser_lit_string (lit) = 
	parcc_seqs (foldr<char,list (parser char)> (string_explode lit, ListNil (), lam (x, xs) =<cloref1> ListCons (parser_lit_char x, xs)))
		\parcc_map (lam xs => string_unexplode xs)

implement parser_digits () = 
	(parcc_rpt1 (parser_digit ())) \parcc_map (lam ds => string_unexplode ds)

implement parser_oneof (str) = 
	parser_char () \parcc_sat (lam c => string_contains (str, string_from_char c))

implement parser_noneof (str) = 
	parser_char () \parcc_sat (lam c => not (string_contains (str, string_from_char c)))

implement parser_ws () = 
	(parcc_rpt1 (parser_oneof (" \t\n\v\f\r"))) \parcc_map (lam x => string_unexplode x)

implement parser_symbol () = 
	parser_oneof ("!#$%&*+-./<=>?@\\^|~")

implement parser_symbols () = 
	(parcc_rpt1 (parser_symbol ())) \parcc_map (lam xs => string_unexplode xs)

implement {a} parcc_wsaware (p) = 
	p \parcc_followedby parser_ws ()

implement parser_escape () = let 
	val case1 = 
		(parser_lit_char ('\\') \parcc_leadby parser_oneof ("abfnrtv\\\'\""))
			\parcc_map (lam x => 
				case+ x of 
				| 'a' => '\a'
				| 'b' => '\b'
				| 'f' => '\f'
				| 'n' => '\n'
				| 'r' => '\r'
				| 't' => '\t'
				| 'v' => '\v'
				| ch  =>> ch)

	val octnum = 
		parcc_rptn (parser_digit () \parcc_sat (lam x => x >= '0' andalso x <= '7'), 3)
			\parcc_map (lam xs => list_foldl<char,int> (xs, 0, lam (d:char, ds:int):int => (d - '0') * 8 + ds))

	val case2 = parser_lit_char ('\\') \parcc_leadby (octnum \parcc_map (lam x => int2char0 x))

	val hexnum = 
		parcc_rptn (parser_xdigit () \parcc_map (lam x => if isdigit x then x - '0' else ((tolower x) - 'a') + 10), 2)
			\parcc_map (lam xs => list_foldl<int,int> (xs, 0, lam (d, ds) => d * 16 + ds))

	val case3 = parser_lit_string ("\\x") \parcc_leadby (hexnum \parcc_map (lam x => int2char0 x))
in 
	parcc_alt3 (case1, case2, case3)
end




#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
#define ATS_DYNLOADFLAG 0

staload "symintr.sats"

staload "parser.sats"

staload "parcc.sats"
staload "list.sats"
staload "stream.sats"
staload "string.sats"
staload "unit.sats"
staload _ = "parcc.dats"
staload _ = "list.dats"
staload _ = "stream.dats"
staload _ = "string.dats"

assume input_t = stream char

#define :: StreamCons 
#define nil StreamNil

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

(****************************************************************)


postfix ^*
overload ^* with parcc_rpt0

postfix ^+
overload ^+ with parcc_rpt1 

infixl ++
overload ++ with parcc_seq 

symintr ||
infixl || 
overload || with parcc_alt 

assume output_t = unit 

implement gcompare_val_val<parser sexp> (x, y) = 
    gcompare_val_val<ref(void)> ($UN.cast{ref(void)} x, $UN.cast{ref(void)} y)

extern fun {a:t@ype} debug (string, parser a): parser a
implement {a} debug (str, p) = 
	parser_encode (lam (input, cont) => 
		let val _ = println! str
		in parser_apply (p, input, cont) end)

implement parser_symbol () = 
	((parser_alpha () || parser_lit_char '_') ++ (parser_alphadigit () ^*))
		\parcc_map (lam xs => SSymbol (string_unexplode (ListCons (xs.0, xs.1))))

implement parser_bool () =  
	(parser_lit_string ("true") || parser_lit_string ("false"))
		\parcc_map (lam x => if x = "true" then SBool true else SBool false)

implement parser_string () = 
	parcc_between (parser_lit_char '"', parcc_rpt0 (parser_escape () \parcc_alt parser_char ()), parser_lit_char '"')
		\parcc_map (lam xs => SString (string_unexplode xs))

implement parser_int () = 
	parser_digits () \parcc_map (lam xs => SInt (string_to_int xs))

implement parser_atom () = let 
	#define :: ListCons 
	#define nil ListNil 
in 
	parcc_alts (parser_int () :: parser_bool () :: parser_symbol () :: parser_string () :: nil ())
end

implement parser_sexp () = 
	parser_atom () 
		\parcc_alt (parcc_between (parser_lit_string "(", parcc_delay (lam () => parser_sexplist ()), parser_lit_string ")"))

implement parser_sexplist () = 
	parcc_sepby (parser_sexp (), parser_ws ())
		\parcc_map (lam xs => SList xs)

implement parser_file () = 
	parser_sexplist () \parcc_followedby parser_eof ()

implement unparse (sexp) = 
	case+ sexp of 
	| SString s => (show "Str("; show s; show ")"; Unit ())
	| SBool s => (show "Bool("; show s; show ")"; Unit ())
	| SInt s => (show "Int("; show s; show ")"; Unit ())
	| SSymbol s => (show "Sym("; show s; show ")"; Unit ())
	| SExp s => (show "Exp("; let val _ = unparse s in () end; show ")"; Unit ())
	| SList ss => (show "List("; let val _ = list_foldl<sexp,unit> (ss, Unit (), lam (s, ss) => unparse s) in () end; show ")"; Unit ())



extern fun file_get_stream (string): stream char 
implement file_get_stream (path) = sm where {
	staload "libc/SATS/stdio.sats"

	fun tostream (file: FILEref): stream char = let
		val c = int2char0 (fgetc file)
	in 
		if feof file = 0
		then $delay StreamCons (c, tostream file)
		else $delay StreamNil () where {
			val _ = fclose_exn file
		} 
	end

	val file = fopen_ref_exn (path, file_mode_r)
	val sm = tostream file
}

implement main0 () = () where {

	val input = file_get_stream ("./test1.sexp")

	val _ = parser_apply (parser_file (), input, lam (x, input) => unparse x)
}


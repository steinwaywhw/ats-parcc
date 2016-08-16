#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"

staload "sexp.sats"
staload "parcc.sats"
staload "parser.sats"

#include "atsutils.hats"
staload "list.sats"
staload "unit.sats"
staload "string.sats"

staload _ = "parcc.dats"
staload _ = "parser.dats"


overload ^* with parcc_rpt0
postfix ^*

overload ^+ with parcc_rpt1 
postfix ^+

overload ++ with parcc_seq 
infixl ++

symintr ||
overload || with parcc_alt 
infixl || 



implement order_compare<parser sexp> (x, y) = 
    gcompare_val_val<ref(void)> ($UNSAFE.cast{ref(void)} x, $UNSAFE.cast{ref(void)} y)

//extern fun {a:t@ype} debug (string, parser a): parser a
//implement {a} debug (str, p) = 
//	parser_encode (lam (input, cont) => 
//		let val _ = println! str
//		in parser_apply (p, input, cont) end)

implement parser_sexp_symbol () = 
	(((parser_alpha () || parser_lit_char '_') ++ (parser_alphadigit () ^*))
			\parcc_map (lam xs => SSymbol (string_unexplode (ListCons (xs.0, xs.1)))))
	|| ((parser_symbols ()) \parcc_map (lam x => SSymbol x))

implement parser_sexp_bool () =  
	(parser_lit_string ("true") || parser_lit_string ("false"))
		\parcc_map (lam x => if x = "true" then SBool true else SBool false)

implement parser_sexp_string () = 
	parcc_between (parser_lit_char '"', parcc_rpt0 (parser_escape () \parcc_alt parser_char ()), parser_lit_char '"')
		\parcc_map (lam xs => SString (string_unexplode xs))

implement parser_sexp_int () = 
	parser_digits () \parcc_map (lam xs => SInt (string_to_int xs))

implement parser_sexp_atom () = let 
	#define :: ListCons 
	#define nil ListNil 
in 
	parcc_alts (parser_sexp_int () :: parser_sexp_bool () :: parser_sexp_symbol () :: parser_sexp_string () :: nil ())
end

implement parser_sexp_sexp () = 
	parser_sexp_atom () 
		\parcc_alt (parcc_between (
			parser_lit_string "(", 
			(parcc_delay (lam () => parser_sexp_sexplist ())) || parser_succeed (SList (ListNil ())), 
			parser_lit_string ")"))

implement parser_sexp_sexplist () = 
	parcc_sepby (parser_sexp_sexp (), parser_ws ())
		\parcc_map (lam xs => SList xs)

implement parser_sexp_file () = 
	parser_sexp_sexplist () \parcc_followedby parser_eof ()

implement unparse_sexp (sexp) = 
	case+ sexp of 
	| SString s => (show<string> "Str("; show<string> s; show<string> ")"; Unit ())
	| SBool s => (show<string> "Bool("; show<bool> s; show<string> ")"; Unit ())
	| SInt s => (show<string> "Int("; show<int> s; show<string> ")"; Unit ())
	| SSymbol s => (show<string> "Sym("; show<string> s; show<string> ")"; Unit ())
	| SList ss => (show<string> "List("; let val _ = list_foldl<sexp,unit> (ss, Unit (), lam (s, ss) => unparse_sexp s) in () end; show<string> ")"; Unit ())



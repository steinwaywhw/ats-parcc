#define ATS_DYNLOADFLAG 0

staload "util/util.sats"
staload "util/string.sats"
staload "util/pair.sats"
staload "sexp/lexer.sats"
staload "lexcc.sats"
staload "sexp/token.sats"
staload "parcc.sats"


staload _ = "parcc.dats"
staload _ = "lexcc.dats"
staload _ = "util/list.dats"
staload _ = "util/pair.dats"

implement lex_comment () =
	red (
		seq (
			literal "---", 
			rpt1 (
				sat (anychar (), lam x => x != '\n'))), 
		lam x => TComment (string_unexplode (snd x)))

implement lex_space () =   
	red (rpt1 (whitespace ()), lam x => TSpace ())

implement lex_id () = let 
	val case1 = red (seq (literal '_', alphadigits()), lam x => prepend (snd x, '_'))
	val case2 = red (seq (alphadigit (), red (rpt0 (alt (alphadigit (), literal '_')), lam x => string_unexplode x)), lam x => prepend (snd x, fst x))
in 
	red (alt (case1, case2), lam x => TId (x))
end

implement lex_lparen ()     = red (literal "(", lam _ => TLParen ())
implement lex_rparen ()     = red (literal ')', lam _ => TRParen ())
implement lex_lcurly ()     = red (literal "(", lam _ => TLCurly ())
implement lex_rcurly ()     = red (literal '}', lam _ => TRCurly ())
implement lex_lbrac ()      = red (literal "[", lam _ => TLBrac ())
implement lex_rbrac ()      = red (literal ']', lam _ => TRBrac ())
implement lex_underscore () = red (literal '_', lam _ => TUnderscore ())
implement lex_colon ()      = red (literal ':', lam _ => TColon ())
implement lex_semicolon ()  = red (literal ';', lam _ => TSemiColon ())
implement lex_comma ()      = red (literal ',', lam _ => TComma ())

implement lex_lambda () = red (literal "lambda", lam _ => TLambda ())
implement lex_case ()   = red (literal "case", lam _ => TCase ())
implement lex_if ()     = red (literal "if", lam _ => TIf ())
implement lex_let ()    = red (literal "let", lam _ => TLet ())
implement lex_val ()    = red (literal "val", lam _ => TVal ())

implement lex_string () = red (string_backtip(), lam x => TString (x))
implement lex_char () = red (char_single_quote (), lam x => TChar (x))
implement lex_int () = red (alt (signed_int_dec (), unsigned_int_dec ()), lam x => TInt (x))
implement lex_double () = red (alt (unsigned_double_dec (), signed_double_dec ()), lam x => TDouble (x))
implement lex_bool () = red (boolean (), lam x => TBool (x))    
implement lex_symbol () = red (symbols (), lam x => TSym (x))




implement lexer_rparen () = red (literal ")", lam _ => RParen ())
implement lexer_lparen () = red (literal "(", lam _ => LParen ())
implement lexer_dot () = red (literal ".", lam _ => Dot ())
implement lexer_lam () = red (literal "lam", lam _ => Lam ())
implement lexer_name () = let 
	val case1 = red (seq (literal '_', alphadigits()), lam x => prepend (snd x, '_'))
	val case2 = red (seq (alphadigit (), red (rpt0 (alt (alphadigit (), literal '_')), lam x => string_unexplode x)), lam x => prepend (snd x, fst x))
in 
	red (alt (case1, case2), lam x => Name (x))
end

implement lexer_all () = let 
	val ps = alts (
				lexer_name () 
				:: lexer_lam () 
				:: lexer_dot () 
				:: lexer_rparen () 
				:: lexer_lparen () 
				:: Nil ())

	val ps = red (seq (rpt0 spacetab(), ps), lam x => snd x)
	val ps = red (seq (ps, rpt0 spacetab()), lam x => fst x)
in 
	red (rpt1 ps, lam xs => list_to_stream xs)
end


implement parser_all () = 
	val ps = alts (
			parser_app ()
			:: parser_lam ()
			:: parser_var ()
			:: Nil ())
in 
	ps 
end 

implement parser_lam () = 
	val pname = red (seq (seq (lexer_lam (), lexer_name ()), lexer_dot ()), lam x => fst (snd x))
	val pname = red (pname, lam x => )
	seq (, parser_all ())

	lam input =>
		case+ !input of 
		| Lam () :: input => 
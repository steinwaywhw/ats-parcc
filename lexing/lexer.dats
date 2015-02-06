#define ATS_DYNLOADFLAG 0
////
staload "util/util.sats"
staload "lexing/lexer.sats"
staload "lexing/lexcc.sats"
staload "lexing/token.sats"
staload "parcc.sats"


implement lex_comment () =
	red (seq (literal "---", rpt1 (sat (anychar (), lam x => x != '\n')), lam x => TComment ("comment")))

implement lex_space () =   
	red (rpt1 (whitespace()), lam x => TSpace ())

implement lex_id () = let 
	val case1 = red (seq (literal '_', alphadigits()), lam x => prepend (snd x, '_'))
	val case2 = red (seq (alphadigit (), red (rpt0 (alt (alphadigit (), literal '_')), lam x => string_unexplode x), lam x => prepend (snd x, fst x)))
in 
	alt (case1 (), case2 ())
end

implement lex_lparen () = red (literal '(', lam _ => TLParen ())
implement lex_rparen () = red (literal ')', lam _ => TRParen ())
implement lex_lcurly () = red (literal '{', lam _ => TLCurly ())
implement lex_rcurly () = red (literal '}', lam _ => TRCurly ())
implement lex_lbrac () = red (literal '[', lam _ => TLBrac ())
implement lex_rbrac () = red (literal ']', lam _ => TRBrac ())
implement lex_pany () = red (literal '_', lam _ => TPAny ())
implement lex_colon () = red (literal ':', lam _ => TColon ())

implement lex_lambda () = red (literal "lambda", lam _ => TLambda ())
implement lex_case () = red (literal "case", lam _ => TCase ())   
implement lex_if () = red (literal "if", lam _ => TIf ())     
implement lex_let () = red (literal "let", lam _ => TLet ())    
implement lex_val () = red (literal "val", lam _ => TVal ())    

implement lex_string () = 
implement lex_char () = red (, lam _ => TVal ())    
implement lex_int () = red (, lam _ => TVal ())     
implement lex_double () = red (, lam _ => TVal ())  
implement lex_bool () = red (, lam _ => TVal ())    

implement lex_op () =      
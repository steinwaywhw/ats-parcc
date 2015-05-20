staload "parcc.sats"

datatype syntax = 
	| TmLam of (string, syntax)
	| TmVar of (string)
	| TmApp of (syntax, syntax)



fun parser_lam (): lazy (parser syntax)
and parser_var (): lazy (parser syntax)
and parser_app (): lazy (parser syntax)
and parser (): lazy (parser syntax)


fun show_syntax (syntax): void

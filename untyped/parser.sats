staload sm = "../util/stream.sats"

datatype term = 
	| TmLam of (string, term)
	| TmVar of (string)
	| TmApp of (term, term)

datatype token =
	| Name of string
	| Lam of () 
	| Dot of () 
	| LParen of () 
	| RParen of ()

fun lexer_lam    (): lexer token
fun lexer_name   (): lexer token
fun lexer_dot    (): lexer token
fun lexer_lparen (): lexer token
fun lexer_rparen (): lexer token

fun lexer_all (): lexer (lazy ($sm.stream token))

fun parser_app   (): parser (lazy ($sm.stream token), term)
fun parser_var 	 (): parser (lazy ($sm.stream token), term)
fun parser_lam   (): parser (lazy ($sm.stream token), term)

fun parser_all (): parser (lazy ($sm.stream token), term)



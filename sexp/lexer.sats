
staload "util/util.sats"
staload "lexcc.sats"
staload "sexp/token.sats"

fun lex_comment ()       : lexer token
fun lex_space ()         : lexer token
fun lex_id ()            : lexer token
fun lex_lparen ()        : lexer token
fun lex_rparen ()        : lexer token
fun lex_lcurly ()        : lexer token
fun lex_rcurly ()        : lexer token
fun lex_lbrac ()         : lexer token
fun lex_rbrac ()         : lexer token
fun lex_underscore ()    : lexer token
fun lex_colon ()         : lexer token
fun lex_semicolon ()     : lexer token
fun lex_comma ()         : lexer token

fun lex_lambda ()        : lexer token
fun lex_case ()          : lexer token
fun lex_if ()            : lexer token
fun lex_let ()           : lexer token
fun lex_val ()           : lexer token

fun lex_string ()        : lexer token
fun lex_char ()          : lexer token

fun lex_int ()           : lexer token
fun lex_double ()        : lexer token
fun lex_bool ()          : lexer token

fun lex_symbol ()        : lexer token
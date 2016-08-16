staload "parcc.sats"

staload "stream.sats"
staload "unit.sats"

fun parser_char (): parser char
fun parser_eof (): parser unit

fun parser_oneof (string): parser char
fun parser_noneof (string): parser char
fun parser_printable (): parser char 
fun parser_ws (): parser string
fun parser_space (): parser char 
fun parser_spaces (): parser string 
fun parser_alpha (): parser char 
fun parser_digit (): parser char 
fun parser_xdigit (): parser char 
fun parser_digits (): parser string 
fun parser_alphadigit (): parser char 
fun parser_lowercase (): parser char 
fun parser_uppercase (): parser char 
fun parser_lit_char (char): parser char
fun parser_lit_string (string): parser string 
fun parser_escape (): parser char 
fun parser_symbol (): parser char 
fun parser_symbols (): parser string 

fun {a:t@ype} parcc_wsaware (parser a): parser a

(****************************************************************)





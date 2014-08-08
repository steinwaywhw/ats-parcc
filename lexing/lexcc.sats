staload "parcc.sats"
staload "util/stream.sats"
staload "lexing/token.sats"
staload "file/location.sats"
staload "util/pair.sats"

// These types are for internal use only
// No public use is allowed
typedef charloc = pair (char, location)
typedef charloc_stream = lazy (stream charloc)
typedef strloc = pair (string, location)
typedef intloc = pair (int, location)
typedef lex_t = parser (charloc_stream, charloc)

//
// lexer
//
symintr literal

fun lit_char (input: char): lex_t
fun lit_string (input: string): parser (charloc_stream, strloc)
overload literal with lit_char 
overload literal with lit_string

fun anychar (): lex_t // any input
fun charin (string): lex_t // char is in the set
fun charnotin (string): lex_t // char is not in the set
fun digit (): parser (charloc_stream, intloc) // [0-9]
fun xdigit (): parser (charloc_stream, intloc) // [0-9] + ABCDEF + abcdef
fun alpha (): lex_t // [a-zA-Z]
fun spacetab (): lex_t // space or tab
fun newline (): lex_t // \n
fun whitespace (): lex_t // space, \t\n\v\f\r
fun printable (): lex_t // printable
fun escape (): lex_t // c escape
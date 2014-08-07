staload "util/util.sats"
staload "file/location.sats"

abstype token = ptr

datatype tokennode = 
	| TComment 	of string
	| TSpace 	of ()
	| TChar 	of char
	| TString 	of string
	| TId 		of string
	| TInt 		of int

fun token_get_file (token): string
fun token_get_range (token): range
fun token_get_location (token): location
fun token_get_node (token): tokennode
fun token_make (tokennode, location): token

fun fprint_token (out: FILEref, t: token): void
fun fprint_tokennode (out: FILEref, t: tokennode): void
overload fprint with fprint_token
overload fprint with fprint_tokennode
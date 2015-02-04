staload "util/util.sats"
staload "file/location.sats"
staload "util/list.sats"

//abstype token = ptr

datatype token = 
	| TComment 	of string
	| TSpace 	of ()
	| TChar 	of char
	| TString 	of string
	| TId 		of string
	| TInt 		of int


fun print_token (token): void 
fun fprint_token (token, FILEref): void

overload show with print_token 
overload show with fprint_token 


////
fun token_get_file (token): string
fun token_get_range (token): range
fun token_get_location (token): location
fun token_get_node (token): tokennode
fun token_make (tokennode, location): token

fun fprint_token (out: FILEref, t: token): void
fun fprint_tokennode (out: FILEref, t: tokennode): void
fun fprint_token_list (out: FILEref, ts: list (token), int): void
overload fprint with fprint_token
overload fprint with fprint_tokennode
overload fprint with fprint_token_list 



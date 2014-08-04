

datatype token = 
	| Token of (string)

symintr equal

fun equal_token_token (token, token): bool
fun equal_token_string (token, string): bool
fun equal_string_token (string, token): bool
overload equal with equal_token_token
overload equal with equal_token_string
overload equal with equal_string_token

fun {a,b:type} equal_any_any (a, b): bool
overload equal with equal_any_any
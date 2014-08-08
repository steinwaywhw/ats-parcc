staload "util/list.sats"

fun string_explode (string): list (char)
fun string_get (string, int): char = "mac#"
fun string_len (string): int 

overload [] with string_get
overload len with string_len

fun string_find (string, char): int = "mac#"
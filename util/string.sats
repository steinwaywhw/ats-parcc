staload "util/list.sats"

fun string_explode (string): list (char)
fun string_unexplode (list char): string 

fun string_from_char (char): string
fun string_from_int (int): string
fun string_to_int (string): int 
fun string_to_double (string): double

fun string_find (string, string): int = "mac#"

fun string_join (list string, string): string
fun string_split (string, string): list string
fun string_concat (string, string): string 
fun string_append (string, char): string
fun string_prepend (string, char): string

fun string_range (string, int, int): string
fun string_compare (string, string): int
fun string_eq (string, string): bool

fun string_get (string, int): char = "mac#"
fun string_len (string): int 


overload [] with string_get
overload len with string_len
overload compare with string_compare
overload append with string_append 
overload prepend with string_prepend
overload concat with string_concat 
overload eq with string_eq

//overload = with string_eq




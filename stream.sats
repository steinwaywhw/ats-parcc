
//
// it is a string stream
//
abstype stream (a:type)

symintr head
fun head_default {a:type} (stream (a)): a
fun head_len     {a:type} (stream (a), int): stream (a) 
overload head with head_default
overload head with head_len 

symintr tail
fun tail_default {a:type} (stream (a)): stream (a)
fun tail_len	 {a:type} (stream (a), int): stream (a)
overload tail with tail_default
overload tail with tail_len 

fun drop {a:type} (stream (a), int): stream (a)
fun eof  {a:type} (stream (a)): bool

symintr tostring
//fun tostring_char (stream (char)): string
//overload tostring with tostring_char


//fun len (stream): int

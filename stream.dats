staload "stream.sats"

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


implement head_default {a} (sm) = head_len (sm, 1)
implement head_len {a} (sm, len) = 
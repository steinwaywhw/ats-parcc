
//
// it is a char stream
//
abstype stream (a:type)

fun head   {a:type} (stream (a)): a
fun tail   {a:type} (stream (a)): stream (a)
fun drop   {a:type} (stream (a), int): stream (a)
fun is_eof {a:type} (stream (a)): bool
//symintr tostring
//fun tostring_char (stream (char)): string
//overload tostring with tostring_char


//fun len (stream): int

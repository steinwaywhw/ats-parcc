

abstype list (a:t@ype)

fun empty  {a:t@ype} (list (a)): bool
fun append {a:t@ype} (list (a), a): list (a)
fun head   {a:t@ype} (list (a)): a
fun tail   {a:t@ype} (list (a)): list (a)
fun drop   {a:t@ype} (list (a), int): list (a)
fun concat {a:t@ype} (list (a), list (a)): list (a)

symintr ::
overload :: with concat

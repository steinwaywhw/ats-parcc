

staload "parcc.sats"

//
// k: key type, should be the parser
// v: value type, shoud be the stream (the input of the parser)
// o: output type, should be the result (the output of the parser)
//
abstype tramp (k:t@ype, v:t@ype, o:t@ype)

typedef tramp (o:t@ype) = [k,v:t@ype] tramp (k, v, o)
typedef tramp (k:t@ype, v:t@ype) = [o:t@ype] tramp (k, v, o)
typedef tramp = [k,v,o:t@ype] tramp (k, v, o)

fun has_next 		 (tramp): bool
fun push {k,v:t@ype} (tramp (k, v), k, v): void // stateful
fun step {o:t@ype}   (tramp (o)): maybe (o)	 	// stateful
fun run  {o:t@ype}   (tramp (o)): list (o)		// stateful

typedef call_node (k:t@ype, v:t@ype): @{f = k, args = v}

//
// These are stateful
//
fun set_call_stack {k,v:t@ype} (tramp (k, v), ref (stack (call_node (k, v)))): tramp (k, v)
fun get_call_stack {k,v:t@ype} (tramp (k, v)): ref (stack (call_node (k, v)))
fun set_memo_table {k,v:t@ype} {o:t@ype} (tramp (k, v, o), ref (map (k, map (v, o)))): tramp (k, v, o)
fun get_memo_table {k,v:t@ype} {o:t@ype} (tramp (k, v, o)): ref (map (k, map (v, o)))
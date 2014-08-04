staload "./stream.sats"
staload "./pair.sats"
staload "./unit.sats"

abstype parser (i:type, o:type)

datatype result (i:type, o:type) = 
	| Success of (o, stream (i))
	| Failure of (stream (i))

//
// pargen
// 
fun succeed {i:type} {o:type} (o): parser (i, o)
fun literal {i:type} 		  (string): parser (i, unit)

//
// parcom
//
fun alt   {i:type} {o:type} 	(parser (i, o), parser (i, o)): parser (i, o)
fun seq   {i:type} {o1,o2:type} (parser (i, o1), parser (i, o2)): parser (i, pair (o1, o2))

fun bind  {i:type} {o1,o2:type} (p: parser (i, o1), f: o1 -<cloref1> parser (i, o2)): parser (i, o2)
fun apply {i:type} {o:type} 	(parser (i, o), stream (i)): result (i, o)


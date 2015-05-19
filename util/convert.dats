#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"


staload "./util.sats"
staload "./convert.sats"
staload "./list.sats"
staload "./string.sats"
staload sm = "./stream.sats"

#define :: Cons

implement string_to_stream (s) = 
	if empty s
	then $delay $sm.Nil ()
	else $delay $sm.Cons (s[0], string_to_stream (string_range (s, 1, len (s) - 1)))


implement {a} stream_to_list (xs) = 
	case+ !xs of 
	| $sm.Cons (x, xs) => Cons (x, stream_to_list xs)
	| $sm.Nil () => Nil ()
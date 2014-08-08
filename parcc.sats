#include "share/atspre_staload.hats"

staload "util/pair.sats"
staload "lexing/token.sats"
staload "util/maybe.sats"
staload "file/location.sats"
staload "util/list.sats"
staload "util/stream.sats"
staload "util/unit.sats"

datatype result (i:t@ype, o:t@ype) = 
	| Success of (o, i)
	| Failure of (i)

typedef parser (i:t@ype, o:t@ype) = i -<cloref1> result (i, o)


//
// pargen
// 
fun {i:t@ype} {o:t@ype} 	succeed (o): parser (i, o)
fun {i:t@ype} {o:t@ype}		fail (): parser (i, o)

//
// parcom
//
fun {i:t@ype} {o:t@ype} 	alt   (parser (i, o), parser (i, o)): parser (i, o)
fun {i:t@ype} {o1,o2:t@ype}	seq   (parser (i, o1), parser (i, o2)): parser (i, pair (o1, o2))
fun {i:t@ype} {o:t@ype}    	seqs  (list (parser (i, o))): parser (i, list o)
fun {i:t@ype} {o:t@ype}    	sat   (parser (i, o), o -<cloref1> bool): parser (i, o)
fun {i:t@ype} {o:t@ype}    	opt   (parser (i, o)): parser (i, maybe o)
fun {i:t@ype} {o:t@ype}    	rpt0  (parser (i, o)): parser (i, list o)
fun {i:t@ype} {o:t@ype}    	rpt1  (parser (i, o)): parser (i, list o)
fun {i:t@ype} {o:t@ype}    	rptn  (parser (i, o), int): parser (i, list o)
fun {i:t@ype} {o:t@ype}		skip  (parser (i, o)): parser (i, unit)


fun {i:t@ype} {o1,o2:t@ype} bind  (parser (i, o1), o1 -<cloref1> parser (i, o2)): parser (i, o2)
fun {i:t@ype} {o:t@ype} 	apply (parser (i, o), i): result (i, o)

fun {i,o:t@ype} {r:t@ype} 	red   (parser (i, o), f: o -<cloref1> r): parser (i, r)



infixl 20 <|>
overload <|> with alt 

infixl 20 <&>
overload <&> with seq 

postfix 99 ^*
overload ^* with rpt0

postfix 99 ^+
overload ^+ with rpt1 

postfix 99 ^?
overload ^? with opt 

postfix 99 ^#
overload ^# with skip





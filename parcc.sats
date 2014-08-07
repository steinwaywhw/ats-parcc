#include "share/atspre_staload.hats"

staload "pair.sats"
staload "token.sats"
staload "maybe.sats"
staload "location.sats"
staload "list.sats"
staload sm = "stream.sats"


datatype result (i:t@ype, o:t@ype) = 
	| Success of (o, i)
	| Failure of (i)

typedef parser (i:t@ype, o:t@ype) = i -<cloref1> result (i, o)


//
// pargen
// 
fun {i:t@ype} {o:t@ype} 	succeed (o): parser (i, o)

//
// parcom
//
fun {i:t@ype} {o:t@ype} 	alt   (parser (i, o), parser (i, o)): parser (i, o)
fun {i:t@ype} {o1,o2:t@ype}	seq   (parser (i, o1), parser (i, o2)): parser (i, pair (o1, o2))
fun {i:t@ype} {o:t@ype}    	sat   (parser (i, o), o -<cloref1> bool): parser (i, o)
fun {i:t@ype} {o:t@ype}    	opt   (parser (i, o)): parser (i, maybe o)
fun {i:t@ype} {o:t@ype}    	rpt0  (parser (i, o)): parser (i, list o)
fun {i:t@ype} {o:t@ype}    	rpt1  (parser (i, o)): parser (i, list o)

fun {i:t@ype} {o1,o2:t@ype} bind  (parser (i, o1), o1 -<cloref1> parser (i, o2)): parser (i, o2)
fun {i:t@ype} {o:t@ype} 	apply (parser (i, o), i): result (i, o)

(*
overload / with alt
overload > with seq
overload ? with opt
overload * with rpt0
overload + with rpt1

postfix 99 rpt1 prt0 opt
*)

//
// lexer
//
symintr literal
fun lit_char (input: char): parser (lazy ($sm.stream (pair (char, location))), token)
fun lit_string (input: string): parser (lazy ($sm.stream (pair (char, location))), token)
overload literal with lit_char 
overload literal with lit_string

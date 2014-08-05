#include "share/atspre_staload.hats"

staload "pair.sats"
staload "token.sats"
staload "maybe.sats"
staload "location.sats"
staload sm = "stream.sats"

abstype parser (i:t@ype, o:t@ype)

datatype result (i:t@ype, o:t@ype) = 
	| Success of (o, lazy ($sm.stream i))
	| Failure of (lazy ($sm.stream i))
	

//
// pargen
// 
fun succeed {i:t@ype} {o:t@ype} (o): parser (i, o)

//
// parcom
//
fun alt   {i:t@ype} {o:t@ype} 	  (parser (i, o), parser (i, o)): parser (i, o)
fun seq   {i:t@ype} {o1,o2:t@ype} (parser (i, o1), parser (i, o2)): parser (i, pair (o1, o2))
fun sat   {i:t@ype} {o:t@ype}     (parser (i, o), f: o -<cloref1> bool): parser (i, o)
fun opt   {i:t@ype} {o:t@ype}     (parser (i, o)): parser (i, maybe (o))
fun rpt0  {i:t@ype} {o:t@ype}     (parser (i, o)): parser (i, o)
fun rpt1  {i:t@ype} {o:t@ype}     (parser (i, o)): parser (i, o)

fun bind  {i:t@ype} {o1,o2:t@ype} (p: parser (i, o1), f: o1 -<cloref1> parser (i, o2)): parser (i, o2)
fun apply {i:t@ype} {o:t@ype} 	  (parser (i, o), lazy ($sm.stream i)): result (i, o)

(*
symintr ||
symintr &&
symintr ~
*)
symintr ||
symintr &&
symintr ~
symintr ^+
symintr ^*
symintr ^?


postfix 99 ^?
postfix 99 ^*
postfix 99 ^+
prefix 60 ~


overload || with alt
overload && with seq
overload ^? with opt
overload ^* with rpt0
overload ^+ with rpt1



//
// lexer
//
symintr literal
fun lit_char (input: char): parser (pair (char, location), token)
//fun lit_string (input: string): parser (pair (char, location), token)
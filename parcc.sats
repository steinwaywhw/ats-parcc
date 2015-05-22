#include "share/atspre_staload.hats"

staload "util/util.sats"
staload "util/list.sats"
staload "util/pair.sats"
staload "util/unit.sats"
staload "util/maybe.sats"
staload sm = "util/stream.sats"

datatype result (i:t@ype, o:t@ype) = 
    | Success of (o, i)
    | Failure of (i)

abstype parser (i:t@ype, o:t@ype, epsilon:bool) = ptr
typedef parser (o:t@ype, epsilon:bool) = parser (lazy ($sm.stream char), o, epsilon)
typedef parser (o:t@ype) = parser (lazy ($sm.stream char), o, false)

(*
 *  parsers
 *)

fun spaces ()              : parser (unit, true) // \s*
fun space ()               : parser char // \s
fun spacetab ()            : parser char // \s\t
fun ws ()                  : parser (unit, true) // \s\t\n\v\f\r *
fun newline ()             : parser char // \n
fun tab ()                 : parser char // \t
fun uppercase ()           : parser char // [A-Z]
fun lowercase ()           : parser char // [a-z]
fun alpha ()               : parser char // [a-zA-Z]
fun alphadigit ()          : parser char // [a-zA-Z0-9]
fun digit ()               : parser char // [0-9]
fun hexdigit ()            : parser char // [0-9A-Fa-f]
fun octdigit ()            : parser char // [0-7]
fun anychar ()             : parser char // .
fun printable ()           : parser char // printable
fun escape ()              : parser char // c escape
fun symbol ()              : parser char // !#$%&*+-/<=>?@\^.|~ (that is, not "'`()[]{},:;_)
fun litchar (char)         : parser char
fun oneof (string)         : parser char
fun noneof (string)        : parser char
    
fun litstring (string)     : parser string
fun alphas ()              : parser string // [a-zA-Z]+
fun digits ()              : parser string // [0-9]+
fun alphadigits()          : parser string // [a-zA-Z0-9]+
fun hexdigits ()           : parser string // [a-fA-F0-9]+
fun octdigits ()           : parser string // [0-7]+
fun uppercases ()          : parser string // [A-Z]+
fun lowercases ()          : parser string // [a-z]+
fun symbols ()             : parser string // symbol+

fun eof ()                 : parser (unit, true)

(*
 *  misc parsers
 *)

fun id (): parser string // [a-zA-Z_][a-zA-Z0-9_]*

//fun char_single_quote ()     : parser char   // '_'
//fun string_double_quote ()   : parser string // "one line string"
//fun string_backtip ()        : parser string // `one line string`
//fun string_triple_backtip () : parser string // ```multi line string```

(*
 *  combinators
 *)

//fun {i:t@ype} {o:t@ype}       mk       (i -<cloref1> result (i, o)):  parser (i, o)
//fun {i:t@ype} {o:t@ype}       alts     {e:bool} (list (parser (i, o, e))):   [e:bool]                             parser (i, o, e )
//fun {i:t@ype} {o:t@ype}       seqs     (list (parser (i, o))):                                parser (i, list o)

fun {i:t@ype} {o:t@ype}       succeed (o): parser (i, o, true)
fun {i:t@ype} {o:t@ype}       fail ():     parser (i, o, true)

fun {i:t@ype} {o:t@ype}       apply    {e:bool} (parser (i, o, e), i):     result (i, o)
fun {i:t@ype} {o:t@ype}       force    {e:bool} (lazy (parser (i, o, e))): parser (i, o, e)

fun {i:t@ype} {o:t@ype}       alt      {e1,e2:bool}   (parser (i, o, e1), parser (i, o, e2)):                                     parser (i, o, e1 || e2)
fun {i:t@ype} {o1,o2:t@ype}   seq      {e1,e2:bool}   (parser (i, o1, e1), parser (i, o2, e2)):                                   parser (i, pair (o1, o2), e1 && e2)
fun {i:t@ype} {o,o1:t@ype}    seqr     {e1,e2:bool}   (parser (i, o1, e1), parser (i, o, e2)):                                    parser (i, o, e1 && e2) // keep right, disgard left
fun {i:t@ype} {o,o1:t@ype}    seql     {e1,e2:bool}   (parser (i, o, e1), parser (i, o1, e2)):                                    parser (i, o, e1 && e2) // keep left, disgard right
fun {i:t@ype} {o:t@ype}       sat      {e:bool}       (parser (i, o, e), o -<cloref1> bool):                                      parser (i, o, e)
fun {i:t@ype} {o:t@ype}       opt      {e:bool}       (parser (i, o, e)):                                                         parser (i, maybe o, e)
fun {i:t@ype} {o:t@ype}       rpt0                    (parser (i, o, false)):                                                     parser (i, list o, true)
fun {i:t@ype} {o:t@ype}       rpt1                    (parser (i, o, false)):                                                     parser (i, list o, false)
fun {i:t@ype} {o:t@ype}       rptn                    (parser (i, o, false), int):                                                parser (i, list o, false)
fun {i:t@ype} {o1,o2:t@ype}   rptuntil {e:bool}       (p: parser (i, o1, false), e: parser (i, o2, e)):                           parser (i, list o1, e)
fun {i:t@ype} {o:t@ype}       skip     {e:bool}       (parser (i, o, e)):                                                         parser (i, unit, e)
fun {i:t@ype} {o:t@ype}       not      {e:bool}       (parser (i, o, e)):                                                         parser (i, unit, true)
fun {i,o:t@ype} {r:t@ype}     red      {e:bool}       (parser (i, o, e), f: o -<cloref1> r):                                      parser (i, r, e)
fun {i:t@ype} {o1,o2:t@ype}   bind     {e1,e2:bool}   (parser (i, o1, e1), o1 -<cloref1> parser (i, o2, e2)):                     parser (i, o2, e1 && e2)
fun {i:t@ype} {o,o1,o2:t@ype} between  {e,e1,e2:bool} (p: parser (i, o, e), open: parser (i, o1, e1), close: parser (i, o2, e2)): parser (i, o, e && e1 && e2)
fun {i:t@ype} {o1,o2:t@ype}   sepby0   {e1,e2:bool|e1&&e2==false} (p: parser (i, o1, e1), sep: parser (i, o2, e2)):               parser (i, list o1, true)
fun {i:t@ype} {o1,o2:t@ype}   sepby1   {e1,e2:bool|e1&&e2==false} (p: parser (i, o1, e1), sep: parser (i, o2, e2)):               parser (i, list o1, e1)

fun {o:t@ype}     show_result        (result (lazy ($sm.stream char), o), o -> void): void
fun               show_result_char   (result (lazy ($sm.stream char), char))        : void
fun               show_result_string (result (lazy ($sm.stream char), string))      : void
fun               show_result_int    (result (lazy ($sm.stream char), int))         : void
fun               show_result_double (result (lazy ($sm.stream char), double))      : void
fun               show_result_bool   (result (lazy ($sm.stream char), bool))        : void
fun               show_result_unit   (result (lazy ($sm.stream char), unit))        : void

overload show with show_result
overload show with show_result_int
overload show with show_result_string
overload show with show_result_char
overload show with show_result_bool
overload show with show_result_double
overload show with show_result_unit


(* 
 *  test
 *)

fun parser_test (): void 

//infixl +>
//infixl <+
//infixl <+>
//infixl <|>

//overload +> with seqr 
//overload <+ with seql
//overload <+> with seq 
//overload <|> with alt 




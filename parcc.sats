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

abstype parser (i:t@ype, o:t@ype) = ptr
typedef parser (o:t@ype) = parser (lazy ($sm.stream char), o)


(*
 *  parsers
 *)

fun spaces ()              : lazy (parser unit) // \s*
fun space ()               : lazy (parser char) // \s
fun spacetab ()            : lazy (parser char) // \s\t
fun ws ()                  : lazy (parser unit) // \s\t\n\v\f\r *
fun newline ()             : lazy (parser char) // \n
fun tab ()                 : lazy (parser char) // \t
fun uppercase ()           : lazy (parser char) // [A-Z]
fun lowercase ()           : lazy (parser char) // [a-z]
fun alpha ()               : lazy (parser char) // [a-zA-Z]
fun alphadigit ()          : lazy (parser char) // [a-zA-Z0-9]
fun digit ()               : lazy (parser char) // [0-9]
fun hexdigit ()            : lazy (parser char) // [0-9A-Fa-f]
fun octdigit ()            : lazy (parser char) // [0-7]
fun anychar ()             : lazy (parser char) // .
fun printable ()           : lazy (parser char) // printable
fun escape ()              : lazy (parser char) // c escape
fun symbol ()              : lazy (parser char) // !#$%&*+-/<=>?@\^.|~ (that is, not "'`()[]{},:;_)
fun litchar (char)         : lazy (parser char)
fun oneof (string)         : lazy (parser char)
fun noneof (string)        : lazy (parser char)
    
fun litstring (string)     : lazy (parser string)
fun alphas ()              : lazy (parser string) // [a-zA-Z]+
fun digits ()              : lazy (parser string) // [0-9]+
fun alphadigits()          : lazy (parser string) // [a-zA-Z0-9]+
fun hexdigits ()           : lazy (parser string) // [a-fA-F0-9]+
fun octdigits ()           : lazy (parser string) // [0-7]+
fun uppercases ()          : lazy (parser string) // [A-Z]+
fun lowercases ()          : lazy (parser string) // [a-z]+
fun symbols ()             : lazy (parser string) // symbol+

fun eof ()                 : lazy (parser unit)

(*
 *  misc parsers
 *)

fun id (): lazy (parser string) // [a-zA-Z_][a-zA-Z0-9_]*

//fun char_single_quote ()     : parser char   // '_'
//fun string_double_quote ()   : parser string // "one line string"
//fun string_backtip ()        : parser string // `one line string`
//fun string_triple_backtip () : parser string // ```multi line string```

(*
 *  combinators
 *)

fun {i:t@ype} {o:t@ype}       succeed (o): lazy (parser (i, o))
fun {i:t@ype} {o:t@ype}       fail ():     lazy (parser (i, o))

fun {i:t@ype} {o:t@ype}       apply    (lazy (parser (i, o)), i):     result (i, o)
fun {i:t@ype} {o:t@ype}       force    (lazy (parser (i, o))):        parser (i, o)
fun {i:t@ype} {o:t@ype}       mk       (i -<cloref1> result (i, o)):  parser (i, o)

fun {i:t@ype} {o:t@ype}       alt      (lazy (parser (i, o)), lazy (parser (i, o))):                 lazy (parser (i, o))
fun {i:t@ype} {o:t@ype}       alts     (list (lazy (parser (i, o)))):                                lazy (parser (i, o))
fun {i:t@ype} {o1,o2:t@ype}   seq      (lazy (parser (i, o1)), lazy (parser (i, o2))):               lazy (parser (i, pair (o1, o2)))
fun {i:t@ype} {o:t@ype}       seqs     (list (lazy (parser (i, o)))):                                lazy (parser (i, list o))
fun {i:t@ype} {o,o1:t@ype}    seqr     (lazy (parser (i, o1)), lazy (parser (i, o))):                lazy (parser (i, o)) // keep right, disgard left
fun {i:t@ype} {o,o1:t@ype}    seql     (lazy (parser (i, o)), lazy (parser (i, o1))):                lazy (parser (i, o)) // keep left, disgard right
fun {i:t@ype} {o:t@ype}       sat      (lazy (parser (i, o)), o -<cloref1> bool):                    lazy (parser (i, o))
fun {i:t@ype} {o:t@ype}       opt      (lazy (parser (i, o))):                                       lazy (parser (i, maybe o))
fun {i:t@ype} {o:t@ype}       rpt0     (lazy (parser (i, o))):                                       lazy (parser (i, list o))
fun {i:t@ype} {o:t@ype}       rpt1     (lazy (parser (i, o))):                                       lazy (parser (i, list o))
fun {i:t@ype} {o:t@ype}       rptn     (lazy (parser (i, o)), int):                                  lazy (parser (i, list o))
fun {i:t@ype} {o1,o2:t@ype}   rptuntil (p: lazy (parser (i, o1)), e: lazy (parser (i, o2))):         lazy (parser (i, list o1))
fun {i:t@ype} {o:t@ype}       skip     (lazy (parser (i, o))):                                       lazy (parser (i, unit))
fun {i:t@ype} {o1,o2:t@ype}   sepby0   (p: lazy (parser (i, o1)), sep: lazy (parser (i, o2))):       lazy (parser (i, list o1))
fun {i:t@ype} {o1,o2:t@ype}   sepby1   (p: lazy (parser (i, o1)), sep: lazy (parser (i, o2))):       lazy (parser (i, list o1))
fun {i:t@ype} {o:t@ype}       not      (lazy (parser (i, o))):                                       lazy (parser (i, unit))
fun {i,o:t@ype} {r:t@ype}     red      (lazy (parser (i, o)), f: o -<cloref1> r):                    lazy (parser (i, r))
fun {i:t@ype} {o1,o2:t@ype}   bind     (lazy (parser (i, o1)), o1 -<cloref1> lazy (parser (i, o2))): lazy (parser (i, o2))
fun {i:t@ype} {o,o1,o2:t@ype} between  (p: lazy (parser (i, o)), open: lazy (parser (i, o1)), close: lazy (parser (i, o2))): lazy (parser (i, o))

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




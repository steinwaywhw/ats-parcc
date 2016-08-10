staload "symintr.sats"
staload "list.sats"
staload "maybe.sats"


typedef nat = intGte 0

abstype parser (a:t@ype) = ptr 

abstype input_t  = ptr
abstype output_t = ptr 

typedef cont_t (a:t@ype) = (a, input_t) -<cloref1> output_t 
typedef cont_t = [a:t@ype] cont_t a

typedef par_t  (a:t@ype) = (input_t, cont_t a) -<cloref1> output_t
typedef par_t = [a:t@ype] par_t a
 
castfn parser_encode {a:t@ype} (par_t a): parser a
castfn parser_decode {a:t@ype} (parser a): par_t a

fun {a:t@ype}     parser_apply   (parser a, input_t, cont_t a): output_t

fun {a:t@ype}     parser_fail    (): parser (a)
fun {a:t@ype}     parser_succeed (a): parser (a)
  
fun {a:t@ype}     parcc_maybe (parser a): parser (maybe a)
fun {a,b:t@ype}   parcc_bind  (parser a, a -<cloref1> parser b): parser b

fun {a,b:t@ype}   parcc_seq   (parser a, parser b): parser ($tup(a, b))
fun {a,b,c:t@ype} parcc_seq3  (parser a, parser b, parser c): parser ($tup(a, b, c))
fun {a:t@ype}     parcc_seqs  (list (parser a)): parser (list a)
fun {a:t@ype}     parcc_alt   (parser a, parser a): parser a
fun {a:t@ype}     parcc_alt3  (parser a, parser a, parser a): parser a
fun {a:t@ype}     parcc_alts  (list (parser a)): parser a
 
fun {a:t@ype}     parcc_sat   (parser a, a -<cloref1> bool): parser a
fun {a:t@ype}     parcc_skip  (parser a): parser void
fun {a,b:t@ype}   parcc_map   (parser a, a -<cloref1> b): parser b
fun {a:t@ype}     parcc_rptn  (parser a, nat): parser (list a)
fun {a:t@ype}     parcc_rpt1  (parser a): parser (list a)
fun {a:t@ype}     parcc_rpt0  (parser a): parser (list a)
fun {a,b:t@ype}   parcc_sepby (parser a, parser b): parser (list a)

symintr memo 
fun {o:t@ype}  		memo0 (() -<cloref1> o): () -<cloref1> o
fun {i,o:t@ype} 	memo1 (i -<cloref1> o): i -<cloref1> o 
fun {i1,i2,o:t@ype} memo2 ((i1, i2) -<cloref1> o): (i1, i2) -<cloref1> o 
overload memo with memo0
overload memo with memo1 
overload memo with memo2

exception ParsingException of (string)
//fun 
//fun {a:t@ype} memo_parser (par_t a): par_t a










////

(* types *)

abstype input_t = ptr
//abstype output_t = ptr

datatype result (o:t@ype) = 
    | Success of (o, input_t)
    | Failure of (input_t)

typedef cont (o:t@ype, a:t@ype) = result o -<cloref1> a

abstype parser (o:t@ype) = ptr
//typedef parser (o:t@ype) = parser (lazy ($sm.stream char), o)

typedef parserfun (o:t@ype, a:t@ype) = (input_t, cont (o, a)) -<cloref1> a

castfn mk   {o,a:t@ype} (parserfun (o, a)): parser o
castfn unmk {o,a:t@ype} (parser o): parserfun (o, a)

//absvtype trampoline 

(* show functions *)

fun {o:t@ype}     show_result        (result o, o -> void): void
fun               show_result_char   (result char)        : void
fun               show_result_string (result string)      : void
fun               show_result_int    (result int)         : void
fun               show_result_double (result double)      : void
fun               show_result_bool   (result bool)        : void
fun               show_result_unit   (result unit)        : void

overload show with show_result
overload show with show_result_int
overload show with show_result_string
overload show with show_result_char
overload show with show_result_bool
overload show with show_result_double
overload show with show_result_unit

(* basic combinators *)

fun {o:t@ype}     succeed (o): parser o
fun {o:t@ype}     fail    (): parser o
fun {o1,o2:t@ype} bind    (parser o1, o1 -<cloref1> parser o2): parser o2
fun {o,a:t@ype}   apply   (parser o, input_t, cont (o, a)): a

fun {o1,o2:t@ype} seq     (parser o1, parser o2): parser (pair (o1, o2))
fun {o:t@ype}     alt     (parser o, parser o): parser o

fun {o,a:t@ype}   memo_cps (parser o): parser o


////
fun {i:t@ype} {o:t@ype}       force   (lazy (parser (i, o, e))): parser (i, o, e)

fun {i:t@ype} {o:t@ype}       alt     (parser (i, o, e1), parser (i, o, e2)):                                         parser (i, o, e1 || e2)





////

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



//fun {i:t@ype} {o:t@ype}       alts     {e:bool} (list (parser (i, o, e))):   [e:bool]                             parser (i, o, e )
//fun {i:t@ype} {o:t@ype}       seqs     (list (parser (i, o))):                                parser (i, list o)

fun {i:t@ype} {o,o1:t@ype}    seqr     {e1,e2:bool}   (parser (i, o1, e1), parser (i, o, e2)):                                        parser (i, o, e1 && e2) // keep right, disgard left
fun {i:t@ype} {o,o1:t@ype}    seql     {e1,e2:bool}   (parser (i, o, e1), parser (i, o1, e2)):                                        parser (i, o, e1 && e2) // keep left, disgard right
fun {i:t@ype} {o:t@ype}       sat      {e:bool}       (parser (i, o, e), o -<cloref1> bool):                                          parser (i, o, e)
fun {i:t@ype} {o:t@ype}       opt      {e:bool}       (parser (i, o, e)):                                                             parser (i, maybe o, e)
fun {i:t@ype} {o:t@ype}       rpt0                    (parser (i, o, false)):                                                         parser (i, list o, true)
fun {i:t@ype} {o:t@ype}       rpt1                    (parser (i, o, false)):                                                         parser (i, list o, false)
fun {i:t@ype} {o:t@ype}       rptn     {n:nat}        (parser (i, o, false), int n):                                                  parser (i, list o, n == 0)
fun {i:t@ype} {o1,o2:t@ype}   rptuntil {e:bool}       (p: parser (i, o1, false), e: parser (i, o2, e)):                               parser (i, list o1, e)
fun {i:t@ype} {o:t@ype}       skip     {e:bool}       (parser (i, o, e)):                                                             parser (i, unit, e)
fun {i:t@ype} {o:t@ype}       not      {e:bool}       (parser (i, o, e)):                                                             parser (i, unit, true)
fun {i,o:t@ype} {r:t@ype}     red      {e:bool}       (parser (i, o, e), f: o -<cloref1> r):                                          parser (i, r, e)
fun {i:t@ype} {o,o1,o2:t@ype} between  {e1,e2:bool}   (p: parser (i, o, false), open: parser (i, o1, e1), close: parser (i, o2, e2)): parser (i, o, false)
fun {i:t@ype} {o1,o2:t@ype}   sepby0   (*{e1,e2:bool|(e1&&e2)==false}*) {e:bool} (p: parser (i, o1, false), sep: parser (i, o2, e)):                 parser (i, list o1, true)
fun {i:t@ype} {o1,o2:t@ype}   sepby1   (*{e1,e2:bool|(e1&&e2)==false}*) {e:bool} (p: parser (i, o1, false), sep: parser (i, o2, e)):                 parser (i, list o1, false)




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




#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "util/util.sats"
staload "util/list.sats"
staload "util/unit.sats"
staload "util/maybe.sats"
staload "util/pair.sats"
staload "util/string.sats"
staload sm = "util/stream.sats"

staload _ = "util/list.dats"
staload _ = "util/stream.dats"
staload _ = "util/pair.dats"

staload "parcc.sats"

(*
 *  type
 *)

assume parser (i:t@ype, o:t@ype) = i -<cloref1> result (i, o)

#define :: Cons

(* 
 *  show
 *)

implement {o} show_result (r, f) =
    case+ r of 
    | Success (o, _) => f o 
    | Failure i      => () where {
        val _ = show "fail ("
        val _ = $sm.show (i, 10)
        val _ = show ")"
    }

implement show_result_int (r)    = show_result (r, lam x => show x)
implement show_result_string (r) = show_result (r, lam x => show x)
implement show_result_char (r)   = show_result (r, lam x => show x)
implement show_result_double (r) = show_result (r, lam x => show x)
implement show_result_bool (r)   = show_result (r, lam x => show x)
implement show_result_unit (r)   = show_result (r, lam x => show "unit")

(*
 *  combinators
 *)

implement {i} {o} alt (a, b) =
    lam input =<cloref1> 
        case+ apply (a, input) of 
        | Success (ret, rest) => Success (ret, rest)
        | Failure _ => apply (b, input)


implement {i} {o} alts (ps) = 
    list_foldr (
        ps, 
        fail (), 
        lam (a, b) => a \alt b)


implement {i} {o1,o2} seq (a, b) = 
    a \bind (lam x => b \bind (lam y => succeed (Pair (x, y))))

//    bind (a, 
//        lam x => 
//            bind (b, lam y => succeed (Pair (x, y))))

implement {i} {o} seqs (ps) = 
    list_foldr (
        ps,
        succeed (Nil ()),
        lam (a, b) => a \bind (lam x => b \bind (lam y => succeed (x :: y)))
    )



(*
    $delay (
        list_foldr (
            ps,
            succeed (Nil ()),
            lam (a, b) => 
                bind (a, lam x => 
                    bind (b, lam y => 
                        succeed (x :: y))))
    )*)

implement {i} {o,o1} seqr (p, r) = 
    red (seq (skip p, r), lam x => snd x)

implement {i} {o,o1} seql (l, p) = 
    red (seq (l, skip p), lam x => fst x)

implement {i} {o} sat (p, f) = 
    lam input =<cloref1> 
        case+ apply (p, input) of 
        | Failure _ => Failure input 
        | Success (ret, rest) =>
            if f ret
            then Success (ret, rest) 
            else Failure (input)


implement {i} {o} opt (p) = 
    lam input =<cloref1> 
        case+ apply (p, input) of 
        | Success (ret, rest) => Success (Just ret, rest)
        | Failure (input) => Success (Nothing (), input)


implement {i} {o} rpt1 (p) = 
    p \bind (lam x => alt (red (rpt1 p, lam y => x :: y), succeed (x :: Nil ())))

implement {i} {o} rpt0 (p) = (rpt1 p) \alt (succeed (Nil ()))

implement {i} {o} rptn (p, n) = 
    if n <= 0
    then succeed (Nil ())
    else p \bind (lam x => (rptn (p, n-1)) \bind (lam y => succeed (x :: y)))

//implement {i} {o1,o2} rptuntil (p, e) = let 
//    val trye = 
//        (lam input => 
//            case+ apply (e, input) of 
//            | Success (_, _) => Success (Nil (), input)
//            | Failure (_) => Failure (input)): parser (list o1)
//in 
//    alt (trye, succeed (Nil ()))
//end


implement {i} {o} skip (p) = p \bind (lam x => succeed (Unit ()))

implement {i} {o1,o2} sepby1 (p, sep) = 
    red (
        seq (p, rpt0 (sep \seqr p)),
        lam x => fst x :: snd x)

implement {i} {o1,o2} sepby0 (p, sep) = 
    alt (p \sepby1 sep, succeed (Nil ()))

implement {i} {o,o1,o2} between (p, open, close) = 
    open \bind (lam _ => p \bind (lam x => close \bind (lam _ => succeed x)))

implement {i} {o} not (p) = 
    lam input =<cloref1>
        case+ apply (p, input) of 
        | Success (_, _) => Failure input 
        | Failure _ => Success (Unit (), input)

implement {i} {o} force (p) = lam input => apply (!p, input)

implement {i} {o} apply (p, s) = p (s)

implement {i, o} {r} red (p, f) = p \bind (lam x => succeed (f x))

implement {i} {o1,o2} bind (p, f) = 
    lam input =<cloref1> 
        case+ apply (p, input) of 
            | Success (ret, rest) => apply (f (ret), rest)
            | Failure _      => Failure (input)


(*
 *  parsers
 *)

implement {i} {o} succeed (ret) = lam input =<cloref1> Success (ret, input)
implement {i} {o} fail () = lam input =<cloref1> Failure (input)

implement anychar () = 
    lam input =<cloref1>
        case+ !input of 
        | $sm.Nil () => Failure (input) 
        | $sm.Cons (x, rest) => Success (x, rest)//): 
    //lazy ($sm.stream char) -<cloref1> result (lazy ($sm.stream char), char)


implement spaces ()     = skip (rpt0 (litchar ' '))
implement space ()      = litchar ' '
implement spacetab ()   = space () \alt tab ()
implement ws ()         = skip (rpt0 (oneof (" \t\n\v\f\r")))
implement newline ()    = litchar '\n'
implement tab ()        = litchar '\t'
implement uppercase ()  = anychar () \sat (lam x => isupper x)
implement lowercase ()  = anychar () \sat (lam x => islower x)
implement alpha ()      = anychar () \sat (lam x => isalpha x)
implement alphadigit () = anychar () \sat (lam x => isalnum x)
implement digit ()      = anychar () \sat (lam x => isdigit x)
implement hexdigit ()   = anychar () \sat (lam x => isxdigit x)
implement octdigit ()   = anychar () \sat (lam x => x >= '0' && x <= '7')
implement printable ()  = anychar () \sat (lam x => isprint x)
implement symbol ()     = oneof ("!#$%&*+-./<=>?@\\^|~")
implement litchar (c)   = anychar () \sat (lam x => x = c)
implement oneof (s)     = anychar () \sat (lam x => string_find (s, string_from_char x) >= 0)
implement noneof (s)    = anychar () \sat (lam x => string_find (s, string_from_char x) < 0)
implement escape ()     = let
    val case1 = red (
                    seq (litchar '\\', oneof "abfnrtv\\\'\"\?`"), 
                    lam x => case+ snd x of
                        | 'a' => '\a'
                        | 'b' => '\b'
                        | 'f' => '\f'
                        | 'n' => '\n'
                        | 'r' => '\r'
                        | 't' => '\t'
                        | 'v' => '\v'
                        | ch  =>> ch)
    
    val case2 = red (
            seq (
                litchar '\\', 
                red (
                    rptn (red (octdigit (), lam x => x - '0'), 3),
                    lam xs => list_foldl<int><int> (xs, 0, lam (x, y) => y + x * 8))), 
            lam x => int2char0 (snd x))

    val case3 = red (
            seq (
                litstring "\\x", 
                red (
                    rptn (red (hexdigit (), lam x => if isdigit x then x - '0' else ((tolower x) - 'a') + 10), 2),
                    lam xs => list_foldl<int><int> (xs, 0, lam (x, y) => y + x * 16))),
            lam x => int2char0 (snd x))
in 
    alts (case1 :: case2 :: case3 :: Nil ()) 
end

implement litstring (match) = let 
    fun genpar (index: int):<cloref1> parser unit =
        if index = len (match) - 1
        then skip (litchar match[index])
        else skip (seq (skip (litchar match[index]), genpar (index+1)))
in 
    red (genpar (0), lam _ => match)
end

implement alphas ()     = red (rpt1 (alpha ()), lam x => string_unexplode x)
implement digits ()     = red (rpt1 (digit ()), lam x => string_unexplode x)
implement alphadigits() = red (rpt1 (alphadigit ()), lam x => string_unexplode x)
implement hexdigits ()  = red (rpt1 (hexdigit ()), lam x => string_unexplode x)
implement octdigits ()  = red (rpt1 (octdigit ()), lam x => string_unexplode x)
implement uppercases () = red (rpt1 (uppercase ()), lam x => string_unexplode x)
implement lowercases () = red (rpt1 (lowercase ()), lam x => string_unexplode x)
implement symbols ()    = red (rpt1 (symbol ()), lam x => string_unexplode x)

implement eof () = 
    lam (input: lazy ($sm.stream char)) =<cloref1> 
        case+ !input of 
        | $sm.Nil _ => Success (Unit (), input)
        | _ => Failure (input)


implement id () = let 
    val case1 = red (alt (alpha(), litchar '_'), lam x => string_from_char x)
    val case2 = red (rpt1 (alt (alphadigit(), litchar '_')), lam x => string_unexplode x)
in 
    alt (case1, red (seq (case1, case2), lam x => string_concat (fst x, snd x)))
end 


////
staload "util/convert.sats"

implement parser_test () = () where {
    val sep = "\n==========================\n"
    val _ = show (apply (id (), string_to_stream "x "))
    val _ = show sep 
    val _ = show (apply (spaces (), string_to_stream "   c"))
    val _ = show sep 
    val _ = show (apply (spaces (), string_to_stream "a   c"))
    val _ = show sep 
    val _ = show (apply (space (), string_to_stream "   c"))
    val _ = show sep 
    val _ = show (apply (space (), string_to_stream "c   c"))
    val _ = show sep 
    val _ = show (apply (spacetab (), string_to_stream "\t cdefg"))
    val _ = show sep 
    val _ = show (apply (spacetab (), string_to_stream " \tcdefg"))
    val _ = show sep 
    val _ = show (apply (spacetab (), string_to_stream "cdefg"))
    val _ = show sep 
    val _ = show (apply (ws (), string_to_stream "\t\n\v\f\r end"))
    val _ = show sep 
    val _ = show (apply (newline (), string_to_stream "\ncdefg"))
    val _ = show sep 
    val _ = show (apply (newline (), string_to_stream "a\ncdefg"))
    val _ = show sep 
    val _ = show (apply (tab (), string_to_stream "\tcdefg"))
    val _ = show sep 
    val _ = show (apply (tab (), string_to_stream "a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (uppercase (), string_to_stream "ACcdefg"))
    val _ = show sep 
    val _ = show (apply (uppercase (), string_to_stream "accdefg"))
    val _ = show sep 
    val _ = show (apply (lowercase (), string_to_stream "accdefg"))
    val _ = show sep 
    val _ = show (apply (lowercase (), string_to_stream "aCcdefg"))
    val _ = show sep 
    val _ = show (apply (alpha (), string_to_stream "\tcdefg"))
    val _ = show sep 
    val _ = show (apply (alpha (), string_to_stream "a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (alphadigit (), string_to_stream "a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (alphadigit (), string_to_stream "1a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (alphadigit (), string_to_stream "\tcdefg"))
    val _ = show sep 
    val _ = show (apply (digit (), string_to_stream "1\tcdefg"))
    val _ = show sep 
    val _ = show (apply (digit (), string_to_stream "a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (hexdigit (), string_to_stream "1\tcdefg"))
    val _ = show sep 
    val _ = show (apply (hexdigit (), string_to_stream "a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (hexdigit (), string_to_stream "g\tcdefg"))
    val _ = show sep 
    val _ = show (apply (octdigit (), string_to_stream "0\tcdefg"))
    val _ = show sep 
    val _ = show (apply (octdigit (), string_to_stream "8a\tcdefg"))
    val _ = show sep 
    val _ = show (apply (anychar (), string_to_stream "abcde"))
    val _ = show sep 
    val _ = show (apply (printable (), string_to_stream "cdefg"))
    val _ = show sep 
    val _ = show (apply (printable (), string_to_stream "\bcdefg"))
    val _ = show sep 
    val _ = show (apply (escape (), string_to_stream "\\t"))
    val _ = show sep 
    val _ = show (apply (litchar 'c', string_to_stream "cdefg"))
    val _ = show sep 
    val _ = show (apply (litstring "hello", string_to_stream "hello"))
    val _ = show sep 
    val _ = show (apply (oneof "cde", string_to_stream "eab"))
    val _ = show sep 
    val _ = show (apply (noneof "cde", string_to_stream "eab"))
    val _ = show sep 
    


    val _ = show (apply (digits (), string_to_stream "123a"))
    val _ = show sep 
    val _ = show (apply (hexdigits (), string_to_stream "a112387x"))
    val _ = show sep 
    val _ = show (apply (alphas (), string_to_stream "avasd1"))
    val _ = show sep 
    val _ = show (apply (alphadigits (), string_to_stream "ansdj123lkasdj\t"))
    val _ = show sep 
    val _ = show (apply (hexdigits (), string_to_stream "abcdef123451l"))
    val _ = show sep 
//  val _ = show (apply (string_double_quote (), string_to_stream "\"\\tbcde\""))
//  val _ = show sep 
//  val _ = show (apply (string_double_quote (), string_to_stream "\"\tbcde\""))
//  val _ = show sep 
//  val _ = show (apply (string_double_quote (), string_to_stream "\"a\"bcde\""))
//  val _ = show sep 
//  val _ = show (apply (string_double_quote (), string_to_stream "\"abc\\\"de\""))
//  val _ = show sep 
//  val _ = show (apply (string_double_quote (), string_to_stream "\"abc\nde\""))
//  val _ = show sep 
//  val _ = show (apply (string_double_quote (), string_to_stream "\"abc'de\""))
//  val _ = show sep 
//  val _ = show (apply (string_backtip (), string_to_stream "`abcde`"))
//  val _ = show sep 
//  val _ = show (apply (string_backtip (), string_to_stream "`\"abcde`"))
//  val _ = show sep 
//  val _ = show (apply (string_backtip (), string_to_stream "`\\abcde`"))
//  val _ = show sep 
//  val _ = show (apply (string_backtip (), string_to_stream "``abcde`"))
//  val _ = show sep 
//  val _ = show (apply (string_backtip (), string_to_stream "`ab`cde`"))
//  val _ = show sep 
//  val _ = show (apply (string_backtip (), string_to_stream "`\\`abcde`"))
//  val _ = show sep 
//  val _ = show (apply (string_multiline (), string_to_stream "```abc\nde`fs``sa````dasd```asd```"))
//  val _ = show sep 
//  val _ = show (apply (signed_int_dec (), string_to_stream "-109"))
//  val _ = show sep 
//  val _ = show (apply (unsigned_int_hex (), string_to_stream "0fF"))
//  val _ = show sep 
//  val _ = show (apply (unsigned_int_bin (), string_to_stream "101"))
//  val _ = show sep 
//  val _ = show (apply (unsigned_int_oct (), string_to_stream "0777"))
//  val _ = show sep 
//  val _ = show (apply (exponent (), string_to_stream "e+10"))
//  val _ = show sep 
//  val _ = show (apply (unsigned_double_dec (), string_to_stream "10.900"))
//  val _ = show sep 
//  val _ = show (apply (signed_double_dec (), string_to_stream "-10.900"))
//  val _ = show sep 
//  val _ = show (apply (boolean (), string_to_stream "true"))
//  val _ = show sep 
}
 

implement main0 () = parser_test ()
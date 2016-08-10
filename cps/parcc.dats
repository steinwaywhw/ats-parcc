#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"
#define ATS_DYNLOADFLAG 0

staload "parcc.sats"

staload "symintr.sats"
staload "list.sats"
staload "maybe.sats"
staload _ = "list.dats"
staload _ = "maybe.dats"


#define :: ListCons 
#define nil ListNil

implement (a) gcompare_val_val<parser a> (x, y) = 
    gcompare_val_val<ref(void)> ($UN.cast{ref(void)} x, $UN.cast{ref(void)} y)

implement {a} parcc_delay (p) = 
    parser_encode (lam (input, cont) => 
        parser_apply (p (), input, cont))

implement {a} parser_apply (p, input, cont) = 
    (parser_decode p)(input, cont)

implement {a} parser_fail () = 
    parser_encode (lam (input, cont) => 
        $raise ParsingException ("parser_fail"))

implement {a} parser_succeed (x) = 
    parser_encode (lam (input, cont) => 
        cont (x, input))

implement {a} parcc_maybe (p) = 
    parser_encode (lam (input, cont) => 
        try parser_apply (p, input, lam (result, rest) => cont (Just result, rest))
        with ~ParsingException (str) => (println! str; cont (Nothing (), input)))

implement {a,b} parcc_bind (p, f) = 
    parser_encode (lam (input, cont) => 
        parser_apply (p, input, lam (result, rest) => parser_apply (f result, rest, cont)))

implement {a,b} parcc_seq (pa, pb) = 
    pa \parcc_bind (lam a => 
        pb \parcc_bind (lam b => 
            parser_succeed ($tup(a, b))))

implement {a,b,c} parcc_seq3 (pa, pb, pc) =
    pa \parcc_bind (lam a => 
        pb \parcc_bind (lam b => 
            pc \parcc_bind (lam c 
                => parser_succeed ($tup(a, b, c)))))

implement {a} parcc_seqs (pas) =
    case+ pas of 
    | nil _     => parser_succeed (nil ())
    | pa :: pas => 
        pa \parcc_bind (lam a => 
            (parcc_seqs pas) \parcc_bind (lam bs => 
                parser_succeed (a :: bs)))

implement {a} parcc_alt (pa, pb) = 
    (parcc_maybe pa) \parcc_bind (lam a => 
        case+ a of 
        | Just a => parser_succeed a 
        | Nothing _ => pb)

implement {a} parcc_alt3 (pa, pb, pc) = 
    (parcc_maybe pa) \parcc_bind (lam a => 
        case+ a of 
        | Just a => parser_succeed a
        | Nothing _ => parcc_alt (pb, pc))

implement {a} parcc_alts (pas) = 
    case+ pas of 
    | nil _     => parser_fail ()
    | pa :: pas => 
        (parcc_maybe pa) \parcc_bind (lam a => 
            case+ a of 
            | Just a    => parser_succeed a
            | Nothing _ => parcc_alts pas)

implement {a} parcc_sat (p, f) = 
    p \parcc_bind (lam a => 
        if f a then parser_succeed a else parser_fail ())

implement {a} parcc_skip (p) = 
    p \parcc_bind (lam a => let val v = () in parser_succeed v end)

implement {a,b} parcc_map (p, f) =
    p \parcc_bind (lam a => parser_succeed (f a))

implement {a} parcc_rpt0 (p) = 
    (parcc_maybe p) \parcc_bind (lam a => 
        case+ a of 
        | Just a    => (parcc_rpt0 p) \parcc_bind (lam bs => parser_succeed (a :: bs))
        | Nothing _ => parser_succeed (nil ()))

implement {a} parcc_rpt1 (p) = 
    (parcc_seq (p, parcc_rpt0 p)) \parcc_bind (lam a => parser_succeed (a.0 :: a.1))

implement {a} parcc_rptn (p, n) = 
    if n = 0 
    then parser_succeed (nil ())
    else (parcc_seq (p, parcc_rptn (p, n-1))) \parcc_bind (lam a => parser_succeed (a.0 :: a.1))

implement {a,b} parcc_sepby (a, sep) = 
    parcc_seq (a, parcc_rpt0 (parcc_seq (sep, a) \parcc_map (lam x => x.1))) 
        \parcc_map (lam x => ListCons (x.0, x.1))

implement {a,b} parcc_leadby (lead, pb) = 
    parcc_seq (lead, pb) \parcc_map (lam x => x.1)

implement {a,b} parcc_followedby (pa, follow) = 
    parcc_seq (pa, follow) \parcc_map (lam x => x.0)

implement {a,b,c} parcc_between (pa, pb, pc) =
    (pa \parcc_leadby pb) \parcc_followedby pc

implement {o} memo0 (f) = let 
    val p = f ()
in 
    lam () => p 
end 

implement {i,o} memo1 (f) = let 
    staload "libats/ML/SATS/funmap.sats"
    staload _ = "libats/ML/DATS/funmap.dats"    
    staload _ = "libats/DATS/funmap_avltree.dats"

    val table = ref<map(i,o)> (funmap_nil ())
in 
    lam a => 
        case+ funmap_search (!table, a) of 
        | ~Some_vt p => p
        | ~None_vt _ => let 
            val p = f a 
            var t = !table
            val- ~None_vt _ = funmap_insert (t, a, p)
            val _ = !table := t
        in 
            p
        end 
end

implement {i1,i2,o} memo2 (f) = let 
    staload "libats/ML/SATS/funmap.sats"
    staload _  = "libats/ML/DATS/funmap.dats"    
    staload _  = "libats/DATS/funmap_avltree.dats"

    val table = ref<map(@(i1,i2),o)> (funmap_nil ())
in 
    lam (a, b) => 
        case+ funmap_search (!table, @(a, b)) of 
        | ~Some_vt p => p
        | ~None_vt _ => let 
            val p = f (a, b)
            var t = !table
            val- ~None_vt _ = funmap_insert (t, @(a, b), p)
            val _ = !table := t
        in 
            p
        end 
end








////
implement {a} memo_parser (p) = let 
    staload "libats/ML/SATS/funmap.sats"
    staload _  = "libats/ML/DATS/funmap.dats"    
    staload _  = "libats/DATS/funmap_avltree.dats"

    #define :: Cons

    typedef entry_t = @(a, input_t, list (cont_t a))
    val table = ref<map(input_t, ref entry_t)> (funmap_nil ())
in
    lam (input, cont) =<cloref1> 
        case+ funmap_search (!table, input) of 
        | ~Some_vt entry => 
            let 
                val _ = !entry.2 := cont :: !entry.2
            in 
                cont (result, input)
            end 
        | ~None_vt _ => 
            let 
                val entry = ref<entry_t> ()
                var t = !table 
                val- ~None_vt _ = funmap_insert (t, input, entry)
                val _ = !table := t
            in 
                p (input, lam (result, input) => let 
                    val :=)
end


//assume input_t ...

implement {} parser_char () = 
    parser_encode (lam (input, cont) => cont (..., ...))


implement {} parser_digit () = 
    parser_encode (lam (input, cont) => parser_char () \)
implement {} parser_digits () = 
    (parser_digit ())
    \parcc_alt 
    (parser_digits () \parcc_seq parser_digit ())
////

assume input_t = lazy ($sm.stream char)

typedef entry_t = [o,a:t@ype] @{ks = list (cont (o, a)), rs = list (result o)}
//assume trampoline = ref (@{stack = list (pair (parser o, input_t)), table = map (input_t, entry)})

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

implement {o} succeed (ret) = mk (lam (input, k) => k (Success (ret, input)))
implement {o} fail    ()    = mk (lam (input, k) => k (Failure (input)))

implement {o,a}   apply (p, input, k)  = (unmk p) (input, k)
implement {o1,o2} bind  (p, f) = mk (lam (input, k) => 
    apply (p, input, lam result => 
        case+ result of 
        | Success (v, rest) => apply ((f v), rest, k)
        | Failure (rest) => k (Failure (rest)))
)

implement {o1,o2} seq (a, b) = a \bind (lam x => b \bind (lam y => succeed (Pair (x, y))))
implement {o}     alt (a, b) = mk (lam (input, k) => let 
    val _ = apply (a, input, k)
    val _ = apply (b, input, k)
in 
end
)



implement memo_cps (f) = let 
    val ref = ref<map(input_t, entry)>(map_make ())
    extern fun eq 
    fun table_ref (input) = 
        case+ map_lookup (input, ref) of 
        | Just (entry) => entry 
        | None () => let val _ = insert (input, entry, ref) in entry end
in
    lam (input, k) => 


////
implement {i} {o} alt {e1,e2} (a, b) = 
    mk (lam input => 
            case+ apply (a, input) of 
            | Success (ret, rest) => Success (ret, rest)
            | Failure _ => apply (b, input))


//implement {i} {o} alts (ps) = list_foldr (ps, fail (), lam (a, b) => a \alt b)
//implement {i} {o}     seqs (ps)  = force (list_foldr (ps, succeed (Nil ()), lam (a, b) => a \bind (lam x => b \bind (lam y => succeed (x :: y)))))

implement {i} {o1,o2} seq  {e1,e2} (a, b) = a \bind (lam x => b \bind (lam y => succeed (Pair (x, y))))
implement {i} {o,o1}  seqr {e1,e2} (p, r) = red (seq (skip p, r), lam x => snd x)
implement {i} {o,o1}  seql {e1,e2} (l, p) = red (seq (l, skip p), lam x => fst x)

implement {i} {o} sat {e} (p, f) = mk p where {
    val p = lam input =<cloref1> 
            (case+ apply (p, input) of 
            | Failure _ => Failure input 
            | Success (ret, rest) =>
                if f ret
                then Success (ret, rest) 
                else Failure (input)): result (i, o)
}


implement {i} {o} opt {e} (p) = mk p where {
    val p = lam input =<cloref1> 
            (case+ apply (p, input) of 
            | Success (ret, rest) => Success (Just ret, rest)
            | Failure (input) => Success (Nothing (), input)): result (i, maybe o)
}

implement {i} {o} rpt1 (p)        = p \bind (lam x => ((rpt1 p) \red (lam y => x :: y)) \alt succeed (x :: Nil ()))
implement {i} {o} rpt0 (p)        = (rpt1 p) \alt (succeed (Nil ()))
implement {i} {o} rptn {n} (p, n) = if n = 0 then succeed (Nil ()) else p \bind (lam x => (rptn (p, n-1)) \bind (lam y => succeed (x :: y)))

//implement {i} {o1,o2} rptuntil {e} (p, e) = let 
//    val trye = 
//        lam input =<cloref1> 
//            (case+ apply (e, input) of 
//            | Success (_, _) => Success (Nil (), input)
//            | Failure (_) => Failure (input)): result (i, list o1)
//in 
//    alt (mk trye, (p \seq rptuntil (p, e)) \red (lam x => fst x :: snd x))
//end


implement {i} {o} skip {e} (p)                             = p \bind (lam x => succeed (Unit ()))
implement {i} {o1,o2} sepby1 (*{e1,e2}*) {e} (p, sep)              = (p \seq (rpt0 (sep \seqr p))) \red (lam x => fst x :: snd x)
implement {i} {o1,o2} sepby0 (*{e1,e2}*) {e} (p, sep)              = (p \sepby1 sep) \alt (succeed (Nil ()))
implement {i} {o,o1,o2} between {e1,e2} (p, open, close) = open \bind (lam _ => p \bind (lam x => close \bind (lam _ => succeed x)))

implement {i} {o} not {e} (p) = mk p where {
    val p = lam input =<cloref1>
            (case+ apply (p, input) of 
            | Success (_, _) => Failure input 
            | Failure _ => Success (Unit (), input)): result (i, unit)
}


//implement {i} {o} mk (p) = p

implement {i} {o} force {e} (p)     = mk (lam input => apply (!p, input))
implement {i,o} {r} red {e} (p, f)  = p \bind (lam x => succeed (f x))





////


(*
 *  parsers
 *)



implement anychar () = mk p where {
    val p = lam (input: lazy ($sm.stream char)) =<cloref1>
            (case+ !input of 
            | $sm.Nil () => Failure (input)
            | $sm.Cons (x, rest) => Success (x, rest)): result (lazy ($sm.stream char), char)
}



implement spaces ()     = skip (rpt0 (litchar ' '))
implement space ()      = litchar ' '
implement spacetab ()   = space () \alt (tab ())
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
    (case1 \alt case2) \alt case3
end

implement litstring (match) = let
    fun genpar (index: int):<cloref1> parser unit =
        if index = len (match) - 1
        then skip (litchar match[index])
        else skip (seq (skip (litchar match[index]), genpar (index+1)))
in 
    genpar (0) \red (lam _ => match)
end

implement alphas ()     = (rpt1 (alpha ()))       \red (lam x => string_unexplode x)
implement digits ()     = (rpt1 (digit ()))       \red (lam x => string_unexplode x)
implement alphadigits() = (rpt1 (alphadigit ()))  \red (lam x => string_unexplode x)
implement hexdigits ()  = (rpt1 (hexdigit ()))    \red (lam x => string_unexplode x)
implement octdigits ()  = (rpt1 (octdigit ()))    \red (lam x => string_unexplode x)
implement uppercases () = (rpt1 (uppercase ()))   \red (lam x => string_unexplode x)
implement lowercases () = (rpt1 (lowercase ()))   \red (lam x => string_unexplode x)
implement symbols ()    = (rpt1 (symbol ()))      \red (lam x => string_unexplode x)

implement eof () = mk p where {
    val p = lam (input: lazy ($sm.stream char)) =<cloref1>
        case+ !input of 
        | $sm.Nil _ => Success (Unit (), input)
        | _ => Failure (input)
}



implement id () = let
    val case1 = (alpha() \alt (litchar '_')) \red (lam x => string_from_char x)
    val case2 = (rpt1 (alphadigit() \alt (litchar '_'))) \red (lam x => string_unexplode x)
in 
    ((case1 \seq case2) \red (lam x => string_concat (fst x, snd x))) \alt case1
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




(*







*)
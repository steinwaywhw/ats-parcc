#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "parcc.sats"
staload _ = "parcc.dats"

staload "util/util.sats"
staload "util/list.sats"
staload "util/string.sats"
staload "util/pair.sats"
staload sm = "util/stream.sats"

staload _ = "util/list.dats"
staload _ = "util/stream.dats"
staload _ = "util/string.dats"
staload _ = "util/pair.dats"

staload "untyped/parser.sats"

#define :: Cons

infixl +>
infixl <+
infixl <+>
infixl <|>

overload +> with seqr 
overload <+ with seql
overload <+> with seq 
overload <|> with alt 

implement show_syntax (s) = 
	case+ s of 
	| TmVar x => $extfcall (void, "printf", "var(%s)", x)
	| TmApp (x, y) => () where {
		val _ = show "("
		val _ = show_syntax x 
		val _ = show ")"
		val _ = show "("
	 	val _ = show_syntax y 
	 	val _ = show ")"
	}
	| TmLam (x, y) => () where {
		val _ = $extfcall (void, "printf", "lam(%s)=>", x)
		val _ = show_syntax y
	}

//implement parser_lam () = $delay (let 
//	val _ = show "lam\n"

//	val l = skip (litstring "lam" <+> ws ())
//	val b = between (ws () +> id () <+ ws (), litstring "(", litstring ")")
//	val o = between (skip (litstring "=>"), ws(), ws())
//in 
	
//		red (l +> b <+ o <+> force (parser ()), lam x => TmLam (fst x, snd x))
//end
//)

//implement parser_var () = 
//	(id () \red (lam x => TmVar x))
//	where {
//		val _ = show "var\n"
//	}


//implement parser_app () = 
//	$delay (
//		red (
//			force (parser ()) <+ ws () <+> between (force (parser ()), litstring "(", litstring ")"),
//			lam x => TmApp (fst x, snd x))
//	)
//	where {
//		val _ = show "app\n"
//	}



implement parser () = let 
	val pvar = id () \red (lam x => TmVar x)
	val plam = $delay (litstring "lam" +> ws () +> (between (id(), litstring "(", litstring ")")) <+ ws () <+ litstring "=>" <+ ws () <+> force (parser ()))
	val xplam = $delay (red (force plam, lam x => TmLam (fst x, snd x)))
	val papp = $delay (force (parser ()) <+ ws () <+> between (force (parser ()), litstring "(", litstring ")"))
	val xpapp = $delay (red (force papp, lam x => TmApp (fst x, snd x)))
in $delay (
	pvar <|> force xplam <|> force xpapp
) end 




//implement parser () = 
//	$delay (
//		parser_var () <|> force (parser_lam ()) <|> force (parser_app ()) <|> between (force (parser ()), litstring "(", litstring ")")
//	)
//	where {
//		val _ = show "term\n"
//	}


staload "libc/SATS/stdio.sats"

implement main0 () = () where {
	fun file_get_stream (path: string): lazy ($sm.stream char) = sm where {
		fun tostream (file: FILEref): lazy ($sm.stream char) = let
			val c = int2char0 (fgetc file)
		in 
			if feof file = 0
			then $delay $sm.Cons (c, tostream file)
			else $delay $sm.Nil () where {
				val _ = fclose_exn file
			} 
		end

		val file = fopen_ref_exn (path, file_mode_r)
		val sm = tostream file
	}

//	val p = sepby1 (parser (), ws ()): parser (list syntax)
	val s = file_get_stream ("untyped/1.test")
//	val p = parser ()
//	val test = litstring "lam" <+ ws () <+ litstring "(" <+ ws () <+> id () <+ ws () <+ litstring ")" 
//	val _ = show_result_string (apply (red (test, lam x => string_concat (fst x, snd x)), s))
//	val _ = show_result (apply (parser_lam(), s), lam x => show_syntax x)
	val _ = show_result (apply (sepby1 (force (parser ()), ws ()), s), lam x => list_foreach (x, lam x => show_syntax x))
}
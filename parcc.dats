#define ATS_DYNLOADFLAG 0

#include "share/atspre_staload.hats"

staload "parcc.sats"
staload "util/util.sats"
staload "util/pair.sats"
staload "util/list.sats"
staload "util/unit.sats"
staload "util/maybe.sats"

staload _ = "util/list.dats"
staload _ = "util/pair.dats"

//staload "util/string.sats"
//staload "lexing/token.sats"
//staload sm = "util/stream.sats"
//staload "file/location.sats"
//staload _ = "util/stream.dats"
//staload _ = "file/location.dats"


#define :: Cons


implement {i} {o} print_result (r, f) =
	case+ r of 
	| Success (o, _) => f o 
	| Failure _      => show "fail"

implement {i} print_result_int (r)    = print_result (r, lam x => show x)
implement {i} print_result_string (r) = print_result (r, lam x => show x)
implement {i} print_result_char (r)   = print_result (r, lam x => show x)
implement {i} print_result_double (r) = print_result (r, lam x => show x)
implement {i} print_result_bool (r)   = print_result (r, lam x => show x)

//
// pargen
//
implement {i} {o} succeed (ret) = 
	lam input => Success (ret, input)

implement {i} {o} fail () = 
	lam input => Failure (input)

//
// parcom
//
implement {i} {o} alt (a, b) = 
	lam (input) => 
		case+ apply (a, input) of 
		| Success (ret, input) => Success (ret, input)
		| Failure _ => apply (b, input)

implement {i} {o} alts (ps) = 
	lam input =>
		case+ ps of 
		| Nil () => Failure (input)
		| p :: Nil () => apply (p, input)
		| p :: ps => 
			case+ apply (p, input) of 
			| Success (ret, input) => Success (ret, input)
			| Failure _ => apply (alts ps, input)

implement {i} {o1,o2} seq (a, b) = 
	bind (a, 
		lam (x) => 
			bind (b, lam (y) => succeed (Pair (x, y))))

implement {i} {o} seqs (ps) = 
	case+ ps of 
	| Nil () => fail ()
	| p :: Nil () => bind (p, lam x => succeed (x :: Nil))
	| p :: ps => bind (p, lam x => bind (seqs ps, lam y => succeed (x :: y)))

implement {i} {o} sat (p, f) = 
	lam (input) => 
		case+ apply (p, input) of 
		| Failure _ => Failure (input) 
		| Success (ret, rest) =>
			if f (ret)
			then Success (ret, rest) 
			else Failure (input)

implement {i} {o} opt (p) =
	lam (input) => 
		case+ apply (p, input) of 
		| Success (ret, rest) => Success (Just (ret), rest)
		| Failure (input) => Success (Nothing (), input)

implement {i} {o} rpt1 (p) = 
	red (
		seq (p, rpt0 p), 
		lam x => fst x :: snd x)

implement {i} {o} rpt0 (p) =
	lam (input) =>
		case+ apply (p, input) of 
		| Failure (input) => Success (Nil (), input)
		| Success (ret, rest) => 
			case+ apply (rpt0 p, rest) of 
			| Success (xs, rest) => Success (ret :: xs, rest)
			| Failure (rest) => Success (ret :: Nil (), rest)


implement {i} {o} rptn (p, n) = 
	if n <= 0
	then succeed (Nil ())
	else bind (p, 
		lam (x) =>
			bind (rptn (p, n-1), lam (y) => succeed (x :: y)))

implement {i} {o} apply (p, s) = p (s)

implement {i} {o1,o2} bind (p, f) = 
	lam (input) => 
		case+ apply (p, input) of 
			| Success (ret, input) => apply (f (ret), input)
			| Failure (input)      => Failure (input)

implement {i, o} {r} red (p, f) = bind (p, lam x => succeed (f x))

implement {i} {o} skip (p) = bind (p, lam x => succeed (Unit ()))


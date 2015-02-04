#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"

staload "libc/SATS/math.sats"
staload _ = "libc/DATS/math.dats"
staload UN = "prelude/SATS/unsafe.sats"

staload "util/util.sats"
staload "util/list.sats"
staload "util/string.sats"

staload _ = "util/list.dats"

#define :: Cons


implement string_unexplode (xs) = let
	val length = len (xs) + 1
	val g1 = g1ofg0 length
	val _ = assertloc (g1 > 0)

	val (view, gc | ptr) = malloc_gc (i2sz g1)

	fun loop (xs: list char, p: ptr): void = 
		case+ xs of
		| x :: xs => loop (xs, ptr_succ<char>(p)) where { val _ = $UN.ptr0_set<char>(p, x) }
		| Nil () => $UN.ptr0_set<char>(p, $UN.cast{char}(0))

	val _ = loop (xs, ptr)
in 
	$UN.castvwtp0{string}((view, gc | ptr))
end




implement string_explode (str) = let 
	val len = $extfcall (int, "strlen", str)
	fun loop (index: int, ret: list (char)): list (char) =
		if index >= len
		then ret 
		else loop (index + 1, str[index] :: ret)
in 
	list_reverse (loop (0, Nil ()))
end

implement string_empty (s) = s = ""

implement string_from_char (c) = string_unexplode (c :: Nil())

implement string_from_int (n) = let 
	fun loop (n: int, s: string): string = 
		if n >= 10
		then loop (n / 10, string_prepend (s, '0' + (n mod 10)))
		else string_prepend (s, '0' + n) 
in 
	if n > 0 then loop (n, "") else string_prepend (loop (~n, ""), '-')
end

implement string_to_int (s) = 
	if s[0] = '-' 
	then ~ (string_to_int (string_range (s, 1, string_len(s)-1)))
	else let 
		val len = string_len s
		fun loop (index: int, r: int): int = 
			if index < len 
			then loop (index + 1, (s[index] - '0') + 10 * r)
			else r 
	in 
		loop (0, 0)
	end

implement string_to_double (s) = 
	if s[0] = '-' 
	then ~ string_to_double (string_range (s, 1, string_len (s) - 1))
	else let 
		val pos = string_find (s, ".")
		val a = string_range (s, 0, pos-1)
		val b = string_range (s, pos+1, string_len (s) - 1)
		val aint = string_to_int a 
		val bint = string_to_int b 
	in 
		(* need to link with option -lm *)
		aint * 1.0 + bint * 1.0 * pow (10.0, ~ string_len (b) * 1.0)
	end

implement string_join (xs, sep) = 
	case+ xs of 
	| x :: Nil () => x
	| x :: xs => string_concat (string_concat (x, sep), string_join (xs, sep))
	| Nil () => ""

implement string_split (s, sep) = let 
	val len = string_len s
	val lensep = string_len sep 
	fun loop (s: string, ls: list string): list string = let 
		val pos = string_find (s, sep)
	in 	
		if pos >= 0
		then loop (string_range (s, pos + lensep, len - 1), string_range (s, 0, pos - 1) :: ls)
		else ls 
	end 
in 
	list_reverse<string>(loop (s, Nil ()))
end

implement string_concat (a, b) = 
	string_unexplode (list_concat (string_explode a, string_explode b))

implement string_append (s, c) = string_concat (s, string_from_char c)
implement string_prepend (s, c) = string_concat (string_from_char (c), s)

implement string_range (s, b, e) = 
	if b <= e 
	then string_unexplode (list_take (list_drop (string_explode s, b), e - b + 1))
	else string_range (s, e, b)

implement string_compare (a, b) = $extfcall (int, "strcmp", a, b)
implement string_eq (a, b) = string_compare (a, b) = 0
implement string_len (str) = $extfcall (int, "strlen", str)



%{#

int string_get (char *str, int pos) {
	if (pos < strlen(str)) {
		return str[pos];
	} else {
		return 0;
	}
}

int string_find (char *str, char *sep) {
	char *p = strstr (str, sep);
	if (p == NULL)
		return -1;
	else 
		return p - str;
}

%}

////

dynload "util/list.dats"

implement main0 () = () where {
	val sep = "\n----------------\n" : string
	val _ = show (string_from_char('C'))
	val _ = show sep
	val _ = show (string_from_int(~12345222))
	val _ = show sep
	val _ = show (string_to_int("-1234562222"))
	val _ = show sep 
	val _ = show (string_to_double("-123.456") = ~123.456)
	val _ = show sep
	val _ = show (string_explode "Abcdefg")
	val _ = show sep
	val _ = show (string_unexplode (string_explode "Abcdefg") = "Abcdefg")
	val _ = show sep
	val _ = show (string_find ("abcdefgsssasdsssa", "sss"))
	val _ = show sep 
	val _ = show (string_concat("abcde", "12345"))
	val _ = show sep
	val _ = show (string_join ("aaa" :: "bbb" :: "ccc" :: Nil(), "XX"))
	val _ = show sep
	val _ = foreach (string_split ("aaaXXXbbbXXcccXXX", "XX"), lam x => show x where { val _ = print_newline ()})
	val _ = show sep 
	val _ = show (string_append ("abc", 'C'))
	val _ = show sep 
	val _ = show (string_prepend ("abc", 'C'))
	val _ = show sep 
	val _ = show (string_range ("abcde", 1, 4))
	val _ = show sep 
	val _ = show (string_range ("abcde", ~1, 9))
	val _ = show sep 
	val _ = show (string_range ("abcde", 2, 1))
	val _ = show sep 
	val _ = show (compare ("abcde", "abcde"))
	val _ = show sep 
	val _ = show (compare ("abc", "ABC"))
	val _ = show sep 
	val _ = show (eq ("ab", "ab"))
	val _ = show sep 
//	val _ = show 
//	val _ = show 

}
#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "libc/SATS/math.sats"
staload _ = "libc/DATS/math.dats"

staload UN = "prelude/SATS/unsafe.sats"

staload "./util.sats"
staload "./list.sats"
staload "./string.sats"

staload _ = "./list.dats"

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

implement string_to_int_unsigned (s) = let 
	fun loop (s: string, r: int): int = 
		if empty s 
		then r 
		else loop (tail s, (head(s) - '0') + 10 * r)
in 
	loop (string_trim s, 0)
end

implement string_to_int (s) = let 
	val sign = head (string_trim s) 
in 
	case+ sign of 
	| _ when sign = '-' => ~ (string_to_int_unsigned (tail (string_trim s)))
	| _ when sign = '+' => string_to_int_unsigned (tail (string_trim s))
	| _ 				=> string_to_int_unsigned (s)
end

implement string_to_double_unsigned (s) = let 
	val s = string_trim s 
	val pos = string_find (s, ".")
	val a = string_range (s, 0, pos-1)
	val b = string_range (s, pos+1, len (s)-1)
	val aint = string_to_int_unsigned a
	val bint = string_to_int_unsigned b
in 
	aint*1.0 + bint*1.0*pow(10.0, ~(len(b)*1.0))
end

implement string_to_double (s) = let 
	val sign = head (string_trim s)
in 
	case+ sign of 
	| _ when sign = '-' => ~ (string_to_double_unsigned (tail (string_trim s)))
	| _ when sign = '+' => string_to_double_unsigned (tail (string_trim s))
	| _ 				=> string_to_double_unsigned (s)
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
	else ""

implement string_compare (a, b) = $extfcall (int, "strcmp", a, b)
implement string_eq (a, b) = string_compare (a, b) = 0
implement string_len (str) = $extfcall (int, "strlen", str)

implement string_head (str) = if empty str then '\0' else str[0]
implement string_tail (str) = if empty str then "" else string_range (str, 1, string_len (str) - 1)

implement string_trim (str) = let 
	fun loop1 (str: string): string = 
		if empty str
		then str 
		else 
			if isspace (head str) 
			then string_trim (tail str) 
			else str 
	fun loop2 (str: string): string = 
		if empty str 
		then str 
		else 
			if isspace (str[len (str) - 1])
			then string_trim (string_range (str, 0, len (str) - 2))
			else str 
in 
	loop2 (loop1 str)
end 

%{

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


implement main0 () = () where {
	val sep = "\n----------------\n" : string
	val _ = show (string_from_char('C'))
	val _ = show sep
	val _ = show (string_from_int(~12345222))
	val _ = show sep
	val _ = show (string_to_int(" -1234562222 "))
	val _ = show sep 
	val _ = show (string_to_double("  -123.456 \n") = ~123.456)
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
#include "share/atspre_staload.hats"

staload "util/util.sats"
staload "util/list.sats"
staload "string/string.sats"
staload UN = "prelude/SATS/unsafe.sats"

#define :: Cons

implement string_unexplode (xs) = let
	val length = len (xs) + 1
	val g1 = g1ofg0 (length)
	val _ = assertloc (g1 > 0)

	val (view, gc | ptr) = malloc_gc (i2sz (g1))

	fun loop (xs: list char, p: ptr): void = 
		case+ xs of
		| x :: xs => loop (xs, ptr_succ (p)) where { val _ = $UN.ptr0_set<char>(p, x) }
		| Nil () => $UN.ptr0_set<char>(p, $UN.cast{char}(0))

	val _ = loop (xs, ptr)
in 
	$UN.castvwtp0{string}((view, gc | ptr))
end

////

implement string_explode (str) = let 
	val len = $extfcall (int, "strlen", str)
	fun loop (index: int, ret: list (char)): list (char) =
		if index >= len
		then ret 
		else loop (index + 1, str[index] :: ret)
in 
	loop (0, Nil ())
end



implement string_from_char (c) = string_unexplode (c :: Nil())

implement string_from_int (n) = let 
	fun loop (n: int, s: string): string = 
		if n >= 10
		then loop (n / 10, string_prepend (s, '0' + (n mod 10)))
		else s 
in 
	loop (n, "")
end

implement string_to_int (s) = let 
	val len = string_len s
	fun loop (index: int, r: int): int = 
		if index < len 
		then loop (index + 1, (s[index] - '0') + 10 * r)
		else r 
in 
	loop (0, 0)
end

implement string_to_double (s) = let 
	val len = string_len s

	fun loop (i: int, r: double): double = 
		if i < len && s[i] != '.'
		then loop (i+1, r * 10 + (s[i] - '0'))
		else r + loop (i+1, 0.0) / len - i
in 
	loop (0, 0.0)
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
	list_reverse (loop (s, Nil ()))
end

(*implement string_concat (a, b) = let 
	val lena = g1ofg0 (string_len a)
	val lenb = g1ofg0 (string_len b)
	val len = lena + lenb + 1
	val _ = assertloc (len > 0)

	val (view, gc | ptr) = malloc_gc (i2sz len)

	fun loop (index: int, p: ptr): void = 
		if index < lena
		then loop (index + 1, ptr_succ p) where { val _ = $UN.ptr0_set<char>(p, a[index]) }
		else 
			if index - lena < lenb
			then loop (index + 1, ptr_succ p) where { val _ = $UN.ptr0_set<char>(p, b[index - lena]) }
			else $UN.ptr0_set<char>(p, $UN.cast{char}(0))

	val _ = loop (0, ptr)
in 
	$UN.castvwtp0{string}((view, gc | ptr))
end*)

implement string_append (s, c) = string_concat (s, string_from_char c)
implement string_prepend (s, c) = string_concat (string_from_char (c), s)

(*implement string_range (s, b, e) = 
	if b = e then ""
	else if b > e then string_range (s, e, b) 
	else if b < 0 then string_range (s, 0, e)
	else if e >= string_len s then string_range (s, b, string_len (s) - 1)
	else let 
		val len = g1ofg0 (e - b)
		val _ = assertloc (len >= 0)

		val (view, gc | ptr) = malloc_gc (i2sz len)

		fun loop (index: int, p: ptr): void = 
			if index <= e
			then loop (index + 1, ptr_succ p) where { val _ = $UN.ptr0_set<char>(p, s[index]) }
			else $UN.ptr0_set<char>(p, $UN.cast{char}(0))

		val _ = loop (b, ptr)
in 
	$UN.castvwtp0{string}((view, gc | ptr))
end*)

implement string_compare (a, b) = $extfcall (int, "strcmp", a, b)
implement string_eq (a, b) = string_compare (a, b) = 1
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


implement main0 () = {} where {
	val a = string_from_char('c')
	val _ = assertloc (a = "c")
}
staload "util/list.sats"
staload "string/string.sats"
#include "share/atspre_staload.hats"

#define :: Cons

implement string_explode (str) = let 
	val len = $extfcall (int, "strlen", str)
	fun loop (index: int, ret: list (char)): list (char) =
		if index >= len
		then ret 
		else loop (index + 1, str[index] :: ret)
in 
	loop (0, Nil ())
end

%{#
int string_get (char *str, int pos) {
	if (pos < strlen(str)) {
		return str[pos];
	} else {
		return 0;
	}
}
%}

implement string_len (str) = x where {
	val x = $extfcall (int, "strlen", str)
}
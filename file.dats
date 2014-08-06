#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "file.sats"
staload "libc/SATS/stdio.sats"
staload sm = "stream.sats"
staload "location.sats"
staload "pair.sats"

staload _ = "stream.dats"
staload _ = "location.dats"


implement filestream (path) = sm where {
	fun tostream (file: FILEref): lazy ($sm.stream char) = let
		val c = int2char0 (fgetc file)
	in 
		if feof (file) = 0
		then $delay ($sm.Cons (c, tostream file))
		else $delay ($sm.Nil ()) where {
			val _ = fclose_exn (file)
		} 
	end

	val file = fopen_ref_exn (path, file_mode_r)
	val sm = tostream file
}

implement append_position (xs, line, col) = 
	case+ !xs of 
	| $sm.Nil _ => $delay $sm.Nil()
	| $sm.Cons (x, xs) =>
		if x = '\n'
		then $delay ($sm.Cons (Pair (x, Pos (line, col)), append_position (xs, line + 1, 1)))
		then $delay ($sm.Cons (Pair (x, Pos (line, col)), append_position (xs, line, col + 1)))

implement current_position (xs) = 
	case+ !xs of 
	| $sm.Nil _ => 
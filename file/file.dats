#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

staload "libc/SATS/stdio.sats"

staload "util/util.sats"
staload sm = "util/stream.sats"

staload "file/file.sats"
staload "file/location.sats"

staload "util/pair.sats"

staload _ = "util/stream.dats"
staload _ = "file/location.dats"


implement file_get_stream (path) = sm where {
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

implement file_append_position (xs, p) = 
	case+ !xs of 
	| $sm.Nil _        => $delay $sm.Nil()
	| $sm.Cons (x, xs) => $delay $sm.Cons (Pair (x, p), file_append_position (xs, position_next (p, x)))

implement file_append_location (xs, file) = 
	$sm.map (xs, lam (x) => v where {
			val Pair (ch, pos) = x 
			val v = Pair (ch, Loc (file, Range (pos, pos)))
		})


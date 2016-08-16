#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
#include "atsparcc.hats"
#include "atsutils.hats"

staload "stream.sats"
staload "unit.sats"

staload "sexp.sats"
staload _ = "sexp.dats"

extern fun file_get_stream (string): stream char 
implement file_get_stream (path) = sm where {
	staload "libc/SATS/stdio.sats"

	fun tostream (file: FILEref): stream char = let
		val c = int2char0 (fgetc file)
	in 
		if feof file = 0
		then $delay StreamCons (c, tostream file)
		else $delay StreamNil () where {
			val _ = fclose_exn file
		} 
	end

	val file = fopen_ref_exn (path, file_mode_r)
	val sm = tostream file
}

implement main0 () = {
  	assume output_t = unit 

	val input = file_get_stream ("./test1.sexp")
	val input = $UNSAFE.cast{input_t} input

	val _ = parser_apply (parser_sexp_file (), input, lam (x, input) => unparse_sexp x)
}


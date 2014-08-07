#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0
staload "location.sats"

typedef position_t = '{line=int, col=int}
assume position = position_t

typedef range_t = '{a=position_t, b=position_t}
assume range = range_t

typedef location_t = '{file=string, r=range_t}
assume location = location_t

implement Pos (line, col) = '{line = line, col = col}
implement position_line (p) = p.line
implement position_col (p) = p.col
implement position_next (current, ch) = 
	if ch = '\n'
	then '{line = current.line + 1, col = 1}
	else '{line = current.line, col = current.col + 1}

implement position_origin = '{line = 1, col = 1}
implement position_eof = '{line = ~1, col = ~1}

implement Range (a, b) = '{a=a, b=b} 
implement range_begin (range) = range.a
implement range_end (range) = range.b

local 
	fun min (x:position, y:position): position = if x < y then x else y
	fun max (x:position, y:position): position = if x > y then x else y
in
	implement range_merge (x, y) = '{a=min(x.a, y.a), b=max(x.b, y.b)}
end

implement Loc (file, r) = '{file=file, r=r} 
implement location_file (loc) = loc.file
implement location_range (loc) = loc.r

(***************
	Utilities
 ***************)

implement fprint_location (out, loc) = () where {
	val _ = fprint (out, loc.file)
	val _ = fprint (out, " [")
	val _ = fprint (out, loc.r)
	val _ = fprint (out, "]")
}

implement fprint_position (out, pos) = $extfcall (void, "fprintf", out, "%d:%d", pos.line, pos.col)

implement fprint_range (out, r) = () where {
	val _ = fprint (out, r.a)
	val _ = fprint (out, " - ")
	val _ = fprint (out, r.b)
}

implement eq_pos_pos (a, b)			 = compare (a, b) = 0	
implement eq_range_range (a, b)		 = a.a = b.a && a.b = b.b	
implement eq_loc_loc (a, b)			 = a.file = b.file && a.r = b.r

implement neq_pos_pos (a, b)		 = ~(a = b)
implement neq_range_range (a, b)	 = ~(a = b)
implement neq_loc_loc (a, b)		 = ~(a = b)

implement gt_pos_pos (a, b)			 = compare (a, b) > 0	
implement lt_pos_pos (a, b)			 = compare (a, b) < 0	
implement gte_pos_pos (a, b)		 = compare (a, b) >= 0	
implement lte_pos_pos (a, b)		 = compare (a, b) <= 0	

implement compare_pos_pos (a, b)	 = if a.line = b.line then a.col - b.col else a.line - b.line	

////

implement fprint_position (out, pos) = 
	case+ pos of Pos (line, col) => $extfcall (void, "fprintf", out, "%d:%d", line, col)

implement fprint_location (out, loc) = 
	case+ loc of
	| None ()  				 => fprint (out, "none")
	| Stdin () 			 	 => fprint (out, "stdin")
	| FilePos (path, a) 	 => () where {
		val _ = $extfcall (void, "fprintf", out, "%s [", path)
		val _ = fprint (out, a)
		val _ = fprint (out, "]")
	}
	| FileRange (path, a, b) => () where {
		val _ = $extfcall (void, "fprintf", out, "%s [", path)
		val _ = fprint (out, a)
		val _ = fprint (out, " - ")
		val _ = fprint (out, b)
		val _ = fprint (out, "]")
	}

implement compare_pos_pos (p1, p2) = let 
	val Pos (l1, c1) = p1
	val Pos (l2, c2) = p2
in 
	if l1 != l2
	then l1 - l2
	else p1 - p2
end

implement isnone (x:location): bool = 
	case+ x of 
	| None _ => true
	| _ => false
implement isstdin (x:location): bool = 
	case+ x of 
	| Stdin _ => true
	| _ => false
implement ispos (x:location): bool = 
	case+ x of 
	| FilePos _ => true
	| _ => false
implement isrange (x:location): bool = 
	case+ x of 
	| FileRange _ => true
	| _ => false

implement eq_loc_loc (x, y) = 
	case+ x of
	| None _ => isnone y
	| Stdin _ => isstdin y 
	| FilePos _ => ispos y
	| FileRange _ => isrange y

end (* LOCAL IN END*)

implement merge (x, y) = 
	
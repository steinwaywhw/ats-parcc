abstype position = ptr
abstype range = ptr
abstype location = ptr

fun Pos (line: int, col: int): position 
fun position_next (current: position, ch: char): position 
fun position_line (position): int 
fun position_col (position): int 

val position_origin : position 
val position_eof : position 

fun Range (a: position, b: position): range 
fun range_begin (range): position 
fun range_end (range): position 
fun range_merge (range, range): range

fun Loc (string, range): location 
fun location_file (location): string
fun location_range (location): range


(***************
	Utilities
 ***************)

fun fprint_location (out: FILEref, loc: location): void
fun fprint_position (out: FILEref, pos: position): void
fun fprint_range (out: FILEref, r: range): void
overload fprint with fprint_range
overload fprint with fprint_location
overload fprint with fprint_position 

fun eq_pos_pos (position, position): bool
fun eq_range_range (range, range): bool
fun eq_loc_loc (location, location): bool

fun neq_pos_pos (position, position): bool
fun neq_range_range (range, range): bool
fun neq_loc_loc (location, location): bool

fun gt_pos_pos (position, position): bool
fun lt_pos_pos (position, position): bool
fun gte_pos_pos (position, position): bool
fun lte_pos_pos (position, position): bool

fun compare_pos_pos (position, position): int

overload compare with compare_pos_pos

overload = with eq_pos_pos
overload = with eq_range_range
overload = with eq_loc_loc

overload != with neq_pos_pos
overload != with neq_range_range
overload != with neq_loc_loc

overload > with gt_pos_pos
overload < with lt_pos_pos
overload >= with gte_pos_pos
overload <= with lte_pos_pos

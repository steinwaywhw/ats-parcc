staload "list.sats"
staload "pair.sats"

datatype position = 
	| Pos of (int, int)
	| Eof of ()

datatype location =
	| None of ()
	| Stdin of ()
	| FilePos of (string, position)
	| FileRange of (string, position, position)

fun fprint_location (out: FILEref, loc: location): void
fun fprint_position (out: FILEref, pos: position): void
overload fprint with fprint_location
overload fprint with fprint_position

fun compare_pos_pos (position, position): int
overload compare with compare_pos_pos

fun merge (location, location): location 

fun eq_loc_loc (location, location): location 
overload eq with eq_loc_loc

fun isstdin (location): bool
fun isnone (location): bool
fun ispos (location): bool 
fun isrange (location): bool
fun getpath (location): string
fun getposition (location): position 
fun getrange (location): pair (position, position)

datatype position = Pos of (int, int)

datatype location =
	| None of ()
	| Stdin of ()
	| FilePos of (string, position)
	| FileRange of (string, position, position)

fun fprint_location (out: FILEref, loc: location): void
fun fprint_position (out: FILEref, pos: position): void
overload fprint with fprint_location
overload fprint with fprint_position

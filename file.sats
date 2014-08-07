staload sm = "stream.sats"
staload "pair.sats"
staload "location.sats"

typedef fs_char_t = lazy ($sm.stream char) 
typedef fs_pos_t = lazy ($sm.stream (pair (char, position)))
typedef fs_loc_t = lazy ($sm.stream (pair (char, location)))

fun filestream (path: string): fs_char_t
fun append_position (fs_char_t, origin: position): fs_pos_t
fun append_location (fs_pos_t, file: string): fs_loc_t
fun current_position (fs_loc_t): position 



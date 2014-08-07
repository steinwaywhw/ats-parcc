staload "file/location.sats"

staload sm = "util/stream.sats"
staload "util/pair.sats"

typedef fs_char_t = lazy ($sm.stream char) 
typedef fs_pos_t = lazy ($sm.stream (pair (char, position)))
typedef fs_loc_t = lazy ($sm.stream (pair (char, location)))


// public interface
fun file_get_stream (path: string): fs_char_t
fun file_append_position (fs_char_t, origin: position): fs_pos_t
fun file_append_location (fs_pos_t, file: string): fs_loc_t




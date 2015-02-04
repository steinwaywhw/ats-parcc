staload "util/util.sats"
staload "util/stream.sats"
staload "util/pair.sats"
staload "file/location.sats"

typedef fs_char_t = lazy (stream char) 
typedef fs_pos_t = lazy (stream (pair (char, position)))
typedef fs_loc_t = lazy (stream (pair (char, location)))

// public interface
fun file_get_stream (path: string): fs_char_t
fun file_append_position (fs_char_t, origin: position): fs_pos_t
fun file_append_location (fs_pos_t, file: string): fs_loc_t




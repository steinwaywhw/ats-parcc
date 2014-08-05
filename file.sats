staload sm = "stream.sats"
staload "pair.sats"
staload "location.sats"

fun filestream (path: string): lazy ($sm.stream (pair (char, location)))
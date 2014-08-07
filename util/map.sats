staload "util/maybe.sats"

abstype map (k:t@ype, v:t@ype)
typedef map (k:t@ype) = [v:t@ype] map (k, v)
typedef map = [k,v:t@ype] map (k, v)

fun insert {k:t@ype} {v:t@ype} (map (k, v), k, v): map (k, v)
fun member {k:t@ype} 		   (map (k), k): bool
fun lookup {k:t@ype} {v:t@ype} (map (k, v), k): maybe (v)
fun update {k:t@ype} {v:t@ype} (map (k, v), k, v): map (k, v)
fun delete {k:t@ype} 		   (map (k), k): map (k)
fun empty (map): bool
fun size (map): int
staload "util/maybe.sats"

datatype stream (a:t@ype) =
	| Cons of (a, lazy (stream a))
	| Nil of ()

fun {a:t@ype} stream_empty (lazy (stream a)): bool
fun {a:t@ype} stream_append (lazy (stream a), a): lazy (stream a)
fun {a:t@ype} stream_concat (lazy (stream a), lazy (stream a)): lazy (stream a)
fun {a,b:t@ype} {r:t@ype} stream_zip (lazy (stream a), lazy (stream b), (a, b) -> r): lazy (stream r)
fun {a:t@ype} stream_foreach (lazy (stream a), a -> void): void


fun {a:t@ype} stream_head (lazy (stream a)): maybe a
fun {a:t@ype} stream_take (lazy (stream a), int): lazy (stream a)
fun {a:t@ype} stream_tail (lazy (stream a)): lazy (stream a)
fun {a:t@ype} stream_drop (lazy (stream a), int): lazy (stream a)
fun {a,b:t@ype} {r:t@ype} stream_zip (lazy (stream a), lazy (stream b), (a, b) -<cloref1> r): lazy (stream r)
fun {a:t@ype} stream_filter (lazy (stream a), a -<cloref1> bool): lazy (stream a)
fun {a:t@ype} {b:t@ype} stream_map (lazy (stream a), a -<cloref1> b): lazy (stream b)
fun {a:t@ype} {b:t@ype} stream_foldr (lazy (stream a), b, (a, b) -<cloref1> b): b 
fun {a:t@ype} {b:t@ype} stream_foldl (lazy (stream a), b, (a, b) -<cloref1> b): b 

fun {a:t@ype} stream_interleave (lazy (stream a), lazy (stream a)): lazy (stream a)
fun {a:t@ype} stream_merge (lazy (stream a), lazy (stream a), (a, a) -<cloref1> int): lazy (stream a)
fun {a:t@ype} stream_get (lazy (stream a), int): maybe a

fun {a:t@ype} fprint_stream (out: FILEref, s: lazy (stream a), len: int, f: (FILEref, a) -> void): void
overload fprint with fprint_stream

overload empty 	 with stream_empty	     
overload append	 with stream_append      	
overload head 	 with stream_head	     
overload tail 	 with stream_tail	     
overload take 	 with stream_take	     
overload drop 	 with stream_drop	     
overload concat	 with stream_concat      	
overload map  	 with stream_map 	     
overload filter	 with stream_filter      	
overload foldl 	 with stream_foldl	     
overload foldr 	 with stream_foldr	     
overload zip 	 with stream_zip	     
overload foreach with stream_foreach	      	
overload [] 	 with stream_get
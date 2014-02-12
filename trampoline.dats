staload "parcc.sats"
staload "trampoline.sats"



(* ***** LOCAL in end ***** *) 
local
(* ***** LOCAL in end ***** *) 

var g_stackref : ref (stack (call_node (k, v)))
var g_tableref : ref (map (k, map (v, o)))

//val g_stack = stack_nil ()
//val g_table = map_nil ()

(* ***** local IN end ***** *) 
in 
(* ***** local IN end ***** *) 

implement set_call_stack {k,v} (t, s) = t where () = g_stackref := s
implement get_call_stack {k,v} (t) = g_stackref
implement set_memo_table {k,v} (t, m) = t where () = g_tableref := m
implement get_memo_table {k,v} (t) = g_tableref

implement has_next (t) = not (empty (!get_call_stack (t)))

implement push {k,v} (t, key, value) = 
	insert (s, key, value) 
	where s = get_call_stack (t)
	// tbd


implement step {o} (t) = 
	if has_next (t) then 
		just (apply (node.f, node.args)) where {
			val s = get_call_stack (t)
			val node = head (!s)
			val () = !s := tail (!s)
		}
	else
		nothing ()

implement run {o} (t) = let
	fun loop (t:tramp (o), rs: list (o)): list (o) = 
		if has_next (t) then
			loop (t, append (rs, step (t)))


(* ***** local in END ***** *) 
end
(* ***** local in END ***** *)
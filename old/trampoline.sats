

abstype list (a:t@ype) = ptr






//typedef c0_t (o:t@ype) = () -<cloref1> o 
//typedef c1_t (i1:t@ype, o:t@ype) = i -<cloref1> o
//typedef c2_t (i1:t@ype, i2:t2ype, o:t@ype) = (i1, i2) -<cloref1> o 

typedef callentry_t = @{p = parser a, args = input_t}
typedef callstack_t = list callentry_t
typedef memoresult_t = @{ks = list (input_t -<cloref1> output_t), os = list output_t}
typedef memoentry_t = @{i = input_t, r = ref memoresult_t}
typedef memotable_t = list ($rec{name = string, fn = () -<cloref1> output_t, memo = list memoentry_t})

typedef trampoline = $rec{s = ref callstack_t, m = ref memotable_t}













////

fun new_trampoline (): trampoline
fun free_trampoline (trampoline): void


fun has_next (trampoline): bool




fun push_stack (stack, f, args) = {f, args} :: stack 
fun step (stack) = let 
	{f, args} :: s = stack 
	 stack := s 
	 f(args)

fun push (f, str, cont) = let 


abstype entry (k:t@ype, o:t@ype) = ptr 
fun {k,o:t@ype} entry_make (): entry (k, o)
fun {k,o:t@ype} entry_put_cont (entry (k, o), k): entry (k, o)
fun {k,o:t@ype} entry_put_result (entry (k, o), o): entry (k, o)
datatype _entry (k:t@ype, o:t@ype) = Entry (k, o) of (list k (*continuation*), list o (*output*))
assume entry (k:t@ype, o:t@ype) = _entry (k, o)

abstype memo (i:t@ype, k:t@ype, o:t@ype) = ptr 
fun {i,k,o:t@ype} memo_add_entry (memo (i, k, o), i, entry (k, o)): memo (i, k, o)
datatype _memo (i:t@ype, k:t@ype, o:t@ype) = Memo (i, k, o) of (i (*input*), _entry (k, o))
assume memo (i, k, o) = _memo (i, k, o)

abstype table (f:t@ype, i:t@ype, k:t@ype, o:t@ype) = ptr 
assume table (f:t@ype, i:t@ype, k:t@ype, o:t@ype) = list ()


(define/public (push fn str cont)
      (define head mcar)
      (define tail mcdr)
      (define (push-continuation! entry cont)
      	let x :: xs = entry 
      	entry := {cont :: x} :: xs 
      (define (push-result! entry result)
      	let x :: xs = entry 
      	entry := x :: {result :: xs}
      (define (result-subsumed? entry result)
        (mmember result (tail entry)))
      (define (make-entry)
        (mcons (mlist) (mlist)))

      // table = (fn, memo) :: (fn, memo) :: ...
      // memo = (str, entry) :: (str, entry) ::
      (define (table-ref fn str)
        (let ((pair (massoc fn table)))
          (match pair
            [(mcons fn memo)
             (match (massoc str memo)
               ;; parser has been called with str before
               [(mcons str entry) entry]
               ;; first time parser has been called with str
               [_ (let ((entry (make-entry)))
                    (set-mcdr! pair (mcons (mcons str entry) memo))
                    entry)])]
            ;; first time parser has been called
            [_ (let* ((entry (make-entry))
                      (memo (mlist (mcons str entry))))
                 (set! table (mcons (mcons fn memo) table))
                 entry)])))

      	case+ lookup (table, f) of 
      	| (fn, memo) => 
      		case+ lookup(memo, input) of 
      		| (input, entry) => entry 
      		| (_) => (fn, (str, new entry) :: memo)

      	| _ => let 
      		entry = pair (nil, nil) = (cont :: cont :: ..., output :: output :: ...)
      		memo  = list of pair (str, entry) = (input, (nil, nil)) :: (input, (nil, nil))
      		table := (f, memo) :: table = (f, (input, (nil, nil)) :: (input, (nil, nil)) ....)




////
staload "parcc.sats"
//
// k: key type, should be the parser
// v: value type, shoud be the stream (the input of the parser)
// o: output type, should be the result (the output of the parser)
//
abstype tramp (k:t@ype, v:t@ype, o:t@ype)

typedef tramp (o:t@ype) = [k,v:t@ype] tramp (k, v, o)
typedef tramp (k:t@ype, v:t@ype) = [o:t@ype] tramp (k, v, o)
typedef tramp = [k,v,o:t@ype] tramp (k, v, o)

fun has_next 		 (tramp): bool
fun push {k,v:t@ype} (tramp (k, v), k, v): void // stateful
fun step {o:t@ype}   (tramp (o)): maybe (o)	 	// stateful
fun run  {o:t@ype}   (tramp (o)): list (o)		// stateful

typedef call_node (k:t@ype, v:t@ype): @{f = k, args = v}

//
// These are stateful
//
fun set_call_stack {k,v:t@ype} (tramp (k, v), ref (stack (call_node (k, v)))): tramp (k, v)
fun get_call_stack {k,v:t@ype} (tramp (k, v)): ref (stack (call_node (k, v)))
fun set_memo_table {k,v:t@ype} {o:t@ype} (tramp (k, v, o), ref (map (k, map (v, o)))): tramp (k, v, o)
fun get_memo_table {k,v:t@ype} {o:t@ype} (tramp (k, v, o)): ref (map (k, map (v, o)))
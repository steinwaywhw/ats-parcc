

abstype parser (i:t@ype, o:t@ype)

datatype result (i:t@ype, o:t@ype) = 
	| Success of (o, i)
	| Failure of (i)
	
assume parser (i, o) = i -<cloref1> result (i, o)

extern fun succeed {i:t@ype} {o:t@ype} (o): parser (i, o)

implement succeed {i} {o} (ret) = 
	lam (input: i): result (i, o) =<cloref1> Success (ret, input)

overload || with succeed
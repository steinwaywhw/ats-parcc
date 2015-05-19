



datatype syntree = 
	| SynTerminal
	| SynExp

datatype synexpr = 
	| SEId
	| SEValue
	| SE

datatype synvalue = 
	| SVString 
	| SVInt
	| SVDouble
	| SVList
	| SVMap 
	| SVVoid

datatype synpattern = 
	| SPId of ()
	| SPAny of ()
	| SPVoid of ()
	| SPList of (list synpattern)
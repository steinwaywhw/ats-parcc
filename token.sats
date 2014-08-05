staload "util.sats"
staload "location.sats"

datatype token = 
	| TNone of ()
	| TEof of ()
	| TComment of (string, location)
	| TSpace of (location)
	| TChar of (char, location)
	| TString of (string, location)
	| TId of (string, location)
	| TInt of (int, location)


fun fprint_token (out: FILEref, t: token): void
overload fprint with fprint_token
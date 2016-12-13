staload "parcc.sats"
staload "list.sats"
staload "unit.sats"

datatype sexp = 
| SList of list sexp 
| SSymbol of string 
| SInt of int 
| SBool of bool 
| SString of string 

fun parser_sexp_symbol (): parser sexp 
fun parser_sexp_bool (): parser sexp 
fun parser_sexp_string (): parser sexp 
fun parser_sexp_int (): parser sexp 

fun parser_sexp_atom (): parser sexp 
fun parser_sexp_sexp (): parser sexp 
fun parser_sexp_sexplist (): parser sexp
fun parser_sexp_file (): parser sexp 


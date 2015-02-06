staload "util/util.sats"
staload "file/location.sats"
staload "util/list.sats"

//abstype token = ptr




   


datatype token = 
//main
    | TComment  of string
    | TSpace    of ()
    | TId       of string

//misc
    | TLParen   of () // (
    | TRParen   of () // )
    | TLCurly   of () // {
    | TRCurly   of () // }
    | TLBrac    of () // [
    | TRBrac    of () // ]
    | TPAny     of () // _
    | TColon    of () // :

//keyword
    | TLambda   of ()
    | TCase     of ()
    | TIf       of ()
    | TLet      of () 
    | TVal      of ()

//literal
    | TString   of string 
    | TChar     of char 
    | TInt      of int 
    | TDouble   of double
    | TBool     of bool 

//operator
    | TOp       of string

fun print_token (token): void 
fun fprint_token (token, FILEref): void

overload show with print_token 
overload show with fprint_token 



(*
  top-level-form         = general-top-level-form
                         | (#%expression expr)
                         | (module id module-path (#%plain-module-begin module-level-form ...))
                         | (begin top-level-form ...)
                         | (begin-for-syntax top-level-form ...)
                 
  module-level-form      = general-top-level-form
                         | (#%provide raw-provide-spec ...)
                         | (begin-for-syntax module-level-form ...)
                         | submodule-form
                         | (#%declare declaration-keyword ...)
                 
  submodule-form         = (module id module-path (#%plain-module-begin module-level-form ...))
                         | (module* id module-path (#%plain-module-begin module-level-form ...))
                         | (module* id #f (#%plain-module-begin module-level-form ...))
                 
  general-top-level-form = expr
                         | (val (id+) expr)

                         | (define-syntaxes (id ...) expr)
                         | (#%require raw-require-spec ...)
                 
  expr                   = id
                         | value
                         | (expr)
                         | (expr expr+)
                         | (lambda pattern expr)
                         | (case expr (pattern expr)+)
                         | (if expr expr expr?)
                         | (let (pattern expr)+ expr)

  value                  = string
                         | numeral
                         | [value*]
                         | [(value:value)*]
                         | ()

                         | (begin expr ...+)
                         | (begin0 expr expr ...)
                         | (let-values ([(id ...) expr] ...) expr ...+)
                         | (letrec-values ([(id ...) expr] ...) expr ...+)
                         | (set! id expr)
                         | (quote datum)
                         | (quote-syntax datum)
                         | (with-continuation-mark expr expr expr)
                         | (#%plain-app expr ...+)
                         | (#%top . id)
                         | (#%variable-reference id)
                         | (#%variable-reference (#%top . id))
                         | (#%variable-reference)

  pattern                = id
                         | (pattern* )
                         | _

  formals                = (id ...)
                         | (id ...+ . id)
                         | id

*)






////
fun token_get_file (token): string
fun token_get_range (token): range
fun token_get_location (token): location
fun token_get_node (token): tokennode
fun token_make (tokennode, location): token

fun fprint_token (out: FILEref, t: token): void
fun fprint_tokennode (out: FILEref, t: tokennode): void
fun fprint_token_list (out: FILEref, ts: list (token), int): void
overload fprint with fprint_token
overload fprint with fprint_tokennode
overload fprint with fprint_token_list 



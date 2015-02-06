#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0
staload "libc/SATS/stdio.sats"

staload "util/util.sats"
staload "lexing/token.sats"
staload "file/location.sats"
staload "util/maybe.sats"
staload "util/list.sats"


staload _ = "util/list.dats"
staload _ = "file/location.dats"
staload _ = "util/maybe.dats"

implement fprint_token (t, out) =
    case+ t of 
    | TComment s   => $extfcall (void, "fprintf", out, "TComment (%s)", s)
    | TSpace   ()  => $extfcall (void, "fprintf", out, "TSpace")
    | TId      id  => $extfcall (void, "fprintf", out, "TId (%s)", id)

    | TLParen  _   => $extfcall (void, "fprintf", out, "TLParen")
    | TRParen  _   => $extfcall (void, "fprintf", out, "TRParen")
    | TLCurly  _   => $extfcall (void, "fprintf", out, "TLCurly")
    | TRCurly  _   => $extfcall (void, "fprintf", out, "TRCurly")
    | TLBrac   _   => $extfcall (void, "fprintf", out, "TLBrac")
    | TRBrac   _   => $extfcall (void, "fprintf", out, "TRBrac")
    | TPAny    _   => $extfcall (void, "fprintf", out, "TPAny")
    | TColon   _   => $extfcall (void, "fprintf", out, "TColon")

    | TLambda  _   => $extfcall (void, "fprintf", out, "TLambda")
    | TCase    _   => $extfcall (void, "fprintf", out, "TCase")
    | TIf      _   => $extfcall (void, "fprintf", out, "TIf")
    | TLet     _   => $extfcall (void, "fprintf", out, "TLet")
    | TVal     _   => $extfcall (void, "fprintf", out, "TVal")

    | TChar    c   => $extfcall (void, "fprintf", out, "TChar (%c)", c)
    | TString  s   => $extfcall (void, "fprintf", out, "TString (%s)", s)
    | TInt     i   => $extfcall (void, "fprintf", out, "TInt (%d)", i)
    | TDouble  d   => $extfcall (void, "fprintf", out, "TDouble (%f)", d)
    | TBool    b   => $extfcall (void, "fprintf", out, "TBool (%d)", b)

    | TOp      opr => $extfcall (void, "fprintf", out, "TOp (%s)", opr)

implement print_token (t) = fprint_token (t, stdout_ref)

//assume token = '{node = tokennode, loc = location}

//implement fprint_token (out, t) = () where {
//  val _ = fprint (out, t.node)
//  val _ = fprint (out, " at ")
//  val _ = fprint (out, t.loc)
//}



//implement token_get_file (t) = location_file t.loc
//implement token_get_range (t) = location_range t.loc
//implement token_get_location (t) = t.loc
//implement token_get_node (t) = t.node
//implement token_make (t, loc) = '{node = t, loc = loc}

//implement fprint_token_list (out, ts, len) = 
//  if empty (ts) || len = 0
//  then fprint (out, "nil")
//  else fprint_token_list (out, tail ts, len - 1) where {
//      val _ = fprint_token (out, maybe_unjust (head ts))
//      val _ = fprint (out, '\n')
//  }
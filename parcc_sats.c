/*
**
** The C code is generated by ATS/Postiats
** The compilation time is: 2014-8-5:  2h:32m
**
*/

/*
** include runtime header files
*/
#ifndef _ATS_CCOMP_HEADER_NONE
#include "pats_ccomp_config.h"
#include "pats_ccomp_basics.h"
#include "pats_ccomp_typedefs.h"
#include "pats_ccomp_instrset.h"
#include "pats_ccomp_memalloc.h"
#ifndef _ATS_CCOMP_EXCEPTION_NONE
#include "pats_ccomp_memalloca.h"
#include "pats_ccomp_exception.h"
#endif // end of [_ATS_CCOMP_EXCEPTION_NONE]
#endif /* _ATS_CCOMP_HEADER_NONE */


/*
** include prelude cats files
*/
#ifndef _ATS_CCOMP_PRELUDE_NONE
//
#include "prelude/CATS/basics.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/pointer.cats"
#include "prelude/CATS/bool.cats"
#include "prelude/CATS/char.cats"
#include "prelude/CATS/integer_ptr.cats"
#include "prelude/CATS/integer_fixed.cats"
#include "prelude/CATS/float.cats"
#include "prelude/CATS/memory.cats"
#include "prelude/CATS/string.cats"
#include "prelude/CATS/strptr.cats"
//
#include "prelude/CATS/filebas.cats"
//
#include "prelude/CATS/list.cats"
#include "prelude/CATS/option.cats"
#include "prelude/CATS/array.cats"
#include "prelude/CATS/arrayptr.cats"
#include "prelude/CATS/arrayref.cats"
#include "prelude/CATS/matrix.cats"
#include "prelude/CATS/matrixptr.cats"
//
#endif /* _ATS_CCOMP_PRELUDE_NONE */
/*
** for user-supplied prelude
*/
#ifdef _ATS_CCOMP_PRELUDE_USER
//
#include _ATS_CCOMP_PRELUDE_USER
//
#endif /* _ATS_CCOMP_PRELUDE_USER */

/*
staload-prologues(beg)
*/
/*
/home/ubuntu/workspace/prelude.sats: 1(line=1, offs=1) -- 23(line=1, offs=23)
*/
/*
/home/ubuntu/workspace/prelude.sats: 24(line=2, offs=1) -- 47(line=2, offs=24)
*/
/*
/home/ubuntu/workspace/string.sats: 1(line=1, offs=1) -- 27(line=1, offs=27)
*/
/*
/home/ubuntu/workspace/ats-sds/sds.sats: 1(line=1, offs=1) -- 28(line=3, offs=3)
*/

#include "sds/sds.h"
/*
/home/ubuntu/workspace/prelude.sats: 48(line=3, offs=1) -- 69(line=3, offs=22)
*/
/*
/home/ubuntu/workspace/prelude.sats: 70(line=4, offs=1) -- 90(line=4, offs=21)
*/
/*
/home/ubuntu/workspace/map.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/home/ubuntu/workspace/prelude.sats: 91(line=5, offs=1) -- 112(line=5, offs=22)
*/
/*
/home/ubuntu/workspace/prelude.sats: 113(line=6, offs=1) -- 135(line=6, offs=23)
*/
/*
/home/ubuntu/workspace/prelude.sats: 136(line=7, offs=1) -- 157(line=7, offs=22)
*/
/*
/home/ubuntu/workspace/prelude.sats: 158(line=8, offs=1) -- 179(line=8, offs=22)
*/
/*
staload-prologues(end)
*/
/*
typedefs-for-tyrecs-and-tysums(beg)
*/
/*
typedefs-for-tyrecs-and-tysums(end)
*/
/*
dynconlst-declaration(beg)
*/
/*
dynconlst-declaration(end)
*/
/*
dyncstlst-declaration(beg)
*/
/*
dyncstlst-declaration(end)
*/
/*
dynvalist-implementation(beg)
*/
/*
dynvalist-implementation(end)
*/
/*
exnconlst-declaration(beg)
*/
#ifndef _ATS_CCOMP_EXCEPTION_NONE
extern void the_atsexncon_initize (atstype_exncon *d2c, char *exnmsg) ;
#endif // end of [_ATS_CCOMP_EXCEPTION_NONE]
/*
exnconlst-declaration(end)
*/
/*
assumelst-declaration(beg)
*/
/*
assumelst-declaration(end)
*/
/*
extypelst-declaration(beg)
*/
/*
extypelst-declaration(end)
*/
/*
** for initialization(dynloading)
*/
atsvoid_t0ype
_057_home_057_ubuntu_057_workspace_057_parcc_056_sats__dynload()
{
ATSdynload1(
_057_home_057_ubuntu_057_workspace_057_parcc_056_sats__dynloadflag
) ;
ATSif(
ATSCKiseqz(
_057_home_057_ubuntu_057_workspace_057_parcc_056_sats__dynloadflag
)
) ATSthen() {
ATSdynloadset(_057_home_057_ubuntu_057_workspace_057_parcc_056_sats__dynloadflag) ;
/*
dynexnlst-initize(beg)
*/
/*
dynexnlst-initize(end)
*/
} /* ATSendif */
ATSreturn_void() ;
} /* end of [*_dynload] */

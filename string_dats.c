/*
**
** The C code is generated by ATS/Postiats
** The compilation time is: 2014-8-6: 19h: 5m
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
/vagrant/string.sats: 1(line=1, offs=1) -- 20(line=1, offs=20)
*/
/*
/ats2/prelude/DATS/pointer.dats: 1533(line=44, offs=1) -- 1572(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/integer.dats: 1636(line=51, offs=1) -- 1675(line=51, offs=40)
*/
/*
/ats2/prelude/DATS/integer_fixed.dats: 1641(line=51, offs=1) -- 1680(line=51, offs=40)
*/
/*
/ats2/prelude/DATS/memory.dats: 1410(line=38, offs=1) -- 1449(line=39, offs=32)
*/
/*
/ats2/prelude/DATS/string.dats: 1609(line=48, offs=1) -- 1648(line=48, offs=40)
*/
/*
/ats2/prelude/DATS/strptr.dats: 1609(line=48, offs=1) -- 1648(line=48, offs=40)
*/
/*
/ats2/prelude/DATS/strptr.dats: 1671(line=52, offs=1) -- 1718(line=52, offs=48)
*/
/*
/ats2/prelude/DATS/integer.dats: 1636(line=51, offs=1) -- 1675(line=51, offs=40)
*/
/*
/ats2/prelude/DATS/filebas.dats: 1613(line=48, offs=1) -- 1652(line=48, offs=40)
*/
/*
/ats2/prelude/DATS/filebas.dats: 1675(line=52, offs=1) -- 1722(line=52, offs=48)
*/
/*
/ats2/prelude/DATS/integer.dats: 1636(line=51, offs=1) -- 1675(line=51, offs=40)
*/
/*
/ats2/prelude/DATS/filebas.dats: 1745(line=56, offs=1) -- 1783(line=56, offs=39)
*/
/*
/ats2/libc/SATS/stdio.sats: 1380(line=35, offs=1) -- 1418(line=37, offs=3)
*/

#include "libc/CATS/stdio.cats"
/*
/ats2/libc/SATS/stdio.sats: 1791(line=56, offs=1) -- 1833(line=58, offs=27)
*/
/*
/ats2/libc/sys/SATS/types.sats: 1390(line=36, offs=1) -- 1432(line=38, offs=3)
*/

#include "libc/sys/CATS/types.cats"
/*
/ats2/prelude/DATS/filebas.dats: 1861(line=61, offs=1) -- 1901(line=61, offs=41)
*/
/*
/ats2/libc/sys/SATS/stat.sats: 1390(line=36, offs=1) -- 1431(line=38, offs=3)
*/

#include "libc/sys/CATS/stat.cats"
/*
/ats2/libc/sys/SATS/stat.sats: 1776(line=53, offs=1) -- 1818(line=54, offs=35)
*/
/*
/ats2/libc/sys/SATS/types.sats: 1390(line=36, offs=1) -- 1432(line=38, offs=3)
*/

#include "libc/sys/CATS/types.cats"
/*
/ats2/prelude/DATS/filebas.dats: 15159(line=816, offs=1) -- 15189(line=816, offs=31)
*/
/*
/ats2/libc/SATS/stdio.sats: 1380(line=35, offs=1) -- 1418(line=37, offs=3)
*/

#include "libc/CATS/stdio.cats"
/*
/ats2/libc/SATS/stdio.sats: 1791(line=56, offs=1) -- 1833(line=58, offs=27)
*/
/*
/ats2/libc/sys/SATS/types.sats: 1390(line=36, offs=1) -- 1432(line=38, offs=3)
*/

#include "libc/sys/CATS/types.cats"
/*
/ats2/prelude/DATS/list.dats: 1527(line=44, offs=1) -- 1566(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/list.dats: 1567(line=45, offs=1) -- 1613(line=45, offs=47)
*/
/*
/ats2/prelude/DATS/unsafe.dats: 1532(line=44, offs=1) -- 1566(line=44, offs=35)
*/
/*
/ats2/prelude/DATS/list_vt.dats: 1536(line=44, offs=1) -- 1575(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/list_vt_mergesort.dats: 1546(line=44, offs=1) -- 1585(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/list_vt_quicksort.dats: 1546(line=44, offs=1) -- 1585(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/array.dats: 1534(line=44, offs=1) -- 1573(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/array.dats: 1574(line=45, offs=1) -- 1616(line=45, offs=43)
*/
/*
/ats2/prelude/DATS/array_bsearch.dats: 1531(line=44, offs=1) -- 1570(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/array_quicksort.dats: 1531(line=44, offs=1) -- 1570(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/arrayptr.dats: 1532(line=44, offs=1) -- 1571(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/arrayref.dats: 1532(line=44, offs=1) -- 1571(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/matrix.dats: 1535(line=44, offs=1) -- 1574(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/matrixptr.dats: 1538(line=44, offs=1) -- 1577(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/matrixref.dats: 1538(line=44, offs=1) -- 1577(line=44, offs=40)
*/
/*
/ats2/prelude/DATS/stream.dats: 1570(line=48, offs=1) -- 1609(line=48, offs=40)
*/
/*
/ats2/prelude/DATS/stream_vt.dats: 1573(line=48, offs=1) -- 1612(line=48, offs=40)
*/
/*
/ats2/prelude/DATS/unsafe.dats: 1532(line=44, offs=1) -- 1566(line=44, offs=35)
*/
/*
/ats2/prelude/DATS/checkast.dats: 1531(line=44, offs=1) -- 1570(line=45, offs=32)
*/
/*
staload-prologues(end)
*/
/*
typedefs-for-tyrecs-and-tysums(beg)
*/
typedef
struct {
#if(0)
int contag ;
#endif
atstkind_t0ype(atstype_char) atslab__0; 
atstype_boxed atslab__1; 
} postiats_tysum_0 ;
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
ATSdyncst_mac(atspre_g0int2int_int_int)
ATSdyncst_mac(atspre_g0int_gte_int)
ATSdyncst_mac(atspre_g0int_add_int)
ATSdyncst_mac(string_get)
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
/vagrant/string.dats: 368(line=17, offs=1) -- 491(line=25, offs=3)
*/
int string_get (char *str, int pos) {
	if (pos < strlen(str)) {
		return str[pos];
	} else {
		return 0;
	}
}
#if(0)
ATSglobaldec()
atstype_boxed
_057_vagrant_057_string_056_sats__explode(atstkind_type(atstype_ptrk)) ;
#endif // end of [QUALIFIED]

ATSstaticdec()
atstype_boxed
loop_1(atstkind_type(atstype_ptrk), atstkind_t0ype(atstype_int), atstkind_t0ype(atstype_int), atstype_boxed) ;

#if(0)
#if(0)
ATSglobaldec()
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gte_g0int_int__2(atstkind_t0ype(atstyvar_type(tk)), atstkind_t0ype(atstype_int)) ;
#endif // end of [QUALIFIED]
#endif // end of [TEMPLATE]

ATSstaticdec()
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gte_g0int_int__2__1(atstkind_t0ype(atstype_int), atstkind_t0ype(atstype_int)) ;

/*
/vagrant/string.dats: 147(line=7, offs=19) -- 364(line=15, offs=4)
*/
/*
local: 
global: explode$0$0(level=0)
local: 
global: 
*/
ATSglobaldec()
atstype_boxed
_057_vagrant_057_string_056_sats__explode(atstkind_type(atstype_ptrk) arg0)
{
/* tmpvardeclst(beg) */
ATStmpdec(tmpret0, atstype_boxed) ;
ATStmpdec(tmp1, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp11, atstype_boxed) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/string.dats: 139(line=7, offs=11) -- 364(line=15, offs=4)
*/
ATSINSlab(__patsflab_explode):
/*
emit_instr: loc0 = /vagrant/string.dats: 155(line=7, offs=27) -- 364(line=15, offs=4)
*/
/*
letpush(beg)
*/
/*
emit_instr: loc0 = /vagrant/string.dats: 172(line=8, offs=12) -- 202(line=8, offs=42)
*/
ATSINSmove(tmp1, strlen(arg0)) ;
/*
letpush(end)
*/

/*
emit_instr: loc0 = /vagrant/string.dats: 352(line=14, offs=11) -- 358(line=14, offs=17)
*/

ATSINSmove_con0(tmp11, 0) ;

/*
emit_instr: loc0 = /vagrant/string.dats: 343(line=14, offs=2) -- 359(line=14, offs=18)
*/
ATSINSmove(tmpret0, loop_1(arg0, tmp1, ATSPMVi0nt(0), tmp11)) ;

/*
emit_instr: loc0 = /vagrant/string.dats: 155(line=7, offs=27) -- 364(line=15, offs=4)
*/
/*
INSletpop()
*/
/* funbodyinstrlst(end) */
ATSreturn(tmpret0) ;
} /* end of [_057_vagrant_057_string_056_sats__explode] */

/*
/vagrant/string.dats: 209(line=9, offs=6) -- 335(line=12, offs=43)
*/
/*
local: loop_1$0(level=1)
global: loop_1$0(level=1)
local: str$3437(1)(HSEapp(HSEcst(atstkind_type); HSEs2exp(S2Eextkind(atstype_ptrk)))), len$3438(1)(HSEapp(HSEcst(atstkind_t0ype); HSEs2exp(S2Eextkind(atstype_int))))
global: str$3437(1)(HSEapp(HSEcst(atstkind_type); HSEs2exp(S2Eextkind(atstype_ptrk)))), len$3438(1)(HSEapp(HSEcst(atstkind_t0ype); HSEs2exp(S2Eextkind(atstype_int))))
*/
ATSstaticdec()
atstype_boxed
loop_1(atstkind_type(atstype_ptrk) env0, atstkind_t0ype(atstype_int) env1, atstkind_t0ype(atstype_int) arg0, atstype_boxed arg1)
{
/* tmpvardeclst(beg) */
ATStmpdec(argx0, atstkind_t0ype(atstype_int)) ;
ATStmpdec(argx1, atstype_boxed) ;
ATStmpdec(tmpret2, atstype_boxed) ;
ATStmpdec(tmp3, atstkind_t0ype(atstype_bool)) ;
ATStmpdec(tmp8, atstkind_t0ype(atstype_int)) ;
ATStmpdec(tmp9, atstype_boxed) ;
ATStmpdec(tmp10, atstkind_t0ype(atstype_char)) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/string.dats: 209(line=9, offs=6) -- 335(line=12, offs=43)
*/
ATSINSlab(__patsflab_loop_1):
/*
emit_instr: loc0 = /vagrant/string.dats: 266(line=10, offs=6) -- 278(line=10, offs=18)
*/
ATSINSmove(tmp3, ATSLIB_056_prelude__gte_g0int_int__2__1(arg0, env1)) ;

/*
emit_instr: loc0 = /vagrant/string.dats: 263(line=10, offs=3) -- 335(line=12, offs=43)
*/
ATSif(
tmp3
) ATSthen() {
/*
emit_instr: loc0 = /vagrant/string.dats: 287(line=11, offs=8) -- 290(line=11, offs=11)
*/
ATSINSmove(tmpret2, arg1) ;
} ATSelse() {
/*
emit_instr: loc0 = /vagrant/string.dats: 306(line=12, offs=14) -- 315(line=12, offs=23)
*/
ATSINSmove(tmp8, atspre_g0int_add_int(arg0, ATSPMVi0nt(1))) ;

/*
emit_instr: loc0 = /vagrant/string.dats: 317(line=12, offs=25) -- 327(line=12, offs=35)
*/
ATSINSmove(tmp10, string_get(env0, arg0)) ;

/*
emit_instr: loc0 = /vagrant/string.dats: 317(line=12, offs=25) -- 334(line=12, offs=42)
*/

ATSINSmove_con1(tmp9, postiats_tysum_0) ;
#if(0)
ATSINSstore_con_tag(tmp9, 0) ;
#endif
ATSINSstore_con_ofs(tmp9, postiats_tysum_0, atslab__0, tmp10) ;
ATSINSstore_con_ofs(tmp9, postiats_tysum_0, atslab__1, arg1) ;

/*
emit_instr: loc0 = /vagrant/string.dats: 300(line=12, offs=8) -- 335(line=12, offs=43)
*/
ATStailcalbeg()
ATSINSmove_tlcal(argx0, tmp8) ;
ATSINSmove_tlcal(argx1, tmp9) ;
ATSINSargmove_tlcal(arg0, argx0) ;
ATSINSargmove_tlcal(arg1, argx1) ;
ATSgoto(__patsflab_loop_1) ;
ATStailcalend()

} /* ATSendif */
/* funbodyinstrlst(end) */
ATSreturn(tmpret2) ;
} /* end of [loop_1] */

#if(0)
/*
/ats2/prelude/DATS/integer.dats: 29032(line=800, offs=15) -- 29073(line=800, offs=56)
*/
/*
local: 
global: gte_g0int_int$2$0(level=0)
local: 
global: 
*/
ATSglobaldec()
/*
imparg = tk(4439)
tmparg = S2Evar(tk(4439))
tmpsub = None()
*/
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gte_g0int_int__2(atstkind_t0ype(atstyvar_type(tk)) arg0, atstkind_t0ype(atstype_int) arg1)
{
/* tmpvardeclst(beg) */
ATStmpdec(tmpret4, atstkind_t0ype(atstype_bool)) ;
ATStmpdec(tmp5, atstkind_t0ype(atstyvar_type(tk))) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 29018(line=800, offs=1) -- 29073(line=800, offs=56)
*/
ATSINSlab(__patsflab_gte_g0int_int):
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 29059(line=800, offs=42) -- 29071(line=800, offs=54)
*/
ATSINSmove(tmp5, PMVtmpltcst(g0int2int<S2Eextkind(atstype_int), S2Evar(tk(4439))>)(arg1)) ;

/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 29041(line=800, offs=24) -- 29073(line=800, offs=56)
*/
ATSINSmove(tmpret4, PMVtmpltcst(g0int_gte<S2Evar(tk(4439))>)(arg0, tmp5)) ;

/* funbodyinstrlst(end) */
ATSreturn(tmpret4) ;
} /* end of [ATSLIB_056_prelude__gte_g0int_int__2] */
#endif // end of [TEMPLATE]

/*
/ats2/prelude/DATS/integer.dats: 29032(line=800, offs=15) -- 29073(line=800, offs=56)
*/
/*
local: 
global: gte_g0int_int$2$1(level=2)
local: 
global: 
*/
ATSstaticdec()
/*
imparg = tk(4439)
tmparg = S2Evar(tk(4439))
tmpsub = Some(tk(4439) -> S2Eextkind(atstype_int))
*/
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gte_g0int_int__2__1(atstkind_t0ype(atstype_int) arg0, atstkind_t0ype(atstype_int) arg1)
{
/* tmpvardeclst(beg) */
ATStmpdec(tmpret4__1, atstkind_t0ype(atstype_bool)) ;
ATStmpdec(tmp5__1, atstkind_t0ype(atstype_int)) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 29018(line=800, offs=1) -- 29073(line=800, offs=56)
*/
ATSINSlab(__patsflab_gte_g0int_int):
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 29059(line=800, offs=42) -- 29071(line=800, offs=54)
*/
ATSINSmove(tmp5__1, atspre_g0int2int_int_int(arg1)) ;

/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 29041(line=800, offs=24) -- 29073(line=800, offs=56)
*/
ATSINSmove(tmpret4__1, atspre_g0int_gte_int(arg0, tmp5__1)) ;

/* funbodyinstrlst(end) */
ATSreturn(tmpret4__1) ;
} /* end of [ATSLIB_056_prelude__gte_g0int_int__2__1] */

#if(0)
/*
** for initialization(dynloading)
*/
atsvoid_t0ype
_057_vagrant_057_string_056_dats__dynload()
{
ATSdynload0(
_057_vagrant_057_string_056_dats__dynloadflag
) ;
ATSif(
ATSCKiseqz(
_057_vagrant_057_string_056_dats__dynloadflag
)
) ATSthen() {
ATSdynloadset(_057_vagrant_057_string_056_dats__dynloadflag) ;
/*
dynexnlst-initize(beg)
*/
/*
dynexnlst-initize(end)
*/
} /* ATSendif */
ATSreturn_void() ;
} /* end of [*_dynload] */
#endif // end of [#if(0)]
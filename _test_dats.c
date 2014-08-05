/*
**
** The C code is generated by ATS/Postiats
** The compilation time is: 2014-8-5: 19h:36m
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
/vagrant/file.sats: 1(line=1, offs=1) -- 27(line=1, offs=27)
*/
/*
/vagrant/stream.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/vagrant/file.sats: 29(line=2, offs=1) -- 48(line=2, offs=20)
*/
/*
/vagrant/file.sats: 50(line=3, offs=1) -- 73(line=3, offs=24)
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
/vagrant/stream.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/vagrant/file.dats: 68(line=4, offs=1) -- 87(line=4, offs=20)
*/
/*
/vagrant/file.sats: 1(line=1, offs=1) -- 27(line=1, offs=27)
*/
/*
/vagrant/stream.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/vagrant/file.sats: 29(line=2, offs=1) -- 48(line=2, offs=20)
*/
/*
/vagrant/file.sats: 50(line=3, offs=1) -- 73(line=3, offs=24)
*/
/*
/vagrant/file.dats: 89(line=5, offs=1) -- 119(line=5, offs=31)
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
/vagrant/file.dats: 121(line=6, offs=1) -- 147(line=6, offs=27)
*/
/*
/vagrant/stream.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/vagrant/file.dats: 149(line=7, offs=1) -- 172(line=7, offs=24)
*/
/*
/vagrant/file.dats: 174(line=8, offs=1) -- 193(line=8, offs=20)
*/
/*
/vagrant/file.dats: 197(line=10, offs=1) -- 222(line=10, offs=26)
*/
/*
/vagrant/stream.dats: 30(line=3, offs=1) -- 51(line=3, offs=22)
*/
/*
/vagrant/stream.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/vagrant/stream.dats: 53(line=4, offs=1) -- 73(line=4, offs=21)
*/
/*
/vagrant/file.dats: 224(line=11, offs=1) -- 251(line=11, offs=28)
*/
/*
/vagrant/location.dats: 30(line=3, offs=1) -- 53(line=3, offs=24)
*/
/*
/vagrant/stream.dats: 30(line=3, offs=1) -- 51(line=3, offs=22)
*/
/*
/vagrant/stream.sats: 1(line=1, offs=1) -- 21(line=1, offs=21)
*/
/*
/vagrant/stream.dats: 53(line=4, offs=1) -- 73(line=4, offs=21)
*/
/*
/vagrant/location.dats: 30(line=3, offs=1) -- 53(line=3, offs=24)
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
typedef
struct {
#if(0)
int contag ;
#endif
atstyvar_type(a) atslab__0; 
atstkind_type(atstype_ptrk) atslab__1; 
} postiats_tysum_1 ;
typedef
struct {
#if(0)
int contag ;
#endif
atstype_boxed atslab__0; 
atstkind_type(atstype_ptrk) atslab__1; 
} postiats_tysum_2 ;
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
ATSdyncst_extfun(_057_vagrant_057_file_056_sats__filestream, (atstkind_type(atstype_ptrk)), atstkind_type(atstype_ptrk)) ;
ATSdyncst_mac(atspre_fprint_char)
ATSdyncst_mac(atspre_fprint_string)
ATSdyncst_extfun(_057_vagrant_057_location_056_sats__fprint_location, (atstkind_type(atstype_ptrk), atstype_boxed), atsvoid_t0ype) ;
ATSdyncst_mac(atspre_g0int2int_int_int)
ATSdyncst_mac(atspre_g0int_gt_int)
ATSdyncst_mac(atspre_g0int_sub_int)
ATSdyncst_mac(atspre_FILE_stdout)
ATSdyncst_extfun(_057_vagrant_057__test_056_dats__test, (atstkind_type(atstype_ptrk)), atsvoid_t0ype) ;
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
#if(0)
ATSglobaldec()
atsvoid_t0ype
_057_vagrant_057__test_056_dats__test(atstkind_type(atstype_ptrk)) ;
#endif // end of [QUALIFIED]

ATSstaticdec()
atsvoid_t0ype
p_1(atstkind_type(atstype_ptrk), atstype_boxed) ;

#if(0)
#if(0)
ATSglobaldec()
atsvoid_t0ype
_057_vagrant_057_stream_056_sats__fprint_stream__2(atstkind_type(atstype_ptrk), atstkind_type(atstype_ptrk), atstkind_t0ype(atstype_int), atstype_funptr) ;
#endif // end of [QUALIFIED]
#endif // end of [TEMPLATE]

ATSstaticdec()
atsvoid_t0ype
_057_vagrant_057_stream_056_sats__fprint_stream__2__1(atstkind_type(atstype_ptrk), atstkind_type(atstype_ptrk), atstkind_t0ype(atstype_int), atstype_funptr) ;

#if(0)
#if(0)
ATSglobaldec()
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gt_g0int_int__4(atstkind_t0ype(atstyvar_type(tk)), atstkind_t0ype(atstype_int)) ;
#endif // end of [QUALIFIED]
#endif // end of [TEMPLATE]

ATSstaticdec()
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gt_g0int_int__4__1(atstkind_t0ype(atstype_int), atstkind_t0ype(atstype_int)) ;

#if(0)
ATSglobaldec()
atsvoid_t0ype
mainats_void_0() ;
#endif // end of [QUALIFIED]

/*
/vagrant/_test.dats: 306(line=14, offs=16) -- 641(line=25, offs=2)
*/
/*
local: 
global: test$0$0(level=0)
local: 
global: 
*/
ATSglobaldec()
atsvoid_t0ype
_057_vagrant_057__test_056_dats__test(atstkind_type(atstype_ptrk) arg0)
{
/* tmpvardeclst(beg) */
ATStmpdec_void(tmpret0, atsvoid_t0ype) ;
ATStmpdec(tmp1, atstkind_type(atstype_ptrk)) ;
ATStmpdec_void(tmp9, atsvoid_t0ype) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/_test.dats: 301(line=14, offs=11) -- 641(line=25, offs=2)
*/
ATSINSlab(__patsflab_test):
/*
emit_instr: loc0 = /vagrant/_test.dats: 315(line=14, offs=25) -- 641(line=25, offs=2)
*/
/*
letpush(beg)
*/
/*
emit_instr: loc0 = /vagrant/_test.dats: 337(line=15, offs=11) -- 353(line=15, offs=27)
*/
ATSINSmove(tmp1, _057_vagrant_057_file_056_sats__filestream(arg0)) ;

/*
emit_instr: loc0 = /vagrant/_test.dats: 580(line=24, offs=10) -- 638(line=24, offs=68)
*/
ATSINSmove_void(tmp9, _057_vagrant_057_stream_056_sats__fprint_stream__2__1(atspre_FILE_stdout, tmp1, ATSPMVi0nt(1000), ATSPMVfunlab(p_1))) ;

/*
letpush(end)
*/

/*
emit_instr: loc0 = /vagrant/_test.dats: 315(line=14, offs=25) -- 317(line=14, offs=27)
*/
ATSINSmove_void(tmpret0, ATSempty()) ;
/*
emit_instr: loc0 = /vagrant/_test.dats: 315(line=14, offs=25) -- 641(line=25, offs=2)
*/
/*
INSletpop()
*/
/* funbodyinstrlst(end) */
ATSreturn_void(tmpret0) ;
} /* end of [_057_vagrant_057__test_056_dats__test] */

/*
/vagrant/_test.dats: 363(line=17, offs=6) -- 569(line=23, offs=3)
*/
/*
local: 
global: p_1$0(level=1)
local: 
global: 
*/
ATSstaticdec()
atsvoid_t0ype
p_1(atstkind_type(atstype_ptrk) arg0, atstype_boxed arg1)
{
/* tmpvardeclst(beg) */
ATStmpdec_void(tmpret2, atsvoid_t0ype) ;
ATStmpdec(tmp3, atstkind_t0ype(atstype_char)) ;
ATStmpdec(tmp4, atstype_boxed) ;
ATStmpdec_void(tmp5, atsvoid_t0ype) ;
ATStmpdec_void(tmp6, atsvoid_t0ype) ;
ATStmpdec_void(tmp7, atsvoid_t0ype) ;
ATStmpdec_void(tmp8, atsvoid_t0ype) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/_test.dats: 363(line=17, offs=6) -- 569(line=23, offs=3)
*/
ATSINSlab(__patsflab_p_1):
/*
emit_instr: loc0 = /vagrant/_test.dats: 414(line=17, offs=57) -- 569(line=23, offs=3)
*/
/*
letpush(beg)
*/
/*
emit_instr: loc0 = /vagrant/_test.dats: 432(line=18, offs=7) -- 445(line=18, offs=20)
*/

/*
emit_instr: loc0 = /vagrant/_test.dats: 438(line=18, offs=13) -- 439(line=18, offs=14)
*/
ATSINSmove(tmp3, ATSSELcon(arg1, postiats_tysum_0, atslab__0)) ;
/*
emit_instr: loc0 = /vagrant/_test.dats: 441(line=18, offs=16) -- 444(line=18, offs=19)
*/
ATSINSmove(tmp4, ATSSELcon(arg1, postiats_tysum_0, atslab__1)) ;
/*
emit_instr: loc0 = /vagrant/_test.dats: 462(line=19, offs=11) -- 477(line=19, offs=26)
*/
ATSINSmove_void(tmp5, atspre_fprint_char(arg0, tmp3)) ;

/*
emit_instr: loc0 = /vagrant/_test.dats: 489(line=20, offs=11) -- 506(line=20, offs=28)
*/
ATSINSmove_void(tmp6, atspre_fprint_string(arg0, ATSPMVstring(" "))) ;

/*
emit_instr: loc0 = /vagrant/_test.dats: 518(line=21, offs=11) -- 535(line=21, offs=28)
*/
ATSINSmove_void(tmp7, _057_vagrant_057_location_056_sats__fprint_location(arg0, tmp4)) ;

/*
emit_instr: loc0 = /vagrant/_test.dats: 547(line=22, offs=11) -- 565(line=22, offs=29)
*/
ATSINSmove_void(tmp8, atspre_fprint_string(arg0, ATSPMVstring("\n"))) ;

/*
letpush(end)
*/

/*
emit_instr: loc0 = /vagrant/_test.dats: 414(line=17, offs=57) -- 416(line=17, offs=59)
*/
ATSINSmove_void(tmpret2, ATSempty()) ;
/*
emit_instr: loc0 = /vagrant/_test.dats: 414(line=17, offs=57) -- 569(line=23, offs=3)
*/
/*
INSletpop()
*/
/* funbodyinstrlst(end) */
ATSreturn_void(tmpret2) ;
} /* end of [p_1] */

#if(0)
/*
/vagrant/stream.dats: 1527(line=69, offs=29) -- 1746(line=77, offs=4)
*/
/*
local: 
global: fprint_stream$2$0(level=0)
local: 
global: 
*/
ATSglobaldec()
/*
imparg = a(7621)
tmparg = S2Evar(a(7621))
tmpsub = None()
*/
atsvoid_t0ype
_057_vagrant_057_stream_056_sats__fprint_stream__2(atstkind_type(atstype_ptrk) arg0, atstkind_type(atstype_ptrk) arg1, atstkind_t0ype(atstype_int) arg2, atstype_funptr arg3)
{
/* tmpvardeclst(beg) */
ATStmpdec_void(tmpret10, atsvoid_t0ype) ;
ATStmpdec(tmp11, atstype_boxed) ;
ATStmpdec(tmp12, atstyvar_type(a)) ;
ATStmpdec(tmp13, atstkind_type(atstype_ptrk)) ;
ATStmpdec(tmp14, atstkind_t0ype(atstype_bool)) ;
ATStmpdec_void(tmp15, atsvoid_t0ype) ;
ATStmpdec_void(tmp16, atsvoid_t0ype) ;
ATStmpdec_void(tmp17, atsvoid_t0ype) ;
ATStmpdec(tmp18, atstkind_t0ype(atstype_int)) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/stream.dats: 1513(line=69, offs=15) -- 1746(line=77, offs=4)
*/
ATSINSlab(__patsflab_fprint_stream):
/*
emit_instr: loc0 = /vagrant/stream.dats: 1553(line=69, offs=55) -- 1556(line=69, offs=58)
*/
ATSINSmove_lazyeval(tmp11, atstype_boxed, arg1) ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1547(line=69, offs=49) -- 1746(line=77, offs=4)
*/
ATScaseofbeg()
/*
** ibranchlst-beg
*/
ATSbranchbeg() ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1565(line=70, offs=4) -- 1571(line=70, offs=10)
*/
__atstmplab0:
/*
emit_instr: loc0 = /vagrant/stream.dats: 1553(line=69, offs=55) -- 1556(line=69, offs=58)
*/
if (ATSCKptriscons(tmp11)) { ATSgoto(__atstmplab3) ; }
/*
emit_instr: loc0 = /vagrant/stream.dats: 1571(line=70, offs=10) -- 1571(line=70, offs=10)
*/
__atstmplab1:
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /vagrant/stream.dats: 1575(line=70, offs=14) -- 1594(line=70, offs=33)
*/
ATSINSmove_void(tmpret10, atspre_fprint_string(arg0, ATSPMVstring("nil"))) ;

ATSbranchend() ;

ATSbranchbeg() ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1599(line=71, offs=4) -- 1611(line=71, offs=16)
*/
__atstmplab2:
/*
emit_instr: loc0 = /vagrant/stream.dats: 1553(line=69, offs=55) -- 1556(line=69, offs=58)
*/
#if(0)
if (ATSCKptrisnull(tmp11)) { ATSINSdeadcode_fail() ; }
#endif
/*
emit_instr: loc0 = /vagrant/stream.dats: 1611(line=71, offs=16) -- 1611(line=71, offs=16)
*/
__atstmplab3:
/*
emit_instr: loc0 = /vagrant/stream.dats: 1605(line=71, offs=10) -- 1606(line=71, offs=11)
*/
ATSINSmove(tmp12, ATSSELcon(tmp11, postiats_tysum_1, atslab__0)) ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1608(line=71, offs=13) -- 1610(line=71, offs=15)
*/
ATSINSmove(tmp13, ATSSELcon(tmp11, postiats_tysum_1, atslab__1)) ;
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /vagrant/stream.dats: 1622(line=72, offs=6) -- 1629(line=72, offs=13)
*/
ATSINSmove(tmp14, PMVtmpltcst(gt_g0int_int<S2Eextkind(atstype_int)>)(arg2, ATSPMVi0nt(0))) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1619(line=72, offs=3) -- 1746(line=77, offs=4)
*/
ATSif(
tmp14
) ATSthen() {
/*
emit_instr: loc0 = /vagrant/stream.dats: 1639(line=73, offs=8) -- 1746(line=77, offs=4)
*/
/*
letpush(beg)
*/
/*
emit_instr: loc0 = /vagrant/stream.dats: 1662(line=74, offs=12) -- 1672(line=74, offs=22)
*/
ATSINSmove_void(tmp15, ATSfunclo_fun(arg3, (atstkind_type(atstype_ptrk), atstyvar_type(a)), atsvoid_t0ype)(arg0, tmp12)) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1685(line=75, offs=12) -- 1702(line=75, offs=29)
*/
ATSINSmove_void(tmp16, atspre_fprint_string(arg0, ATSPMVstring(":"))) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1732(line=76, offs=29) -- 1737(line=76, offs=34)
*/
ATSINSmove(tmp18, PMVtmpltcst(g0int_sub<S2Eextkind(atstype_int)>)(arg2, ATSPMVi0nt(1))) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1715(line=76, offs=12) -- 1741(line=76, offs=38)
*/
ATSINSmove_void(tmp17, PMVtmpltcst(fprint_stream<S2Evar(a(7621))>)(arg0, tmp13, tmp18, arg3)) ;

/*
letpush(end)
*/

/*
emit_instr: loc0 = /vagrant/stream.dats: 1639(line=73, offs=8) -- 1641(line=73, offs=10)
*/
ATSINSmove_void(tmpret10, ATSempty()) ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1639(line=73, offs=8) -- 1746(line=77, offs=4)
*/
/*
INSletpop()
*/
} ATSelse() {
/*
emit_instr: loc0 = /vagrant/stream.dats: 1746(line=77, offs=4) -- 1746(line=77, offs=4)
*/
ATSINSmove_void(tmpret10, ATSempty()) ;
} /* ATSendif */
ATSbranchend() ;

/*
** ibranchlst-end
*/
ATScaseofend()

/* funbodyinstrlst(end) */
ATSreturn_void(tmpret10) ;
} /* end of [_057_vagrant_057_stream_056_sats__fprint_stream__2] */
#endif // end of [TEMPLATE]

/*
/vagrant/stream.dats: 1527(line=69, offs=29) -- 1746(line=77, offs=4)
*/
/*
local: 
global: fprint_stream$2$1(level=1)
local: 
global: 
*/
ATSstaticdec()
/*
imparg = a(7621)
tmparg = S2Evar(a(7621))
tmpsub = Some(a(7621) -> S2Eapp(S2Ecst(pair); S2Ecst(char), S2Ecst(location)))
*/
atsvoid_t0ype
_057_vagrant_057_stream_056_sats__fprint_stream__2__1(atstkind_type(atstype_ptrk) arg0, atstkind_type(atstype_ptrk) arg1, atstkind_t0ype(atstype_int) arg2, atstype_funptr arg3)
{
/* tmpvardeclst(beg) */
ATStmpdec_void(tmpret10__1, atsvoid_t0ype) ;
ATStmpdec(tmp11__1, atstype_boxed) ;
ATStmpdec(tmp12__1, atstype_boxed) ;
ATStmpdec(tmp13__1, atstkind_type(atstype_ptrk)) ;
ATStmpdec(tmp14__1, atstkind_t0ype(atstype_bool)) ;
ATStmpdec_void(tmp15__1, atsvoid_t0ype) ;
ATStmpdec_void(tmp16__1, atsvoid_t0ype) ;
ATStmpdec_void(tmp17__1, atsvoid_t0ype) ;
ATStmpdec(tmp18__1, atstkind_t0ype(atstype_int)) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/stream.dats: 1513(line=69, offs=15) -- 1746(line=77, offs=4)
*/
ATSINSlab(__patsflab_fprint_stream):
/*
emit_instr: loc0 = /vagrant/stream.dats: 1553(line=69, offs=55) -- 1556(line=69, offs=58)
*/
ATSINSmove_lazyeval(tmp11__1, atstype_boxed, arg1) ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1547(line=69, offs=49) -- 1746(line=77, offs=4)
*/
ATScaseofbeg()
/*
** ibranchlst-beg
*/
ATSbranchbeg() ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1565(line=70, offs=4) -- 1571(line=70, offs=10)
*/
__atstmplab0:
/*
emit_instr: loc0 = /vagrant/stream.dats: 1553(line=69, offs=55) -- 1556(line=69, offs=58)
*/
if (ATSCKptriscons(tmp11__1)) { ATSgoto(__atstmplab3) ; }
/*
emit_instr: loc0 = /vagrant/stream.dats: 1571(line=70, offs=10) -- 1571(line=70, offs=10)
*/
__atstmplab1:
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /vagrant/stream.dats: 1575(line=70, offs=14) -- 1594(line=70, offs=33)
*/
ATSINSmove_void(tmpret10__1, atspre_fprint_string(arg0, ATSPMVstring("nil"))) ;

ATSbranchend() ;

ATSbranchbeg() ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1599(line=71, offs=4) -- 1611(line=71, offs=16)
*/
__atstmplab2:
/*
emit_instr: loc0 = /vagrant/stream.dats: 1553(line=69, offs=55) -- 1556(line=69, offs=58)
*/
#if(0)
if (ATSCKptrisnull(tmp11__1)) { ATSINSdeadcode_fail() ; }
#endif
/*
emit_instr: loc0 = /vagrant/stream.dats: 1611(line=71, offs=16) -- 1611(line=71, offs=16)
*/
__atstmplab3:
/*
emit_instr: loc0 = /vagrant/stream.dats: 1605(line=71, offs=10) -- 1606(line=71, offs=11)
*/
ATSINSmove(tmp12__1, ATSSELcon(tmp11__1, postiats_tysum_2, atslab__0)) ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1608(line=71, offs=13) -- 1610(line=71, offs=15)
*/
ATSINSmove(tmp13__1, ATSSELcon(tmp11__1, postiats_tysum_2, atslab__1)) ;
/*
emit_instr: loc0 = : 0(line=0, offs=0) -- 0(line=0, offs=0)
*/
/*
ibranch-mbody:
*/
/*
emit_instr: loc0 = /vagrant/stream.dats: 1622(line=72, offs=6) -- 1629(line=72, offs=13)
*/
ATSINSmove(tmp14__1, ATSLIB_056_prelude__gt_g0int_int__4__1(arg2, ATSPMVi0nt(0))) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1619(line=72, offs=3) -- 1746(line=77, offs=4)
*/
ATSif(
tmp14__1
) ATSthen() {
/*
emit_instr: loc0 = /vagrant/stream.dats: 1639(line=73, offs=8) -- 1746(line=77, offs=4)
*/
/*
letpush(beg)
*/
/*
emit_instr: loc0 = /vagrant/stream.dats: 1662(line=74, offs=12) -- 1672(line=74, offs=22)
*/
ATSINSmove_void(tmp15__1, ATSfunclo_fun(arg3, (atstkind_type(atstype_ptrk), atstype_boxed), atsvoid_t0ype)(arg0, tmp12__1)) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1685(line=75, offs=12) -- 1702(line=75, offs=29)
*/
ATSINSmove_void(tmp16__1, atspre_fprint_string(arg0, ATSPMVstring(":"))) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1732(line=76, offs=29) -- 1737(line=76, offs=34)
*/
ATSINSmove(tmp18__1, atspre_g0int_sub_int(arg2, ATSPMVi0nt(1))) ;

/*
emit_instr: loc0 = /vagrant/stream.dats: 1715(line=76, offs=12) -- 1741(line=76, offs=38)
*/
ATSINSmove_void(tmp17__1, _057_vagrant_057_stream_056_sats__fprint_stream__2__1(arg0, tmp13__1, tmp18__1, arg3)) ;

/*
letpush(end)
*/

/*
emit_instr: loc0 = /vagrant/stream.dats: 1639(line=73, offs=8) -- 1641(line=73, offs=10)
*/
ATSINSmove_void(tmpret10__1, ATSempty()) ;
/*
emit_instr: loc0 = /vagrant/stream.dats: 1639(line=73, offs=8) -- 1746(line=77, offs=4)
*/
/*
INSletpop()
*/
} ATSelse() {
/*
emit_instr: loc0 = /vagrant/stream.dats: 1746(line=77, offs=4) -- 1746(line=77, offs=4)
*/
ATSINSmove_void(tmpret10__1, ATSempty()) ;
} /* ATSendif */
ATSbranchend() ;

/*
** ibranchlst-end
*/
ATScaseofend()

/* funbodyinstrlst(end) */
ATSreturn_void(tmpret10__1) ;
} /* end of [_057_vagrant_057_stream_056_sats__fprint_stream__2__1] */

#if(0)
/*
/ats2/prelude/DATS/integer.dats: 28963(line=798, offs=14) -- 29003(line=798, offs=54)
*/
/*
local: 
global: gt_g0int_int$4$0(level=0)
local: 
global: 
*/
ATSglobaldec()
/*
imparg = tk(4431)
tmparg = S2Evar(tk(4431))
tmpsub = None()
*/
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gt_g0int_int__4(atstkind_t0ype(atstyvar_type(tk)) arg0, atstkind_t0ype(atstype_int) arg1)
{
/* tmpvardeclst(beg) */
ATStmpdec(tmpret28, atstkind_t0ype(atstype_bool)) ;
ATStmpdec(tmp29, atstkind_t0ype(atstyvar_type(tk))) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 28950(line=798, offs=1) -- 29003(line=798, offs=54)
*/
ATSINSlab(__patsflab_gt_g0int_int):
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 28989(line=798, offs=40) -- 29001(line=798, offs=52)
*/
ATSINSmove(tmp29, PMVtmpltcst(g0int2int<S2Eextkind(atstype_int), S2Evar(tk(4431))>)(arg1)) ;

/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 28972(line=798, offs=23) -- 29003(line=798, offs=54)
*/
ATSINSmove(tmpret28, PMVtmpltcst(g0int_gt<S2Evar(tk(4431))>)(arg0, tmp29)) ;

/* funbodyinstrlst(end) */
ATSreturn(tmpret28) ;
} /* end of [ATSLIB_056_prelude__gt_g0int_int__4] */
#endif // end of [TEMPLATE]

/*
/ats2/prelude/DATS/integer.dats: 28963(line=798, offs=14) -- 29003(line=798, offs=54)
*/
/*
local: 
global: gt_g0int_int$4$1(level=2)
local: 
global: 
*/
ATSstaticdec()
/*
imparg = tk(4431)
tmparg = S2Evar(tk(4431))
tmpsub = Some(tk(4431) -> S2Eextkind(atstype_int))
*/
atstkind_t0ype(atstype_bool)
ATSLIB_056_prelude__gt_g0int_int__4__1(atstkind_t0ype(atstype_int) arg0, atstkind_t0ype(atstype_int) arg1)
{
/* tmpvardeclst(beg) */
ATStmpdec(tmpret28__1, atstkind_t0ype(atstype_bool)) ;
ATStmpdec(tmp29__1, atstkind_t0ype(atstype_int)) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 28950(line=798, offs=1) -- 29003(line=798, offs=54)
*/
ATSINSlab(__patsflab_gt_g0int_int):
/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 28989(line=798, offs=40) -- 29001(line=798, offs=52)
*/
ATSINSmove(tmp29__1, atspre_g0int2int_int_int(arg1)) ;

/*
emit_instr: loc0 = /ats2/prelude/DATS/integer.dats: 28972(line=798, offs=23) -- 29003(line=798, offs=54)
*/
ATSINSmove(tmpret28__1, atspre_g0int_gt_int(arg0, tmp29__1)) ;

/* funbodyinstrlst(end) */
ATSreturn(tmpret28__1) ;
} /* end of [ATSLIB_056_prelude__gt_g0int_int__4__1] */

/*
/vagrant/_test.dats: 665(line=29, offs=17) -- 713(line=31, offs=2)
*/
/*
local: 
global: mainats_void_0$9$0(level=0)
local: 
global: 
*/
ATSglobaldec()
atsvoid_t0ype
mainats_void_0()
{
/* tmpvardeclst(beg) */
ATStmpdec_void(tmpret32, atsvoid_t0ype) ;
ATStmpdec_void(tmp33, atsvoid_t0ype) ;
/* tmpvardeclst(end) */
/* funbodyinstrlst(beg) */
/*
emit_instr: loc0 = /vagrant/_test.dats: 659(line=29, offs=11) -- 713(line=31, offs=2)
*/
ATSINSlab(__patsflab_main_void_0):
/*
emit_instr: loc0 = /vagrant/_test.dats: 670(line=29, offs=22) -- 713(line=31, offs=2)
*/
/*
letpush(beg)
*/
/*
emit_instr: loc0 = /vagrant/_test.dats: 691(line=30, offs=10) -- 709(line=30, offs=28)
*/
ATSINSmove_void(tmp33, _057_vagrant_057__test_056_dats__test(ATSPMVstring("./file.dats"))) ;

/*
letpush(end)
*/

/*
emit_instr: loc0 = /vagrant/_test.dats: 670(line=29, offs=22) -- 672(line=29, offs=24)
*/
ATSINSmove_void(tmpret32, ATSempty()) ;
/*
emit_instr: loc0 = /vagrant/_test.dats: 670(line=29, offs=22) -- 713(line=31, offs=2)
*/
/*
INSletpop()
*/
/* funbodyinstrlst(end) */
ATSreturn_void(tmpret32) ;
} /* end of [mainats_void_0] */

/*
** for initialization(dynloading)
*/
atsvoid_t0ype
_057_vagrant_057__test_056_dats__dynload()
{
ATSdynload0(
_057_vagrant_057__test_056_dats__dynloadflag
) ;
ATSif(
ATSCKiseqz(
_057_vagrant_057__test_056_dats__dynloadflag
)
) ATSthen() {
ATSdynloadset(_057_vagrant_057__test_056_dats__dynloadflag) ;
/*
dynexnlst-initize(beg)
*/
/*
dynexnlst-initize(end)
*/
} /* ATSendif */
ATSreturn_void() ;
} /* end of [*_dynload] */

/*
** the ATS runtime
*/
#ifndef _ATS_CCOMP_RUNTIME_NONE
#include "pats_ccomp_runtime.c"
#include "pats_ccomp_runtime_memalloc.c"
#ifndef _ATS_CCOMP_EXCEPTION_NONE
#include "pats_ccomp_runtime2_dats.c"
#ifndef _ATS_CCOMP_RUNTIME_TRYWITH_NONE
#include "pats_ccomp_runtime_trywith.c"
#endif /* _ATS_CCOMP_RUNTIME_TRYWITH_NONE */
#endif // end of [_ATS_CCOMP_EXCEPTION_NONE]
#endif /* _ATS_CCOMP_RUNTIME_NONE */

/*
** the [main] implementation
*/
int
main
(
int argc, char **argv, char **envp
) {
int err = 0 ;
_057_vagrant_057__test_056_dats__dynload() ;
ATSmainats_void_0(err) ;
return (err) ;
} /* end of [main] */

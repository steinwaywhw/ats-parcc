
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 
SRC := $(SRC) location.dats parcc.dats file.dats string.dats token.dats list.dats stream.dats pair.dats 


test: 
	patscc _test.dats $(SRC) -DATS_MEMALLOC_LIBC -latslib
	#patscc stream.dats parcc.dats location.dats -DATS_MEMALLOC_LIBC
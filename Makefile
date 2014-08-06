
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

test: 
	patscc _test.dats file.dats location.dats string.dats token.dats list.dats stream.dats pair.dats parcc.dats -DATS_MEMALLOC_LIBC -latslib
	#patscc stream.dats parcc.dats location.dats -DATS_MEMALLOC_LIBC
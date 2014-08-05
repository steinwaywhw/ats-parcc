
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

test: 
	 #patscc _test.dats file.dats location.dats stream.dats -DATS_MEMALLOC_LIBC -latslib
	patscc stream.dats parcc.dats location.dats -DATS_MEMALLOC_LIBC
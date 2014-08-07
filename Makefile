
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 
SRC := util/*.dats string/*.dats lexing/*.dats file/*.dats parcc.dats dynload.dats

all:
	echo $(SRC) 
	patscc test/test.dats $(SRC) -DATS_MEMALLOC_LIBC -latslib
	#patscc stream.dats parcc.dats location.dats -DATS_MEMALLOC_LIBC
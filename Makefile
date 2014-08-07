
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 
SRC := util/*.dats string/*.dats lexing/*.dats file/*.dats parcc.dats dynload.dats
DEL := *_dats.c *_sats.c *.out

all:
	patscc test/test.dats $(SRC) -DATS_MEMALLOC_LIBC -o main -latslib

clean:
	rm $(DEL)
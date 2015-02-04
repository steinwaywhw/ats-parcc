
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 
SRC := util/*.dats lexing/*.dats file/*.dats parcc.dats #dynload.dats
DEL := *_dats.c *_sats.c *.out main

all:
	patscc $(SRC) -DATS_MEMALLOC_LIBC -o main -latslib -lm

clean:
	rm -f $(DEL)



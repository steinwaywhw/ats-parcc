
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 

# SRC := util/*.dats sexp/*.dats file/*.dats parcc.dats lexcc.dats #dynload.dats 
SRC := util/*.dats parcc.dats  

DEL := *_dats.c *_sats.c *.out main

all:
	patscc $(SRC) -g -DATS_MEMALLOC_LIBC -o main -latslib -lm

clean:
	rm -f $(DEL)



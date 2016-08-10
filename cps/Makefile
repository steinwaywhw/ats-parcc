
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 

# SRC := util/*.dats sexp/*.dats file/*.dats parcc.dats lexcc.dats #dynload.dats 

# SRC := util/*.dats parcc.dats  untyped/*.dats

# DEL := *_dats.c *_sats.c *.out main


# -latslib -lm
UTIL=~/Git/ats-utils
PATSCC=patscc -IATS ~/Git/ats-utils -DATS_MEMALLOC_LIBC -lm -latslib

all: test
	# $(PATSCC) patscc.dats

tc: 
	$(PATSCC) -tcats parcc.dats parser.dats

clean:
	rm -f *ats.c *.out

test: ./* $(UTIL)/*
	$(PATSCC) \
		parcc.dats parser.dats parcc.sats \
		$(UTIL)/list.dats $(UTIL)/maybe.dats $(UTIL)/string.dats $(UTIL)/stream.dats 
	




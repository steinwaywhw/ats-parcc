
#LIBPATH = -L${PATSHOME}/ccomp/atslib/lib -L${PATSHOME}/ccomp/atslib/lib64

#SRC := location.sats parcc.sats file.sats string.sats token.sats list.sats stream.sats pair.sats 

# SRC := util/*.dats sexp/*.dats file/*.dats parcc.dats lexcc.dats #dynload.dats 

# SRC := util/*.dats parcc.dats  untyped/*.dats

# DEL := *_dats.c *_sats.c *.out main


# -latslib -lm
UTIL=$(HOME)/Git/ats-utils
AR=ar -rcs
PATSOPT=patsopt -IATS $(UTIL)
PATSCC=patscc -DATS_MEMALLOC_LIBC -IATS $(UTIL) -lm -latslib -L$(UTIL) -latsutils 
CC=gcc -DATS_MEMALLOC_LIBC -I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime -g
CCFLAGS=-fPIC -O2
RMF=rm -rf


all: libatsparcc.a
	
%_sats.c: %.sats 
	$(PATSOPT) -o $@ -s $^

%_dats.c: %.dats
	$(PATSOPT) -o $@ -d $^

%.o: %.c 
	$(CC) $(CCFLAGS) -c $^ -o $@

libatsparcc.a: parcc_dats.o parcc_sats.o parser_dats.o 
	$(AR) $@ $^

tc: 
	$(PATSCC) -tcats parcc.dats parser.dats

clean:
	$(RMF) *ats.c *.out *.o

cleanall:
	$(RMF) *ats.c *.o *.a *.out *.out.dSYM

test: testall.dats sexp.dats libatsparcc.a 
	$(PATSCC) -L$(PWD) -latsparcc testall.dats sexp.dats
		
	




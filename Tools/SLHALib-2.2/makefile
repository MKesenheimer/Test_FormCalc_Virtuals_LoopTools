# --- variables defined by configure ---

SRC = ./src
PREFIX = unknown

EXE = 
DEF = -D
UNDERSCORE = 1

FC = gfortran
FFLAGS = -O0 -g -ffixed-line-length-none -ff2c -Wall -Wtabs   -DU77EXT=0

CC = gcc
CFLAGS = -O3 -g -fomit-frame-pointer -ffast-math -Wall  -m64

LDFLAGS = -lpthread -L/usr/local/lib/gcc/x86_64-apple-darwin14.4.0/5.1.0 -L/usr/local/lib/gcc/x86_64-apple-darwin14.4.0/5.1.0/../../.. -lgfortran -lSystem -lgcc_ext.10.5 -lquadmath -lm -lgcc_ext.10.5 -lSystem -L/usr/local/lib/gcc/x86_64-apple-darwin14.4.0/5.1.0 -L/usr/local/lib/gcc/x86_64-apple-darwin14.4.0/5.1.0/../../.. -lgfortran -lSystem -lgcc_ext.10.5 -lquadmath -lm -lgcc_ext.10.5 -lSystem

# --- end defs by configure ---


BLD = build

LIB = $(BLD)/libSLHA.a
FCC = $(BLD)/fcc
CSLHA = $(BLD)/CSLHA.h
DEMO = $(BLD)/copy $(BLD)/table $(BLD)/timing
INCLUDE = $(SRC)/SLHA.h $(SRC)/SLHADefs.h $(SRC)/PDG.h $(CSLHA)


default: $(LIB) $(FCC) $(CSLHA)
	 cp -p $(LIB) ../

demo: $(DEMO)

all: default demo

install: default
	-mkdir $(PREFIX)
	-mkdir $(PREFIX)/lib $(PREFIX)/include $(PREFIX)/bin
	cp -p $(LIB) $(PREFIX)/lib
	cp -p $(LIB) ../
	cp -p $(INCLUDE) $(PREFIX)/include
	cp -p $(FCC) $(PREFIX)/bin
	cd $(PREFIX)/bin ; ln -s fcc f++


OBJS = \
  $(BLD)/SLHAClear.o \
  $(BLD)/SLHARead.o \
  $(BLD)/SLHAWrite.o \
  $(BLD)/SLHAGetInfo.o \
  $(BLD)/SLHAPutInfo.o \
  $(BLD)/SLHANewDecay.o \
  $(BLD)/SLHAFindDecay.o \
  $(BLD)/SLHAAddDecay.o \
  $(BLD)/SLHAGetDecay.o \
  $(BLD)/SLHADecayTable.o \
  $(BLD)/SLHAExist.o \
  $(BLD)/SLHAValid.o \
  $(BLD)/SLHAPDGName.o \
  $(BLD)/SLHACopyString.o

TS = $(BLD)/timestamp

$(BLD)/SLHAClear.o: $(SRC)/SLHAClear.F $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAClear.o $(SRC)/SLHAClear.F

$(BLD)/SLHARead.o: $(SRC)/SLHARead.F $(SRC)/SLHAReadBlocks.h \
  $(SRC)/SLHAIntern.h $(SRC)/SLHADefs.h $(SRC)/PDG.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHARead.o $(SRC)/SLHARead.F

$(BLD)/SLHAWrite.o: $(SRC)/SLHAWrite.F $(SRC)/SLHAWriteBlocks.h \
  $(SRC)/SLHAIntern.h $(SRC)/SLHADefs.h $(SRC)/PDG.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAWrite.o $(SRC)/SLHAWrite.F

$(BLD)/SLHAGetInfo.o: $(SRC)/SLHAGetInfo.F $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAGetInfo.o $(SRC)/SLHAGetInfo.F

$(BLD)/SLHAPutInfo.o: $(SRC)/SLHAPutInfo.F $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAPutInfo.o $(SRC)/SLHAPutInfo.F

$(BLD)/SLHANewDecay.o: $(SRC)/SLHANewDecay.F $(SRC)/SLHAIntern.h \
  $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHANewDecay.o $(SRC)/SLHANewDecay.F

$(BLD)/SLHAFindDecay.o: $(SRC)/SLHAFindDecay.F $(SRC)/SLHAIntern.h \
  $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAFindDecay.o $(SRC)/SLHAFindDecay.F

$(BLD)/SLHAAddDecay.o: $(SRC)/SLHAAddDecay.F $(SRC)/SLHAIntern.h \
  $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAAddDecay.o $(SRC)/SLHAAddDecay.F

$(BLD)/SLHAGetDecay.o: $(SRC)/SLHAGetDecay.F $(SRC)/SLHAIntern.h \
  $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAGetDecay.o $(SRC)/SLHAGetDecay.F

$(BLD)/SLHADecayTable.o: $(SRC)/SLHADecayTable.F $(SRC)/SLHAIntern.h \
  $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHADecayTable.o $(SRC)/SLHADecayTable.F

$(BLD)/SLHAExist.o: $(SRC)/SLHAExist.F $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAExist.o $(SRC)/SLHAExist.F

$(BLD)/SLHAValid.o: $(SRC)/SLHAValid.F $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAValid.o $(SRC)/SLHAValid.F

$(BLD)/SLHAPDGName.o: $(SRC)/SLHAPDGName.F $(SRC)/PDG.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHAPDGName.o $(SRC)/SLHAPDGName.F

$(BLD)/SLHACopyString.o: $(SRC)/SLHACopyString.F $(SRC)/SLHADefs.h $(TS)
	$(FC) $(FFLAGS) -c -o $(BLD)/SLHACopyString.o $(SRC)/SLHACopyString.F

$(LIB): $(OBJS)
	ar cru $(LIB) $?
	-ranlib $(LIB)


$(FCC): $(SRC)/fcc.in
	sed "s:^libs=.*:libs=\"$(LDFLAGS)\":" $(SRC)/fcc.in > $(FCC)
	chmod 755 $(FCC)

$(CSLHA): $(SRC)/CSLHA.h.in $(SRC)/ftypes.h
	sed "s:UNDERSCORE:$(UNDERSCORE):" $(SRC)/ftypes.h $(SRC)/CSLHA.h.in > $(CSLHA)


$(BLD)/copy: $(SRC)/../demo/copy.F $(LIB)
	$(FC) $(FFLAGS) -I$(SRC) -o $(BLD)/copy $(SRC)/../demo/copy.F $(LIB)

$(BLD)/table: $(SRC)/../demo/table.F $(LIB)
	$(FC) $(FFLAGS) -I$(SRC) -o $(BLD)/table $(SRC)/../demo/table.F $(LIB)

$(BLD)/timing: $(SRC)/../demo/timing.F $(LIB)
	$(FC) $(FFLAGS) -I$(SRC) -o $(BLD)/timing $(SRC)/../demo/timing.F $(LIB)


$(TS):
	-mkdir $(BLD)
	touch $(TS)

clean:
	rm -fr $(BLD)


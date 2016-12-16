########################################################################
#                          -*- Makefile -*-                            #
########################################################################
# General Conventions for Makefiles
SHELL = /bin/sh
.SUFFIXES:
.SUFFIXES: .c .f .F .cc .cpp .h .hh .inc .o .a
.DEFAULT_GOAL := main

########################################################################
## Flags

## Compiler and additional compiler Flags
# use "./configure ifort" first
#FC  = ifort
# use "./configure gfortran" first
FC  = gfortran
CXX = g++
CC  = gcc

# p-flag to enable code profiling with gprof: gprof ./pwhg_main* gmon.out > analysis.txt
FCFLAGS  = -pg
CXXFLAGS = -g
CFLAGS   = -pg
  
# recommended compiler flags
ifeq ($(FC), ifort)
  LDFLAGS  = -pg
  REC_FCFLAGS   = -fpp -extend-source
  REC_FCFLAGS  += $(FCFLAGS)
else ifeq ($(FC), gfortran)
  LDFLAGS  = -ff2c -pg
  REC_FCFLAGS   = -fno-automatic -fno-range-check
  REC_FCFLAGS  += -ffixed-line-length-none -lgfortran -DU77EXT=0 -DQuad=0
  REC_FCFLAGS  += -ff2c -fno-second-underscore
  REC_FCFLAGS  += $(FCFLAGS)
  #$(error $(REC_FCFLAGS))
endif
REC_CXXFLAGS  = -fomit-frame-pointer -ffast-math -Wall -m64
REC_CXXFLAGS += $(CXXFLAGS)
REC_CFLAGS    = -fomit-frame-pointer -ffast-math -Wall -m64
REC_CFLAGS   += -DNOUNDERSCORE=0 -DBIGENDIAN=0
REC_CFLAGS   += $(CFLAGS)

UNAME = $(shell uname)
ifeq ($(UNAME), Darwin)
  #Mac OSX
  REC_CFLAGS   += -stdlib=libstdc++ -mmacosx-version-min=10.6 -Qunused-arguments
  REC_CXXFLAGS += -stdlib=libstdc++ -mmacosx-version-min=10.6 -Qunused-arguments
endif

## PDF
## choose PDF: native,lhapdf
## LHAPDF package has to be installed separately
## wheter to link precompiled LHAPDF library statically or compile it 
## from source (static, none)
PDF = lhapdf
PDFSTATIC = none

## path to static lhapdf library
#STATICLIBSLHAPDF = /opt/lib/libLHAPDF.a

## choose analysis: none, default
ANALYSIS = default

## path to LHAPDF config executable
LHAPDF_CONFIG = lhapdf-config

## path to fastjet config executable
FASTJET_CONFIG = fastjet-config

## warning for type-conversions -> basically useless, as those occur in
## too many places
#WARN  = -Wconversion -Wall -Wtabs -Wall -Wimplicit-interface
## -fbounds-check sometimes causes a weird error due to non-lazy
## evaluation of boolean in gfortran.
#WARN += -fbounds-check
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT = -O2

### generate directory build, if not yet existing
$(shell mkdir -p build)

########################################################################
## Runtime flags

## Preprocessor
# it might be advisable to use the -ffree-line-length-none 
# or -ffixed-line-length-none options
CPP = -cpp

## For debugging uncomment the following
#DEBUG = -ggdb -pg -D DEBUG
## more verbose debugging
DEBUG = -ggdb -pg -DDEBUG=0 -DDEBUGV
## If you wish to show all possible debug output
#DEBUG = -ggdb -pg -D DEBUG -D DEBUGV -D DEBUGQ -D CHECK

########################################################################
## Paths

WORKINGDIR = $(shell pwd)/..
SUBWORKINGDIR = $(shell pwd)

# important directories
TOOLS = $(SUBWORKINGDIR)/Tools

# includes
SINCLUDE    = $(SUBWORKINGDIR)/include
SLHAINCLUDE = $(TOOLS)/SLHALib-2.2/src
FINCLUDE    = $(SUBWORKINGDIR)/squaredme/include

########################################################################
## search for the files and set paths

vpath %.F $(SUBWORKINGDIR)/squaredme
vpath %.f $(SUBWORKINGDIR)
vpath %.f $(TOOLS)/functions
vpath %.o $(SUBWORKINGDIR)/build

########################################################################
## Source files

### Amplitudes ###
AMPS_F := $(wildcard $(SUBWORKINGDIR)/squaredme/*.F)
AMPS := $(notdir $(AMPS_F:.F=.o))

### Main Files ###
MAIN = Main.o funcprocess.o funcbasic.o VecSet.o phi1_2.o Born_phsp.o \
	init_couplings.o boostrot.o born_ubaru_gamg.o \
	bmunu_ubaru_gamg.o $(AMPS)

########################################################################
## Libraries
### LibSLHA in Tools/SLHALib-2.2 ###
LIBS += $(TOOLS)/libSLHA.a

### LoopTools in Tools/Looptools-2.12 ###
LIBS += $(TOOLS)/libooptools.a

### Libraries for linking c++ code
LIBS += -ldl -lstdc++

### for compressing lhe files
LIBS += -lz

########################################################################
## combine all flags, libraries and includes

ALL_FCFLAGS   = $(REC_FCFLAGS) $(OPT) $(WARN) $(CPP) $(DEBUG)
ALL_FCFLAGS  += -I$(SLHAINCLUDE)
ALL_FCFLAGS  += -I$(SINCLUDE) -I$(FINCLUDE)

LINKER = $(CPPFLAGS) $(LIBS) $(LDFLAGS)

HEADERS += $(wildcard *.h $(SLHAINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(SINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(FINCLUDE)/*.h)
     
########################################################################
## Rules, generate objects

%.o: %.f $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

%.o: %.F $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

########################################################################
## Rules, link
## type make -j4 [rule] to speed up the compilation

main: $(MAIN)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(MAIN)) $(LINKER) -o $@

clean:
	rm -f build/*.o main

########################################################################
#                       -*- End of Makefile -*-                        #
########################################################################

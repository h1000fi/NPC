TARGET = NPC_test
SRC = globals.f nanopore.f mrrrr.f rota36.f creador.f cadenas72mr.f fkfun.f chains.definitions.f monomers.definitions.f free_energy.f mainsolver.f saveresults.f fkpsol.f fkpset.f call_kinsol.f savetodisk.f fitspl.f lookup.f splint.f splines.f geom.f graftpoints.f lookup_kai.f kai.f cadenas_mk.f checking_actual_config.f TUNING.f create_protein.f arrayparas.f binary_array.f solver.f presolver.f

HOST=$(shell hostname)
$(info HOST is ${HOST})


ifeq ($(HOST),carapa)
LFLAGS = -lsundials_fkinsol -lsundials_fnvecserial -lsundials_kinsol -lsundials_nvecserial -lm
endif


# some definitions
SHELL = /bin/bash
FFLAGS= -O3#-fbacktrace -fbounds-check # -O3

ifeq ($(HOST),piluso.rosario-conicet.gov.ar)
LFLAGS = -L/home/mtagliazucchi.inquimae/software/kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64 -L/lib/../lib64 -L/usr/lib/../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../.. -lgfortranbegin -lgfortran -lm -lgcc_s
endif


ifeq ($(HOST),pear)
LFLAGS=-L/home/mario/software/KINSOL/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/usr/lib/gcc/x86_64-linux-gnu/4.6 -L/usr/lib/gcc/x86_64-linux
endif

ifeq ($(HOST),cnode01)
LFLAGS = -L/home/mtagliazucchi/software/Kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64 -L/lib/../lib64 -L/usr/lib/../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../.. -lgfortranbegin -lgfortran -lm -lgcc_s
endif

ifeq ($(HOST),master) 
LFLAGS = -L/shared/software/sundials-2.5.0-openmpi/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/usr/lib/gcc/x86_64-linux-gnu/4.6 -L/usr/lib/gcc/x86_64-linux-gnu/4.6/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/4.6/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/4.6/../../.. -lgfortran -lm -lgcc_s -lquadmath
endif

ifeq ($(HOST),mate.bme.northwestern.edu) 
LFLAGS = -L/home/mario/software/kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/usr/lib/gcc/x86_64-linux-gnu/4.6 -L/usr/lib/gcc/x86_64-linux-gnu/4.6/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/4.6/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/4.6/../../.. -lgfortran -lm -lgcc_s -lquadmath
endif

ifeq ($(HOST),quser13)
LFLAGS=-L/home/khl4149/kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4 -L/hpc/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64/ -L/lib/../lib64 -L/lib/../lib64/ -L/usr/lib/../lib64 -L/usr/lib/../lib64/ -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../ -L/lib64 -L/lib/ -L/usr/lib64 -L/usr/lib -limf -lm -lifport -lifcore -lsvml -lipgo -lirc -lpthread -lgcc_s -lirc_s -ldl
endif

ifeq ($(HOST),quser12)
LFLAGS=-L/home/khl4149/kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4 -L/hpc/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64/ -L/lib/../lib64 -L/lib/../lib64/ -L/usr/lib/../lib64 -L/usr/lib/../lib64/ -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../ -L/lib64 -L/lib/ -L/usr/lib64 -L/usr/lib -limf -lm -lifport -lifcore -lsvml -lipgo -lirc -lpthread -lgcc_s -lirc_s -ldl
endif

ifeq ($(HOST),quser11)
LFLAGS=-L/home/khl4149/kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4 -L/hpc/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64/ -L/lib/../lib64 -L/lib/../lib64/ -L/usr/lib/../lib64 -L/usr/lib/../lib64/ -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../ -L/lib64 -L/lib/ -L/usr/lib64 -L/usr/lib -limf -lm -lifport -lifcore -lsvml -lipgo -lirc -lpthread -lgcc_s -lirc_s -ldl
endif

ifeq ($(HOST),quser10)
LFLAGS=-L/home/khl4149/kinsol/lib -lsundials_fkinsol -lsundials_kinsol -lsundials_fnvecserial -lsundials_nvecserial -lm -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64 -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4 -L/hpc/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64 -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../../lib64/ -L/lib/../lib64 -L/lib/../lib64/ -L/usr/lib/../lib64 -L/usr/lib/../lib64/ -L/opt/intel/composer_xe_2015.0.090/ipp/../compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/ipp/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/compiler/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/mkl/lib/intel64/ -L/opt/intel/composer_xe_2015.0.090/tbb/lib/intel64/gcc4.4/ -L/usr/lib/gcc/x86_64-redhat-linux/4.4.7/../../../ -L/lib64 -L/lib/ -L/usr/lib64 -L/usr/lib -limf -lm -lifport -lifcore -lsvml -lipgo -lirc -lpthread -lgcc_s -lirc_s -ldl
endif

GIT_VERSION := $(shell git describe --abbrev=6 --dirty --always --tags)
GFLAGS=-cpp -D_VERSION=\"$(GIT_VERSION)\"

FF = mpif77 #${F90}
VER = ~/bin/NPC_test

all:	$(TARGET)

$(TARGET): $(SRC:.f=.o)
	$(FF) -o $(TARGET) $(SRC:.f=.o) $(LFLAGS) $(GFLAGS)
	cp $(TARGET) $(VER)

$(SRC:.f=.o): $(SRC)
	${FF} -c ${FFLAGS}  $(SRC) $(LFLAGS) $(GFLAGS)

install: all
	cp $(TARGET) $(VER)

clean:	
	@rm -f $(SRC:.f=.o) $(SRC:.f=.d) $(TARGET) *~

realclean: clean
	@rm -f .depend

depend dep:
	@$(FF)  $(CFLAGS) -MM $(SRC) > .depend 

ifeq (.depend, $(wildcard .depend))
include .depend
endif























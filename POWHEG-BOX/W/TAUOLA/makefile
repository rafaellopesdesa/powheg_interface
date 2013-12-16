-include config.mk

default:
	@echo "NOTE: This makefile is already prepared for C++ distribution"
	@echo "Only make cleanfortran can be used from the part truncated to fortran"
	@echo "For distribution including C++ part you may want to execute" 
	@echo "make all"

all: config.mk paths
	make clean;
	make default;
	(cd photos-F; make 10kD-all)
	(cd tauola-F; make CLEO)
	(cd tauola; make)
ifneq ($(BUILD_HEPMC), )
	mkdir -p ${HEPMCLOCATION}/build
	(cd ${HEPMCLOCATION}/build; ../src/configure --prefix="${HEPMCLOCATION}" --with-momentum=GEV --with-length=CM; make; make install)
endif
ifneq ($(BUILD_PYTHIA), )
	(cd ${PYTHIALOCATION}; ./configure --with-hepmc="${HEPMCLOCATION}")
	(cd ${PYTHIALOCATION}; make)
endif
	(cd demo_cxx; make)
ifneq ($(WITH_MCTESTER), )
#	(cd ${MCTESTERLOCATION}; make clean)
	(cd ${MCTESTERLOCATION}; make)
	(cd ${MCTESTERLOCATION}; make libHepMCEvent)
endif
	@echo "#################################################"
	@echo "        Package compiled successfully."
	@echo "Execute make in demo_cxx/examples to run example."
	@echo "#################################################"
	@true

clean:  cleanfortran
	rm config.mk
ifneq ($(BUILD_HEPMC), )
	(cd ${HEPMCLOCATION};    rm -f lib/lib* )
endif
ifneq ($(BUILD_PYTHIA), )
	(cd ${PYTHIALOCATION};   make clean)
	(cd ./demo_cxx;          make clean)
	(cd ./demo_cxx/examples  make clean)
endif

cleanfortran:  
	rm -f *.o *.a *~ *.exe
	(cd ./glibk;	         make clean)
	(cd ./jetset;	         make clean)
	(cd ./jetset2;	         make clean)
	(cd ./photos-F;	         make clean)
	(cd ./tauola-F;	         make clean)

paths:
ifeq ($(PATH_CHECK), )
	@echo "-------------------------------------------------"
	@echo "  Setup paths first (consider: source setup.sh)"
	@echo "-------------------------------------------------"
	@false
else
	@true
endif

config.mk:
	@echo "-------------------------------------------------"
	@echo "   For C++ Distribution, run ./configure first"
	@echo "-------------------------------------------------"
	@false

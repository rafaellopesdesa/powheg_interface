# Top level GNUmakefile for examplePkg
#############################################################

define setup =
setup gcc v4_5_1
setup ld v2_16_1
setup root v5_32_02
endef

all : W W_ew Z Z_ew

clean : W.clean W_ew.clean Z.clean Z_ew.clean

setup :
	$(setup)

W : | POWHEG-BOX/W/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/W pwhg_main
	$(MAKE) -C POWHEG-BOX/W main-PYTHIA-lhef

W.clean : | POWHEG-BOX/W/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/W clean

W_ew : | POWHEG-BOX/W_ew-BMNNP/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/W_ew-BMNNP pwhg_main
	$(MAKE) -C POWHEG-BOX/W_ew-BMNNP main-PYTHIA-lhef

W_ew.clean : | POWHEG-BOX/W_ew-BMNNP/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/W_ew-BMNNP clean

W.run : | POWHEG-BOX/W/Makefile
	$(setup)
	cd POWHEG-BOX/W ; \
	./pwhg_main ; \
	./main-PYTHIA-lhef ; \
	mv powheg.txt ../../ ; \
	rm -rf pwgevents.lhe ; \
	cd ../../

W_ew.run : | POWHEG-BOX/W_ew-BMNNP/Makefile
	$(setup)
	cd POWHEG-BOX/W_ew-BMNNP ; \
	./pwhg_main ; \
	./main-PYTHIA-lhef ; \
	mv powheg.txt ../../ ; \
	rm -rf pwgevents.lhe ; \
	cd ../../

Z : | POWHEG-BOX/Z/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/Z pwhg_main
	$(MAKE) -C POWHEG-BOX/Z main-PYTHIA-lhef

Z.clean : | POWHEG-BOX/Z/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/Z clean

Z_ew : | POWHEG-BOX/Z_ew-BMNNPV/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/Z_ew-BMNNPV pwhg_main
	$(MAKE) -C POWHEG-BOX/Z_ew-BMNNPV main-PYTHIA-lhef

Z_ew.clean : | POWHEG-BOX/Z_ew-BMNNPV/Makefile
	$(setup)
	$(MAKE) -C POWHEG-BOX/Z_ew-BMNNPV clean

Z.run : | POWHEG-BOX/Z/Makefile
	$(setup)
	cd POWHEG-BOX/Z ; \
	./pwhg_main ; \
	./main-PYTHIA-lhef ; \
	mv powheg.txt ../../ ; \
	rm -rf pwgevents.lhe ; \
	cd ../../

Z_ew.run : | POWHEG-BOX/Z_ew-BMNNPV/Makefile
	$(setup)
	cd POWHEG-BOX/Z_ew-BMNNPV ; \
	./pwhg_main ; \
	./main-PYTHIA-lhef ; \
	mv powheg.txt ../../ ; \
	rm -rf pwgevents.lhe ; \
	cd ../../

.PHONY : clean


############################################################
#include SoftRelTools/standard.mk

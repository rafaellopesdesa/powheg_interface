F77      = gfortran
F77FLAGS = -fPIC -g -fno-automatic -fno-backslash -ffixed-line-length-132
FFLAGS   = $(F77FLAGS)


LD       = gfortran
LDFLAGS  = -g
SOFLAGS  = -shared

RANLIB   = ranlib
AR       = ar

CPP     = /lib/cpp 
CPPFLAGS= -C -P -traditional-cpp
DIFF    = diff -ib

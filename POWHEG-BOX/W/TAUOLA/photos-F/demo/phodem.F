C.----------------------------------------------------------------------
C.
C.    PHOTOS:   PHOton radiation in decays TeST program
C.
C.    Purpose:  Example of application of PHOTOS.
C.
C.    Input Parameters:   None
C.
C.    Output Parameters:  None
C.
C.    Author(s):  B. van Eijk, E. Barberio        Created at:  31/05/90
C.                                                Last Update: 05/06/90
C.
C.----------------------------------------------------------------------
      PROGRAM PHOTST
C      IMPLICIT NONE
      INTEGER EVENT,NHEP0
#include "../../include/HEPEVT.h"
      INTEGER PHLUN
      COMMON/PHOLUN/PHLUN
C--
C--   Initialise PHOTOS
      CALL PHOINI
C--
C--   Loop over JETSET event until PHOTOS has generated one or more pho-
C--   tons.  Do this  for 10 JETSET events.  The event record is printed
C--   before and after photon emission.
      DO 20 EVENT=1,1 
        CALL LUEEVT(4,91.)
C--
C--   Conversion to /HEPEVT/ standard
        CALL LUHEPC(1)
C--
C--   Write event record before emission...
        NEVHEP=EVENT
        CALL PHODMP
        NHEP0=NHEP
C--
C--   Generate photon(s)... Arbitrary enforced generation.
C--   Normally line: IF (NHEP.EQ.NHEP0) GOTO 10 must be absent!
   10   CALL PHOTOS(1)
        IF (NHEP.EQ.NHEP0) GOTO 10
C--
C--   Write event record...
      WRITE(PHLUN,9050)
      WRITE(PHLUN,9040)
        CALL PHODMP
   20 CONTINUE
      WRITE(PHLUN,9000)
      WRITE(PHLUN,9010)
      WRITE(PHLUN,9020)
      WRITE(PHLUN,9030)
      WRITE(PHLUN,9020)
      WRITE(PHLUN,9010)
      STOP
 9000 FORMAT(1H1)
 9010 FORMAT(1H ,80('*'))
 9020 FORMAT(1H ,'*',78X,'*')
 9030 FORMAT(1H ,'**** PHOTOS Test Run has successfully ended',32X,
     &' ****')
 9040 FORMAT(1H ,26X,'=== after PHOTOS: ===')
 9050 FORMAT(1H0,80('='))
      END

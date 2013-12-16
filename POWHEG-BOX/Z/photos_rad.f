C    call photos to do FSR radiation corrections
C    evtnum: event number
C    ID1,ID2,ID3: particle IDs
C    PARTVEC1: particle 4-vectors before photos
C    PARTVEC2: particle 4-vectors after photos
      SUBROUTINE PHOTOS_RAD(EVTNUM, PARTVEC1, PARTVEC2, ID_PARTS, NPART) 

      INTEGER NHEP0
      integer d_h_nmxhep
      PARAMETER (d_h_NMXHEP=2000)
      REAL*8  d_h_phep,  d_h_vhep ! to be real*4/ *8  depending on host
      INTEGER d_h_nevhep,d_h_nhep,d_h_isthep,d_h_idhep,d_h_jmohep,
     $        d_h_jdahep
      COMMON /d_hepevt/
     $      d_h_nevhep,               ! serial number
     $      d_h_nhep,                 ! number of particles
     $      d_h_isthep(d_h_nmxhep),   ! status code
     $      d_h_idhep(d_h_nmxhep),    ! particle ident KF
     $      d_h_jmohep(2,d_h_nmxhep), ! parent particles
     $      d_h_jdahep(2,d_h_nmxhep), ! childreen particles
     $      d_h_phep(5,d_h_nmxhep),   ! four-momentum, mass [GeV]
     $      d_h_vhep(4,d_h_nmxhep)    ! vertex [mm]
* ----------------------------------------------------------------------
      LOGICAL qedrad
      COMMON /phoqed/ 
     $     qedrad(d_h_nmxhep)    ! Photos flag

      SAVE d_hepevt,phoqed

      INTEGER PHLUN
      COMMON/PHOLUN/PHLUN

C RCLSA Augmented the vector size to 8 (W, e, nu + 4 photons)
      REAL*8 PARTVEC1(5,3), PARTVEC2(5,4000)
      INTEGER PARTID(3), ID_PARTS(4000), TEMPID
      INTEGER EVTNUM,IPART,NPART,ID1,ID2,ID3

C     WRITE OUT EVENT INFORMATION TO STDHEP COMMON BLOCK
      d_h_NEVHEP=EVTNUM
      d_h_NHEP=3

C     ASSIGN INITIAL STATE PARTICLE AND FINAL STATE PARTICLE
      d_h_ISTHEP(1)=2
      d_h_ISTHEP(2)=1
      d_h_ISTHEP(3)=1
      
C     ASSIGN MOTHER FOR EACH PARTICLE
      d_h_JMOHEP(1,1)=0
      d_h_JMOHEP(1,2)=1
      d_h_JMOHEP(1,3)=1
      
C     ASSIGN DAUGHTERS FOR EACH PARTICLE
      d_h_JDAHEP(1,1)=2
      d_h_JDAHEP(2,1)=3
      d_h_JDAHEP(1,2)=0
      d_h_JDAHEP(2,2)=0
      d_h_JDAHEP(1,3)=0
      d_h_JDAHEP(2,3)=0
      
C     MODIFY PARTICLE ID FROM ISAJET CONVENTION TO PYTHIA CONVENTION
      DO 15 IPART=1,3        
        
C     ASSIGN FOUR MOMENTA AND MASS TO EACH PARTICLE
        D_H_IDHEP(IPART)=ID_PARTS(IPART)
        D_H_PHEP(1,IPART)=PARTVEC1(1,IPART)
        D_H_PHEP(2,IPART)=PARTVEC1(2,IPART)
        D_H_PHEP(3,IPART)=PARTVEC1(3,IPART)
        D_H_PHEP(4,IPART)=PARTVEC1(4,IPART)
        D_H_PHEP(5,IPART)=PARTVEC1(5,IPART)
 15   CONTINUE
      
      NHEP0=D_H_NHEP
      
C     ###############################################################
C     CALL PHOTOS
C     ###############################################################             
      CALL PHOTOS(1)

      NPART = D_H_NHEP 

      DO 30 I=1,D_H_NHEP
         DO 35 J=1,5
            PARTVEC2(J,I)=D_H_PHEP(J,I)
 35      CONTINUE
         ID_PARTS(I)=D_H_IDHEP(I)
 30   CONTINUE
     
      DO 70 I=1,D_H_NHEP
         call convert2isajet(D_H_IDHEP(I), tempid)
         ID_PARTS(I)=tempid
 70   CONTINUE

      END
     

       SUBROUTINE CONVERT2ISAJET(PYTID,ISAID)

       INTEGER PYTID, ISAID
       IF(PYTID.EQ.23) THEN  !Z0
          ISAID=90
        ELSEIF(PYTID.EQ.24) THEN !W+
          ISAID=80
        ELSEIF(PYTID.EQ.-24) THEN !W-
          ISAID=-80
        ELSEIF(PYTID.EQ.11) THEN !E-
          ISAID=12
        ELSEIF(PYTID.EQ.-11) THEN !E+
          ISAID=-12
        ELSEIF(PYTID.EQ.22) THEN !PHOTON
          ISAID=10
        ELSEIF(PYTID.EQ.13) THEN !MU-
          ISAID=14
        ELSEIF(PYTID.EQ.-13) THEN !MU+
          ISAID=-14
        ELSEIF(PYTID.EQ.15) THEN !TAU-
          ISAID=16
        ELSEIF(PYTID.EQ.-15) THEN !TAU+
          ISAID=-16
        ELSEIF(PYTID.EQ.12) THEN !BARNU_E
          ISAID=11
        ELSEIF(PYTID.EQ.-12) THEN !NU_E
          ISAID=-11
        ELSEIF(PYTID.EQ.14) THEN !BARNU_MU
          ISAID=13
        ELSEIF(PYTID.EQ.-14) THEN !NU_MU
          ISAID=-13
        ELSEIF(PYTID.EQ.16) THEN !BARNU_TAU
          ISAID=15
        ELSEIF(PYTID.EQ.-16) THEN !NU_TAU
          ISAID=-15
        ENDIF
       END

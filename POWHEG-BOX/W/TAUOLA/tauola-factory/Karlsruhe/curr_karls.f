      SUBROUTINE CURR_KARLS(MNUM,PIM1,PIM2,PIM3,PIM4,HADCUR)
      INTEGER MNUM,I
      REAL  PIM1(4),PIM2(4),PIM3(4),PIM4(4)
      COMPLEX HADCUR(4)
      REAL*8 QQ2,Q1(4),Q2(4),Q3(4),Q4(4)
      COMPLEX*16 HADR(4)

      LOGICAL INIT
      DATA INIT /.TRUE./
      SAVE INIT

      IF (INIT) THEN
         CALL had1_init
         INIT = .FALSE.
      ENDIF

      IF (MNUM.EQ.1) THEN  !  PI- PI- PI+ PI0
         Q1(1)=PIM1(4) ! PI-
         Q2(1)=PIM2(4) ! PI-
         Q3(1)=PIM3(4) ! PI0
         Q4(1)=PIM4(4) ! PI+
         DO I=1,3
            Q1(1+I)=PIM1(I)       
            Q2(1+I)=PIM2(I)       
            Q3(1+I)=PIM3(I)       
            Q4(1+I)=PIM4(I)       
         ENDDO
         QQ2=(Q1(1)+Q2(1)+Q3(1)+Q4(1))**2
         DO I=2,4
            QQ2=QQ2-(Q1(I)+Q2(I)+Q3(I)+Q4(I))**2
         ENDDO
C  Tomasz Pierzchala : in HAD4() position of PI+ and Pi0 is changed 
C                      to be in correct order according to TAUOLA routines.
C         CALL HAD4(QQ2,Q1,Q2,Q3,Q4,HADR)
C
          CALL HAD4(QQ2,Q1,Q2,Q4,Q3,HADR)
C         
      ELSEIF(MNUM.EQ.2) THEN ! PI0 PI0 PI0 PI-   
         Q1(1)=PIM1(4) ! PI0
         Q2(1)=PIM2(4) ! PI0
         Q3(1)=PIM3(4) ! PI0
         Q4(1)=PIM4(4) ! PI-
         DO I=1,3
            Q1(1+I)=PIM1(I)       
            Q2(1+I)=PIM2(I)       
            Q3(1+I)=PIM3(I)       
            Q4(1+I)=PIM4(I)       
         ENDDO
         QQ2=(Q1(1)+Q2(1)+Q3(1)+Q4(1))**2
         DO I=2,4
            QQ2=QQ2-(Q1(I)+Q2(I)+Q3(I)+Q4(I))**2
         ENDDO
         CALL HAD3(QQ2,Q1,Q2,Q3,Q4,HADR)
C         
      ELSE
         WRITE(*,*)' WRONG PARAMITER IN CURR_CPC; MNUM=',MNUM
         STOP
      ENDIF
C
      HADCUR(4)=HADR(1)
      DO I=1,3
         HADCUR(I)=HADR(I+1)
      ENDDO
      RETURN
      END
      

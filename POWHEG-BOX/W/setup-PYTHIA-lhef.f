      subroutine setup_PYTHIA_tune
C   100       A : Rick Field's CDF Tune A                     (Oct 2002)
C   103      DW : Rick Field's CDF Tune DW                    (Apr 2006)
C   320 Perugia 0 : "Perugia" update of S0-Pro                (Feb 2009)
C   
ccc      call PYTUNE(320)
      end

      subroutine setup_PYTHIA_parameters
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      double precision parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer MDCY,MDME,KFDP
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer pycomp
      external pycomp
c     multiple interactions
      logical mult_inter
      parameter (mult_inter=.true.)
      integer maxev
      common/mcmaxev/maxev
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     multiple interactions
c     (MI can increase a lot the execution time)
      if(.not.mult_inter) then
         mstp(81)=20   !No Multiple interactions. Force a call to PYEVNW 
      else
         mstp(81)=21   ! MPI on in the PYEVNW MPI scenario
      endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     photon radiation off quarks and leptons
c       mstj(41)=12              
c     No photon radiation off quarks and leptons
       mstj(41)=11              
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c      mstp(61)=0                !No IS shower
c      mstp(71)=0                !No FS shower
c      mstp(91)=0                !No Primordial kt
c      mstp(131)=0               !No Pile Up
c      mstp(111)=0               !No hadronization

      mstp(64) =3 !use Lambda_MC for IS shower > 6.4.19
c      mstp(64) =1 !use Lambda_MSbar (default)

c     number of warnings printed on the shell
      mstu(26)=20
c     call PYLIST(12) to see the PYTHIA decay table
ccccccccccccccccccccccccccccccccccccccccccccccccccc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     tolerate 2% of killed events
      mstu(22)=maxev/50
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
      end

      subroutine getmaxev(maxev)
      integer maxev
C--- Opens input file and counts number of events, setting MAXEV;
      call opencount(maxev)
      end

      subroutine UPINIT
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      double precision parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer MDCY,MDME,KFDP
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer pycomp
      external pycomp
      integer maxev
      common/mcmaxev/maxev
      common/evtcounter/evtnum
      integer evtnum
      evtnum=0
      nevhep=0
c read the header first, so lprup is set
      call lhefreadhdr(97)

c     Make PI0 stable as in herwig default
      mdcy(pycomp(111),1)=0
      if (lprup(1).eq.10015)  mdcy(pycomp(15),1)=0
      if (lprup(1).eq.9985)  mdcy(pycomp(15),1)=0
      end

      subroutine UPEVNT
      implicit none
      call lhefreadev(97)
      end


      subroutine upveto
c pythia routine to abort event
      end

      subroutine pyabeg
      open(unit=15, file='powheg.txt')
      call init_hist
      call phoini
      end

      subroutine pyaend
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      open(unit=99,file=pwgprefix(1:lprefix)//'POWHEG+PYTHIA-output.top'
     #     ,status='unknown')
      call pwhgsetout
      call pwhgtopout
      close(99)
      write(15,*) '0 0'
      close(15)
      end


      subroutine pyanal
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      integer mint
      double precision vint
      common/evtcounter/evtnum
      integer evtnum
      COMMON/PYINT1/MINT(400),VINT(400)
c     check parameters
      logical verbose
      parameter (verbose=.false.)

      integer ipart,imo
      real*8 partvec1(5,3),partvec2(5,4000)
      integer partid(3),id_parts(4000),nphotos

      real * 8 random, powheginput, flip
      external random, powheginput

      logical check_w, check_e, check_n

      if (random().lt.0.5) then
         flip = +1.D0
      else
         flip = -1.D0
      endif
      
      if(mint(51).ne.0) then
         if(verbose) then
            write(*,*) 'Killed event'
            write(*,*) 'Scalup= ',scalup
            call pylist(7)      !hepeup
            call pylist(2)      !all the event
         endif
         return
      endif
c      write(*,*) 'New event', nevhep, evtnum-1
c      if (nevhep.ne.(evtnum-1)) then 
c         write(*,*) 'Inconsistent event', nevhep, evtnum-1
c      endif
      check_e = .false.
      check_n = .false.
      check_w = .false.
      do ipart=1, nhep
       if ((isthep(ipart).eq.1).and.(abs(idhep(ipart)).eq.11)) then
          imo = jmohep(1, ipart)
          do while (abs(idhep(jmohep(1, imo))).eq.11)
             imo = jmohep(1, imo)
          enddo
          imo = jmohep(1, imo)
          if (abs(idhep(imo)).eq.24) then
             partvec1(1,2) = phep(1, ipart)
             partvec1(2,2) = phep(2, ipart)
             partvec1(3,2) = phep(3, ipart)
             partvec1(4,2) = phep(4, ipart)
             partvec1(5,2) = phep(5, ipart)
             id_parts(2) = idhep(ipart)
             check_e = .true.
C             write(*,*) 'Found the electron'
          endif
       else if ((isthep(ipart).eq.1).and.(abs(idhep(ipart)).eq.12)) then
          imo = jmohep(1, ipart)
          do while (abs(idhep(jmohep(1, imo))).eq.12)
             imo = jmohep(1, imo)
          enddo
          imo = jmohep(1, imo)
          if (abs(idhep(imo)).eq.24) then
             partvec1(1,3) = phep(1, ipart)
             partvec1(2,3) = phep(2, ipart)
             partvec1(3,3) = phep(3, ipart)
             partvec1(4,3) = phep(4, ipart)
             partvec1(5,3) = phep(5, ipart)
             id_parts(3) = idhep(ipart)
             check_n = .true.
C             write(*,*) 'Found the neutrino'
          endif
       else if ((isthep(ipart).eq.2).and.(abs(idhep(ipart)).eq.24)) then
          partvec1(1,1) = phep(1, ipart)
          partvec1(2,1) = phep(2, ipart)
          partvec1(3,1) = phep(3, ipart)
          partvec1(4,1) = phep(4, ipart)
          partvec1(5,1) = phep(5, ipart)
          id_parts(1) = idhep(ipart)
          check_w = .true.
c          write(*,*) 'Found the W'
       endif
      enddo
      if (.not.(check_e.and.check_n.and.check_w)) then
         write(*,*) 'Missing'
         write(*,*) 'Electron, Neutrino, W', check_e, check_n, check_w
      endif
      call photos_rad(nevhep, partvec1, partvec2, id_parts, nphotos)
c      if ((nphotos.lt.3).or.(nphotos.gt.5)) then
c         write(*,*) 'Bad PHOTOS'
c         do ipart=1, nphotos
c            write(*,*) ' - ', id_parts(ipart)
c         enddo
c      endif
      write(15,*) nevhep+1, 1.0
      write(15,*) 0., 0., 0.
      write(15,*) int(flip*id_parts(1)),
     & flip*partvec2(1, 1), flip*partvec2(2, 1), 
     & flip*partvec2(3, 1), partvec2(4, 1),
     & '0 0'
      do ipart=2, nphotos
         if (id_parts(ipart).eq.10) then
            write(15,*) id_parts(ipart),
     &           partvec2(1, ipart), partvec2(2, ipart), 
     &           partvec2(3, ipart), partvec2(4, ipart),
     &           '1 0'
         else
            write(15,*) int(flip*id_parts(ipart)),
     &       flip*partvec2(1, ipart), flip*partvec2(2, ipart), 
     &       flip*partvec2(3, ipart), partvec2(4, ipart),
     &           '1 0'
         endif
      enddo
      write(15,*) 0

      nevhep=nevhep+1
      if(abs(idwtup).eq.3) xwgtup=xwgtup*xsecup(1)
      call analysis(xwgtup)
      call pwhgaccumup 
      end

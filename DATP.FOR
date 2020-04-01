      PROGRAM DAT
C
C      DATGMA   DATA FILE REDUCTION TO GMA INPUT SET  WPP,ANL
C
C  W.P.POENITZ,ARGONNE NATIONAL LABORATORY - WEST,SEPT.,1984
C     PC Version renamed DAT.F  -  Modification Feb. 1996.
C
CVP Dimension ECOR(30,30) was increased at ECOR(100,100)
      character*2 NQND,NHEL,NQST,NHMO,NHFI,MTY,KCO1,KCO2,KCO3,LAB,
     1 NAU,NREF,NQT,NCOM,NQQA,NQQ
C
      COMMON /t/ ER(35,200),EQ(200),Q(200),T(35,200),NOD(35),LAB(35,8)
     1 ,MSEP(16)
      COMMON /excl/ NEXL(50)
C
      common /char/ NCOM(40,50),NQT(12),NAU(14),NREF(10)
C
      COMMON /main/ NR,NY,NQ,NT,NCO,NCS,NO,NZXCV(2),ENF(10),
     1 NENF(10),NID(5),NETG(11),EPA(3,11),NCST(10),NEC(2,10,10),E(400),
     2 S(400),F(12,400),ECOR(100,100),ID3,NCCO,
     3 IZZ,FCFC(10,10),INTR(5),EINTR(5,2),NQQ
C
C
      OPEN(14,FILE='DAT.INP')
      OPEN(15,FILE='DAT.LST')
      OPEN(12,FILE='GMDATA.CRD')
      OPEN(13,FILE='DAT.RES')
C
C
      NQND= 'EB'
      NHEL= 'EL'
      NQST= 'ST'
      NHMO= 'MO'
      NHFI= 'FI'
      MTY = '  '
C
C  NOM   MAX APRIORI DIMENSION FOR ONE CROSS SECTION
C
C  NQM   NUMBER OF APRIORI
C
C  MAXF  MAX DIMENSION OF DATA SET
C
      MAXF=400
      NOM=200
      NQM=33
      W=0.
C
C  IO1  INPUT OF GMA CONTROLS AND APRIORI FROM  DAT.INP
C
C  IO2  LIST FILE  DAT.LST
C
C  IO3  INPUT OF DATA FILE  GMDATA.CRD
C
C  IO4  OUTPUT OF GMA DATA SET TO  DAT.RES
C
      IO1=14
      IO2=15
      IO3=12
      IO4=13
      NQQA=NQST
      ID3=IO3
      WRITE(*,520)
  520 FORMAT(' LIMIT OF DIF/UNC SQ (F5.1)')
      READ(*,521) ULI
  521 FORMAT(F5.1)
C
C    COPY OF CONTROLS FOR GMA
C
      DO 80 K=1,10
      READ (IO1,260)KCO1,KCO2,KCO3,MC1,MC2,MC3,MC4,MC5,MC6,MC7,MC8
      write(*,483)KCO1
  483 format(' reading  ', a2)
      IF(KCO1 .eq. MTY) GO TO 84
      WRITE(IO4,260)KCO1,KCO2,KCO3,MC1,MC2,MC3,MC4,MC5,MC6,MC7,MC8
      WRITE(IO2,260)KCO1,KCO2,KCO3,MC1,MC2,MC3,MC4,MC5,MC6,MC7,MC8
  260 FORMAT(A2,A2,A1,8I5)
      IF(KCO1 .EQ. NHMO .AND. MC2 .EQ. 10) GO TO 400
      GO TO 401
C
C      data set numbers selected for downweighting
C
  400 READ(IO1,402)(MSEP(L),L=1,16)
      WRITE(IO2,402)(MSEP(L),L=1,16)
      WRITE(IO4,402)(MSEP(L),L=1,16)
      IF(MSEP(1) .EQ. 0) GO TO 407
      GO TO 400
  401 CONTINUE
  402 FORMAT(16I5)
      IF(KCO1 .EQ. NHFI .AND. MC1 .NE. 0) GO TO 403
      GO TO 405
C
C      fission spectrum
C
  403 READ (IO1,404) AE,BS
      WRITE(IO2,404) AE,BS
      WRITE(IO4,404) AE,BS
  404 FORMAT(2E13.5)
      IF(AE .EQ. 0.0) GO TO 407
      GO TO 403
  405 CONTINUE
      if(KCO1 .eq. NHEL) go to 406
      go to 407
C
C      data set numbers for exclusion
C
  406 JNEX=MC1
      read(IO1,408) (NEXL(l),l=1,JNEX)
  408 format(16i5)
      write(IO4,408) (NEXL(l),l=1,JNEX)
      write(IO2,468)
  468 format(/' Data Sets to be Excluded'/)
      write(IO2,408) (nexl(l),l=1,jnex)
  407 continue
   80 CONTINUE
   84 CONTINUE
C
C  CLEAR
C
      DO 5 K=1,NOM
      Q(K)=0.
      DO 5 L=1,NQM
      ER(L,K)=0.
    5 T(L,K)=0.
C
C  READ APRIRORI
C
      DO 1  L=1,NQM
      READ(IO1,99)(LAB(L,N),N=1,8)
   99 FORMAT(8A2)
      DO 2 K=1,NOM
      READ(IO1,100) EQ9,TQ9
  100 FORMAT(2E10.4)
      IF(EQ9 .EQ. 0.) GO TO 3
      ER(l,k)=EQ9
      T(l,k)=TQ9
    2 CONTINUE
      NOD(L)=NOM
      GO TO 4
    3 NOD(L)=K-1
    4 CONTINUE
    1 CONTINUE
C
C  TRANSFER APRIORI TO OUTPUT FILE
C
      ITOT=0
      DO 96 K=1,NQM
   96 ITOT=ITOT+NOD(K)
      ITOT=ITOT-2*NQM
      WRITE(IO4,264)ITOT,NQM
      WRITE(IO2,264)ITOT,NQM
  264 FORMAT(5HAPRI ,2I5)
      DO 7 L=1,NQM
      NOR=NOD(L)-1
      nor2=nor-1
      WRITE(IO4,99)(LAB(L,N),N=1,8)
      WRITE(IO2,99)(LAB(L,N),N=1,8)
      write(*,3731) L,NOR2,(LAB(L,N),N=1,8)
 3731 format('cross section',i5,' number',i5,8A2)
      DO 8 K=2,NOR
      K11=K-1
      WRITE(IO2,101)K11,ER(L,K),T(L,K)
  101 FORMAT(2X,I4,2X,2E12.4)
    8 WRITE(IO4,100)ER(L,K),T(L,K)
      WRITE(IO2,109)W,W
  109 format(8x,2e10.4)
      WRITE(IO4,100)W,W
    7 CONTINUE                      
C
C  START OF REDUCTION AND TRANSFER  *********************************************
C
  500 CONTINUE
      CALL DATRCL(1,NX,1)
      IF(NR .EQ. 9999) GO TO 160
      write(*,3733) NR
 3733 format(' read data set  ',i7)
C
C      Interpolation type (this is very system specific - does not apply for other 
C                           simultaneous evaluations)
C
      INT=1
      IF(NT .EQ. 1 .OR. NT .EQ. 2) GO TO 92
      IF(NT .EQ. 5 .OR. NT .EQ. 8) GO TO 92
      GO TO 94
   92 CONTINUE
      IF(NID(1) .EQ. 2 .OR. NID(1) .EQ. 5) GO TO 94
      IF(NID(1) .EQ. 10) GO TO 94
      IF(NID(1) .GT. 10) GO TO 94
      INT=2
   94 CONTINUE
C
C  CONSTRUCT APRIORI
C
      DO 25 K=1,NOM
   25 Q(K)=0.
      GO TO(11,11,12,12,13,14,15,13,15),NT
C
C  CS + CS SHAPE
C
   11 M1=NID(1)
      NON=NOD(M1)
      NON1=NON-1
      E11=(ER(M1,1)+ER(M1,2))/2.
      E22=(ER(M1,NON)+ER(M1,NON1))/2.
      DO 20 K=1,NON
      EQ(K)=ER(M1,K)
   20 Q(K)=T(M1,K)
      mxm=non
      mxm1=mxm-1
      write(*,4611) mxm
 4611 format(i6,'  cr. sec. apriori for interp. ')
      GO TO 30
C
C  RATIO + RATIO SHAPE
C
   12 M1=NID(1)
      M2=NID(2)
      NO1=NOD(M1)
      NON=NOD(M2)
      mxm=0
      DO 21 K=1,NO1
C
C      find matching energies
C
      etst1=er(m1,k)*0.9999
      etst2=er(m1,k)*1.0001
      do 55 l=1,non
      if(er(m2,l) .gt. etst1 .and. er(m2,l) .lt. etst2) go to 56
   55 continue
      go to 58
   56 mxm=mxm+1
      eq(mxm)=er(m1,k)
      if(t(m2,l) .eq. 0.0)go to 4616
      go to 4617
 4616 write(*,4618) m1,k,m2,l,eq(mxm)
 4618 format(' apriori constr. ',4i5,f10.7,'*****************')
      stop
 4617 continue
      Q(mxm)=T(M1,K)/T(M2,L)
   58 continue
   21 continue
      mxm1=mxm-1
      E11=(EQ(1)+EQ(2))/2.
      E22=(EQ(mxm)+EQ(mxm1))/2.
      write(*,4612) mxm
 4612 format(i6,'  ratio apriori for interp. ')
      GO TO 30
C
C  SUM AND SHAPE OF SUM
C
   13 M1=NID(1)
      M2=NID(2)
      M3=NID(3)
      NO1=NOD(M1)
      NON=NOD(M2)
      IF(M3 .EQ. 0) GO TO 22
      NO3=NOD(M3)
   22 mxm=0
      DO 23 K=1,NO1
      etst1=er(m1,k)*0.9999
      etst2=er(m1,k)*1.0001
      do 16 l=1,non
      if(er(m2,l) .gt. etst1 .and. er(m2,l) .lt. etst2) go to 17
   16 continue
      go to 26
   17 IF(M3 .EQ. 0) GO TO 18
      do 19 j=1,no3
      if(er(m3,j) .gt. etst1 .and. er(m3,j) .lt. etst2) go to 18
   19 continue
      go to 26
   18 mxm=mxm+1
      eq(mxm)=er(m1,k)
      q(mxm)=t(m1,k)+t(m2,l)
      if(m3 .eq. 0) go to 26
      q(mxm)=q(mxm)+t(m3,j)
   26 CONTINUE
   23 CONTINUE
      mxm1=mxm-1
      E11=(EQ(1)+EQ(2))/2.
      E22=(EQ(mxm)+EQ(mxm1))/2.
      write(*,4613) mxm
 4613 format(i6,'  sum apriori for interp. ')
      GO TO 30
C
C  RATIO OF CS VS. SUM + SHAPE
C
   15 M1=NID(1)
      M2=NID(2)
      M3=NID(3)
      NO1=NOD(M1)
      NON=NOD(M2)
      NO3=NOD(M3)
      mxm=0
      DO 27 K=1,NO1
      etst1=er(m1,k)*0.9999
      etst2=er(m1,k)*1.0001
      do 76 l=1,non
      if(er(m2,l) .gt. etst1 .and. er(m2,l) .lt. etst2) go to 77
   76 continue
      go to 71
   77 do 79 j=1,no3
      if(er(m3,j) .gt. etst1 .and. er(m3,j) .lt. etst2) go to 78
   79 continue
      go to 71
   78 mxm=mxm+1
      eq(mxm)=er(m1,k)
      q(mxm)=t(m1,k)/(t(m2,l)+t(m3,j))
   71 CONTINUE
   27 continue
      mxm1=mxm-1
      E11=(EQ(1)+EQ(2))/2.
      E22=(EQ(mxm)+EQ(mxm1))/2.
      write(*,4614) mxm
 4614 format(i6,'  cr. sec. vs sum apriori for interp. ')
C
C   REDUCTION
C
   30 CONTINUE
C
C  FIND USEFUL DATA RANGE
C
      IF(E(1) .GT. E22) GO TO 61
      IF(E(NO) .LT. E11) GO TO 61
   14 CONTINUE
      NQMM=0
      IF(NQQA .EQ. NQST) GO TO 62
      IF(NQQA .EQ. NQND) GO TO 63
      GO TO 64
   63 WRITE(IO4,250) NQMM,NQMM
      WRITE(IO2,250) NQMM,NQMM
  250 FORMAT(4HEDBL,1X,2I5)
   62 WRITE(IO4,251) NQMM,NQMM
      WRITE(IO2,251) NQMM,NQMM
  251 FORMAT(4HBLCK,1X,2I5)
      GO TO 64
C
C      out of range
C
   61 CONTINUE
      IF(NQQA .NE. NQND .AND. NQQ .EQ. NQND) NQQA=NQQ
      GO TO 500
   64 NQQA=NQQ
C
C      find nr of CS involved
C
      DO 66 MN=1,5
      IF(NID(MN) .EQ. 0) GO TO 67
   66 CONTINUE
   67 NNN=MN-1
      NID(4)=0
C
C      special marker for thermal constants
C
      IF(NR .GE. 910 .AND. NR .LE. 934) NID(4)=1
      WRITE(IO4,253)NR,NT,NCO,NNN,(NID(MN),MN=1,4)
      WRITE(IO2,253)NR,NT,NCO,NNN,(NID(MN),MN=1,4)
  253 FORMAT(5HDATA ,9I5)
      WRITE(IO4,254)NY,NQ,NCS,(NAU(I),I=1,14),(NREF(I),I=1,10)
      WRITE(IO2,254)NY,NQ,NCS,(NAU(I),I=1,14),(NREF(I),I=1,10)
  254 FORMAT(3I5,14A2,8X,10A2)
      IF(NT .EQ. 2 .OR. NT .EQ. 4) GO TO 81
      IF(NT .EQ. 8 .OR. NT .EQ. 9) GO TO 81
C
C      normalization uncertainties
C
      WRITE(IO4,261)(ENF(L),L=1,10),(NENF(L),L=1,10)
      WRITE(IO2,261)(ENF(L),L=1,10),(NENF(L),L=1,10)
   81 CONTINUE
  261 FORMAT(10F5.1,10I3)
C
C      energy dep. unc. parameters
C
      WRITE(IO2,262)((EPA(L,K),L=1,3),NETG(K),K=1,11)
      WRITE(IO4,262)((EPA(L,K),L=1,3),NETG(K),K=1,11)
  262 FORMAT(3F5.2,I3)
      IF(NCS .EQ. 0) GO TO 82
C
C      cross correlations
C
      DO 83 K=1,NCS
      WRITE(IO4,263)NCST(K),((NEC(M,L,K),M=1,2),L=1,10)
      WRITE(IO2,263)NCST(K),((NEC(M,L,K),M=1,2),L=1,10)
  293 FORMAT(10F5.1)
      WRITE(IO4,293)(FCFC(L,K),L=1,10)
      WRITE(IO2,293)(FCFC(L,K),L=1,10)
   83 CONTINUE
   82 CONTINUE
  263 FORMAT(I5,20I3)
C
      IF(NT .EQ. 6) GO TO 1003
C
C       GET GRID VALUES  - try at all apriori energies to find data
C
      write(io2,5173)
 5173 format(/' ENERGY/MEV  VALUE       UNCERTAINTIES               
     1  RATIO TO APRIORI'/)
      DO 40 L=2,mxm1
      E1=(EQ(L-1)+EQ(L))/2.
      E2=(EQ(L)+EQ(L+1))/2.
      E11=0.6*EQ(L)
      E22=1.4*EQ(L)
      IF(E1 .LT. E11) E1=E11
      IF(E2 .GT. E22) E2=E22
C
      AV=0.
      WTS=0.
      NKOT=0
      DO 133 N=1,12
  133 F( N,MAXF)=0.
      IF(E1 .GT. .03) INT=1
C
C   INTERPOLATION CONST.
C
      GO TO (88,89),INT
C
C  LIN LIN
C
   88 CONTINUE
      AL=(Q(L-1)-Q(L))/(EQ(L-1)-EQ(L))
      BL=Q(L)-AL*EQ(L)
      AR=(Q(L)-Q(L+1))/(EQ(L)-EQ(L+1))
      BR=Q(L)-AR*EQ(L)
      GO TO 28
C
C  LOG LOG
C
   89 QBL=(ALOG(Q(L-1))-ALOG(Q(L)))/(ALOG(EQ(L))-ALOG(EQ(L-1)))
      QAL=Q(L)*(EQ(L)**QBL)
      QBR=(ALOG(Q(L))-ALOG(Q(L+1)))/(ALOG(EQ(L+1))-ALOG(EQ(L)))
      QAR=Q(L)*(EQ(L)**QBR)
   28 CONTINUE
C
C   GRID VALUES
C
      DO 35 K=1,NO
      IF(E(K) .LT. E1 .OR. E(K) .GE. E2) GO TO 91
      WT=1./F(12,K)
      WT=WT*WT
      IF(E(K) .GT. EQ(L)) GO TO 32
      IF(E(K) .EQ. EQ(L)) GO TO 33
C
C      left of energy grid point
C
      GO TO (36,37),INT
   36 ADD=AL*E(K)+BL
      AD=S(K)+Q(L)-ADD
      GO TO 34
   37 ADD=QAL/(E(K)**QBL)
      AD=S(K)*Q(L)/ADD
      GO TO 34
C
C      right of energy grid point
C
   32 CONTINUE
      GO TO (51,52),INT
   51 CONTINUE
      ADD= AR*E(K)+BR
      AD=S(K)+Q(L)-ADD
      GO TO 34
   52 ADD=QAR/(E(K)**QBR)
      AD=S(K)*Q(L)/ADD
      GO TO 34
C
C      same energy as grid point
C
   33 AD=S(K)
   34 CONTINUE
C
C    check if difference is within requested limit of ULI*sigma
C
      IF(ULI .EQ. 0.0) GO TO 510
      T1X=100.*(AD-Q(L))/Q(L)
      T2X=T1X*T1X
      TEST=SQRT(WT*T2X)
      IF(TEST .LT. ULI) GO TO 510
      F33=F(3,K)*F(3,K)
      F44=1./WT-F33
      FNEW=T2X/(ULI*ULI)
      F33N=FNEW-F44
      F(12,K)=SQRT(FNEW)
      F(3,K)=SQRT(F33N)
      WT=1./FNEW
      WRITE(IO2,511) ULI,TEST
  511 FORMAT(20X,' VALUE OUTSIDE ',F5.2,' SIGMA BY ',F10.2)
  510 CONTINUE
      AV=AV+AD*WT
      WTS=WTS+WT
C
C      statistical uncertainty reduces if more than one value contributes,
C      average for all other uncertainties
C
      DO 38 M=1,11
      IF(NETG(M) .EQ. 9) GO TO 53
      F(M,MAXF)=F(M,MAXF)+F(M,K)
      GO TO 54
   53 CONTINUE
      IF(F(M,K) .EQ. 0.) GO TO 54
      F(M,MAXF)=F(M,MAXF)+(1./F(M,K))**2
   54 CONTINUE
   38 CONTINUE
      NKOT=NKOT+1
   91 CONTINUE
   35 CONTINUE
      AKOT=NKOT
      IF(AV .EQ. 0.) GO TO 42
C
C  GRID VALUE AND OUT
C
      EEE=EQ(L)
      QQQ=AV/WTS
      DIF=QQQ/Q(L)
      DO 39 N=1,11
      IF(NETG(N) .EQ. 9) GO TO 44
      F(N,MAXF)=F(N,MAXF)/AKOT
      GO TO 46
   44 CONTINUE
      IF(F(N,MAXF) .LE. 0.) GO TO 47
      F(N,MAXF)=1./SQRT(F(N,MAXF))
      GO TO 46
   47 F(N,MAXF)=0.
   46 CONTINUE
   39 CONTINUE
C
C  OUTPUT
C
      WRITE(IO4,200)EEE,QQQ,(F(N,MAXF),N=1,12)
      WRITE(IO2,290)EEE,QQQ,(F(N,MAXF),N=1,12),DIF
  200 FORMAT(2E10.4,12F5.1)
  290 FORMAT(2E10.4,12F5.1,F7.3)
   42 CONTINUE
   40 CONTINUE
C
C      end of data set
C
      WRITE(IO4,200) W,W,(F(N,MAXF),N=1,12)
      WRITE(IO2,200) W,W,(F(N,MAXF),N=1,12)
      IF(NCO .EQ. 0) GO TO 500
      DO 6113 KL=1,NCO
      WRITE(IO2,6114) (ECOR(KL,ML),ML=1,KL)
      WRITE(IO4,6115) (ECOR(KL,ML),ML=1,KL)
 6113 CONTINUE
 6114 FORMAT(1X,10F8.5)
 6115 FORMAT(10F8.5)
      GO TO 500
C
C      fission spectrum average data set
C
 1003 WRITE(IO2,200)E(1),S(1),(F(N,1),N=1,12)
      WRITE(IO4,200)E(1),S(1),(F(N,1),N=1,12)
      WRITE(IO2,200) W,W,(F(N,MAXF),N=1,12)
      WRITE(IO4,200) W,W,(F(N,MAXF),N=1,12)
      GO TO 500
C
C      DATA FILE COMPLETE
C
  160 WRITE(IO4,250) NQMM,NQMM
      WRITE(IO2,250) NQMM,NQMM
      WRITE(IO4,256) NQMM,NQMM
      WRITE(IO2,256) NQMM,NQMM
  256 FORMAT(4HEND*,1X,2I5)
      END FILE IO4
      STOP
      END
      SUBROUTINE DATRCL(NZ, NAB,IBZ)
C
C
C     THIS SUBROUTINE READS GMA DATA FILES
C
      character*2 NAU,NREF,NQT,NCOM,NQQ,NXQT,NXAU,ICC,NES,NEB
C
      Common /g/ NXQT(1000,12),NXAU(1000,14),NXY(1000)
C
      common /char/ NCOM(40,50),NQT(12),NAU(14),NREF(10)
C
      COMMON /main/ NR,NY,NQ,NT,NCO,NCS,NO,NZXCV(2),ENF(10),
     1 NENF(10),NID(5),NETG(11),EPA(3,11),NCST(10),NEC(2,10,10),E(400),
     2 S(400),F(12,400),ECOR(100,100),ID3,NCCO,
     3 IZZ,FCFC(10,10),INTR(5),EINTR(5,2),NQQ
C
      ICC= 'C '
      NES= 'ES'
      NEB= 'EB'
C
C
C  NZ=1  READ NEXT SET
C     2  READ AND COPY  TO SET NAB
C     3  READ AND COPY AND LIST  TO SET NAB
C     4  READ AND LIST TO SET NAB
C     5  READ AND LIST LIBRARY
C     6  WRITE  PRESENT SET
C     7  WRITE AND LIST  PRESENT SET
C     8  LIST PRESENT SET
C     9  SEARCH FOR SET NAB
C
C
C     ID3   DATA FILE INPUT DN
C
C
C  NR     DATA SET NUMBER
C  NY     YEAR OF MEARUREMENT
C  NQ     SET TAG
C  NT     TYPE OF QUANTITY
C  NCO    CORRELATION MATRIX SOURCE
C  NCS    CORRELATIONS WITH PRECEEDING SETS ARE GIVEN
C  NO     NUMBER OF DATA POINTS
C  NQT    QUANTITY
C  NAU    AUTHORS
C  NREF   REFERENCE
C  ENF    NORMALIZATION ERROR COMPONENTS
C  NENF   NORMALIZATION ERROR COMPONENTS TAGS
C  NID    QUANTITIES INVOLVED IN NT
C  NETG   ERROR COMPONENTS TAGS
C  EPA    ERROR CORRELATION PARAMETERS
C  NCST   DATA SET NUMBERS WITH WHICH PRESENT SET IS CORRELATED
C  NEC    ERROR COMPONENT PAIRS WHICH ARE CORRELATED
C  NCOM   COMMENTS
C  E      ENERGY OF DATA POINT
C  S      VALUE OF DATA POINT
C  F      SEQUENCE OF E-UNCERTAINTY, RESOLUTION, STATISTICAL ERROR, AND
C         SYSTEMATIC UNCERTAINTIES
C
C
      IF(NZ .EQ. 5) GO TO 5030
      GO TO 5031
 5030 DO 5032 K=1,1000
 5032 NXY(K)=0
 5031 CONTINUE
      NBQZ=1
      IF(IBZ .EQ. 1) NQQ=NES
      IF(IBZ .EQ. 2) NQQ=NEB
   49 CONTINUE
C
C***********INPUT**************************************************************
C
C
C     DATA SET IDENTIFICATION
C
   10 READ( ID3,100) NR,NY,(NQT(L),L=1,12),(NAU(K),K=1,14),(NREF(I),I=1,
     1 10)
      IF (NR .EQ. 0) GO TO 10
      IF(NR .EQ. 9999) return
  100 FORMAT(  2I4,12A2,14A2,10A2)
      READ( ID3,103) NQ,NT,NCO,NCS,NCCO,NO,(NID(L),L=1,5)
  103 FORMAT(4I2,I3,I5,5I3)
C
C      COMMENTS
C
      DO 17 K=1,NCCO
      READ( ID3,106)     (NCOM(L,K),L=1,40)
   17 CONTINUE
  106 FORMAT(40A2)
C
C      NORMALIZATION UNCERTAINTIES
C
      IF(NT .EQ. 2 .OR. NT .EQ. 4) GO TO 20
      IF(NT .EQ. 8 .OR. NT .EQ. 9) GO TO 20
      READ( ID3,107)(ENF(L),L=1,10),(NENF(L),L=1,10)
  107 FORMAT(10F5.1,10I3)
      SES=0.
      DO 871 K=1,10
  871 SES=SES+ENF(K)*ENF(K)
C
C      ENERGY DEPENDENT UNCERTAINTY  CORRELATIONS PARAMETERS AND TAGS
C
   20 READ (ID3,110)((EPA(L,K),L=1,3),K=1,11)
  110 FORMAT(3F5.2)
      do 2 k=1,11
      absum=epa(1,k)+epa(2,k)
      if(absum .gt. 1.0)epa(2,k)=1.0-epa(1,k)
    2 continue
      READ(ID3,111) (NETG(K),K=1,11)
  111 FORMAT(11I3)
C
C           DATA
C
      DO 32 K=1,NO
      READ(ID3,114) E(K),S(K),(F(L,K),L=1,12)
      SSS=0.
      DO 872 M=3,11
  872 SSS=SSS+F(M,K)*F(M,K)
       F(12,K)=SQRT(SES+SSS)
  114 FORMAT(2E10.4,12F5.1)
   32 CONTINUE
C
C     CORRELATIONS WITH PRECEEDING DATA SETS
C
      IF(NCS .EQ. 0) GO TO 29
      DO 36 K=1,NCS
      READ(ID3,116) NCST(K),(NEC(1,L,K),NEC(2,L,K),L=1,10)
  116 FORMAT(I5,20I2)
      READ(ID3,452)(FCFC(L,K),L=1,10)
  452 FORMAT(10F5.1)
      do 3 ji=1,10
      if(fcfc(ji,k) .gt. 1.0) fcfc(ji,k)=1.0
      if(fcfc(ji,k) .lt. -1.0) fcfc(ji,k)=-1.0
    3 continue
   36 CONTINUE
   29 CONTINUE
C
C     CORRELATION MATRIX INPUT
C
      IF(NCO .EQ. 0) GO TO 94
      DO 1113 L=1,NCO
 1113 READ(ID3,117)(ECOR(L,K),K=1,L  )
  117 FORMAT(10F8.5)
   94 CONTINUE
      READ(ID3,118) NQQ
  118 FORMAT(A2)
      RETURN
      END

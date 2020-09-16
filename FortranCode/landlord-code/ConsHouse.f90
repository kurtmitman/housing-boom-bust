!  ConsHouse.f90
!
!  FUNCTIONS:
!
!

!****************************************************************************
!
!  PROGRAM: ConsHouse
!
!  PURPOSE:  Consumption and House Prices - KMV
!
!  VERSION: 1.0, 27-May-2013
!
!  LAST EDITED BY: Kurt
!****************************************************************************

PROGRAM ConsHouse

  USE omp_lib
  USE params
  USE globals
  USE procedures
  USE funcs



  IMPLICIT NONE


  DOUBLE PRECISION, EXTERNAL :: FnXSdemand
  INTEGER::count,iter,iAgg,iAggp,mc,mcc,iter1,exuberance,bc,lc,hc
  DOUBLE PRECISION::step,lb,Hsupply,lwidth
  DOUBLE PRECISION::adiff,alamb,alamb2,exuval,bdiff,blamb,blamb2
  INTEGER         :: iseed(2),isize
  double precision    :: PhPr(2)
  double precision :: nunu,uChi_c,uChi_h
  integer::iprint,nPhPr
  double precision::ftol,scale
  double precision::Phpmat_old(ngpPh,ngpAgg,ngpAgg)
  double precision::Prpmat_old(ngpPh,ngpAgg,ngpAgg)

  IF(readparameters .EQ. 1) THEN
     count = nargs()
     IF (count<2) THEN
        WRITE(*,*) "need input parameter file name as argument if ReadParameters==1"
        STOP
     ELSE
        CALL getarg(1,InputParamFile)
     ENDIF
  ENDIF
  ! Variables
  iprint=1

  scale=0.01d0
  ftol=1.0d-8
  nPhPr=2


  CALL SetParameters
  CALL AllocateArrays
  CALL AggGrids

  giZh=ngpZh
  step=(0.425d0)/(ngpPhh-1.0d0)
  lb=0.425d0
  DO giPhh = 1, ngpPhh
     GridPh(giPhh)=lb+(giPhh-1.0d0)*step
  ENDDO
  step=(0.15d0)/(ngpPhr-1.0d0)
  lb=0.03d0
  DO giPhr = 1, ngpPhr
     GridPrPh(giPhr)=lb+(giPhr-1.0d0)*step
  ENDDO

  DO giPhh = 1, ngpPhh
     DO giPhr = 1, ngpPhr
        giPh = (giPhh-1)*ngpPhr + giPhr
        PhGrid(giPh)=GridPh(giPhh)
        PrGrid(giPh)=GridPh(giPhh)*GridPrPh(giPhr)
        PrPhGrid(giPh)=GridPrPh(giPhr)
        Trans(giPh)=PhGrid(giPh)*(1.0d0-alpha_h)*((PhGrid(giPh))**(alpha_h/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
     ENDDO
  ENDDO

  PRINT*,'Pr Grid'
  print*,PrGrid
  print*,'PrPhGrid'
  print*,PrPhGrid
  print*,'PhGrid'
  print*,PhGrid

  CALL Grids
  PRINT*,'Pension points'
  PRINT*,pgrid(Jwork+1,:)
  !    initdistsso(1,1,29,2,:)=0.30d0*zydist(1,:)

  !	call SetUpTimeVariables
  DO giAgg=1,ngpAgg
     PRINT*,LTVGrid(giAgg),LTIGrid(giAgg),HELOCGrid(giAgg),FCGrid(giAgg)
  ENDDO
  PRINT*,GSE,GridC


  adiff=1.0d0
  bdiff=1.0d0
  count=1
  iseed(1) = 5432
  iseed(2) = 6543
  isize = 2


  CALL RANDOM_SEED(size = isize)
  CALL RANDOM_SEED(put = iseed)


  CALL RANDOM_NUMBER(zyrand)
  CALL RANDOM_NUMBER(eyrand)
  CALL RANDOM_NUMBER(phrand)
  CALL RANDOM_NUMBER(thetarand)
  exurent=0
  CALL RANDOM_NUMBER(poprand)

  PRINT*,initadist(:,2)


  ssph=0.50d0

  aa0p=aa0
  aa1p=aa1
  aa2p=aa2
  alamb=0.2d0
  alamb2=0.15d0
  bb0p=bb0
  bb1p=bb1
  bb2p=bb2
  blamb=0.2d0
  blamb2=0.05d0


  step=(MAXVAL(ssph)-MINVAL(ssph)+0.6d0)/(ngpPh-1.0d0)
  lb=MINVAL(ssph)-0.2d0
  giZh=ngpZh

     exuberance=0

     adiff=1.0d0
     iter1=0
     PRINT*,ssph
     PRINT*,'Steady state complete'
     PRINT*,'Bzerosim: ',GridBsim(bzerosim)





     PRINT*,aa0
     PRINT*,aa1
     Hsteady=0.7d0 !1.7d0 !2.0d0
     alamb=0.025d0 !0.1d0
     !Ht=Hsteady
     distsso=0.0d0
     distssr=0.0d0
     DO gij=1,Jwork
        distssr(bzerosim,:,gij)=zydist(gij,:)
     ENDDO
     DO gij=Jwork+1,Jtot
        distssr(bzerosim,:,gij)=zydist(Jwork,:)
     ENDDO
     IF(OnlyBankBelief .EQ. 1) THEN
        OPEN(3, FILE = TRIM(OutputDir) // 'qm.txt')
        DO gij=1,Jwork+1
           DO giPh=1,ngpPh
              DO giAgg=1,ngpAgg
                 DO giW=1,ngpW
                    DO hc=1,nh
                       DO mc=1,nm
                          DO lc=1,nl
                             DO bc=1,nb
                                READ(3,*) qm_e_exo(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij)
                             ENDDO
                          ENDDO
                       ENDDO
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO


        CLOSE(3)

        OPEN(3, FILE = TRIM(OutputDir) // 'qmret.txt')
        DO gij=1,Jret
           DO giPh=1,ngpPh
              DO giAgg=1,ngpAgg
                 DO giR=1,ngpR
                    DO hc=1,nh
                       DO mc=1,nm
                          DO lc=1,nl
                             DO bc=1,nb
                                READ(3,*) qmret_e_exo(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij)
                             ENDDO
                          ENDDO
                       ENDDO
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
        CLOSE(3)
     ENDIF



     Phpmat = 0.0d0
     Phpmat_old = 1.0d0
     Prpmat = 0.0d0
     Prpmat_old = 1.0d0


     DO WHILE(adiff .GT. 5.0d-3 .AND. iter1 .LT. 50 .AND. bdiff .GT. 5.0d-3 )
        iter1=iter1+1
        DoIRF=0
        DoExu=0
        IF(iter1 .EQ. 10) alamb=0.15d0 !0.05d0
        EVown=0.0d0
        Evrent=0.0d0
        VownRet=0.0d0
        VrentRet=0.0d0
        Vown=0.0d0
        Vrent=0.0d0
        IF(adiff .LT. 1.0d0) THEN
           aa0=alamb*aa0p+(1.0d0-alamb)*aa0
           aa1=alamb*aa1p+(1.0d0-alamb)*aa1
           aa2=alamb*aa2p+(1.0d0-alamb)*aa2
        ELSE
           aa0=alamb2*aa0p+(1.0d0-alamb2)*aa0
           aa1=alamb2*aa1p+(1.0d0-alamb2)*aa1
           aa2=alamb2*aa2p+(1.0d0-alamb2)*aa2
        ENDIF
        IF(bdiff .LT. 1.0d0) THEN
           bb0=blamb*bb0p+(1.0d0-blamb)*bb0
           bb1=blamb*bb1p+(1.0d0-blamb)*bb1
           bb2=blamb*bb2p+(1.0d0-blamb)*bb2
        ELSE
           bb0=blamb2*bb0p+(1.0d0-blamb2)*bb0
           bb1=blamb2*bb1p+(1.0d0-blamb2)*bb1
           bb2=blamb2*bb2p+(1.0d0-blamb2)*bb2
        ENDIF


        Phpmat_old = Phpmat
        Prpmat_old = Prpmat


        CALL ComputeLOM

        PRINT*,'Max price forecast error'
        PRINT*,maxval(abs(Phpmat_old-Phpmat))

        PRINT*,'Max rent forecast error'
        PRINT*,maxval(abs(Prpmat_old-Prpmat))


        CALL DecisionsRet
        CALL Decisions
        PRINT*,'Decisions Computed'

        !(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1)

        OPEN(3, FILE = TRIM(OutputDir) // 'creditscore.txt', STATUS = 'replace')
        DO gij=1,Jwork
           DO giW=1,ngpW
              WRITE(3,'(3E16.8)') REAL(gij),REAL(giW),qm_e(nbneg,1,13,3,(ngpAgg-1)*ngpW+giW,5,gij)
           ENDDO
        ENDDO
        IF(OnlyBankBelief .EQ. 0 .AND. NoBankBelief .EQ. 0 .AND. exogenous .EQ. 0) THEN
           OPEN(3, FILE = TRIM(OutputDir) // 'qm.txt', STATUS = 'replace')
           DO gij=1,Jwork+1
              DO giPh=1,ngpPh
                 DO giAgg=1,ngpAgg
                    DO giW=1,ngpW
                       DO hc=1,nh
                          DO mc=1,nm
                             DO lc=1,nl
                                DO bc=1,nb
                                   IF(qm_e(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij) .LT. 1.d-20) qm_e(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij)=0.0d0
                                   WRITE(3,'(1e25.15)') qm_e(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij)
                                ENDDO
                             ENDDO
                          ENDDO
                       ENDDO
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO


           CLOSE(3)

           OPEN(3, FILE = TRIM(OutputDir) // 'qmret.txt', STATUS = 'replace')
           DO gij=1,Jret
              DO giPh=1,ngpPh
                 DO giAgg=1,ngpAgg
                    DO giR=1,ngpR
                       DO hc=1,nh
                          DO mc=1,nm
                             DO lc=1,nl
                                DO bc=1,nb
                                   IF(qmret_e(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij) .LT. 1.d-20) qmret_e(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij)=0.0d0
                                   WRITE(3,'(1e25.15)') qmret_e(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij)
                                ENDDO
                             ENDDO
                          ENDDO
                       ENDDO
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
           CLOSE(3)
        ENDIF

        IF(1 .eq. 0) THEN
        OPEN(3, FILE = TRIM(OutputDir) // 'qm1.txt', STATUS = 'replace')
        CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,1,:,:,5))
        OPEN(3, FILE = TRIM(OutputDir) // 'qm2.txt', STATUS = 'replace')
        CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,2,:,:,5))
        OPEN(3, FILE = TRIM(OutputDir) // 'qm3.txt', STATUS = 'replace')
        CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,3,:,:,5))
        OPEN(3, FILE = TRIM(OutputDir) // 'qm4.txt', STATUS = 'replace')
        CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,4,:,:,5))
        OPEN(3, FILE = TRIM(OutputDir) // 'qm5.txt', STATUS = 'replace')
        CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,5,:,:,5))
        !exit
        ENDIF
        IF(iter1 .EQ. 0) THEN

           giPh=ngpPh/2+1
           giAgg=ngpAgg
           giAy=AtoY(giAgg)
           giZh=AtoZ(giAgg)
           giC=AtoC(giAgg)
           CALL SimulateSteadyDist
        ENDIF


        IF(iter .GE. 20) DoIRF=1
        DoIRF=1
        Modify=0
        IF(Modify .EQ. 1) DoIRF = 1
        IF(exogenous .EQ. 1) DoIRF = 1
        tstart=1
        tend=Tsim
        CALL SimulateStochGEDist


        IF(Modify .EQ. 0) THEN
           CALL updateforecast
           PRINT*,aa0p
           PRINT*,aa1p
           !pause

           adiff=MAX(MAXVAL(ABS(aa0-aa0p)),MAXVAL(ABS(aa1-aa1p)),MAXVAL(ABS(aa2-aa2p)))
           bdiff=MAX(MAXVAL(ABS(bb0-bb0p)),MAXVAL(ABS(bb1-bb1p)),MAXVAL(ABS(bb2-bb2p)))
           PRINT*,count,adiff,bdiff
           count=count+1
        ENDIF


        OPEN(3, FILE = TRIM(OutputDir) // 'a0.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa0)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'a1.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa1)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'a2.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa2)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'b0.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,bb0)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'b1.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,bb1)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'b2.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,bb2)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'b0p.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,bb0p)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'b1p.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,bb1p)
        CLOSE(3)
        OPEN(3, FILE = TRIM(OutputDir) // 'b2p.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,bb2p)
        CLOSE(3)


        CALL SaveOutput


        IF(Modify .EQ. 1) iter1=1000
        IF(DoSims .EQ. 1) iter1=1000
        IF(exogenous .EQ. 1) iter1=1000
        !       iter1=1000
     ENDDO


   END PROGRAM ConsHouse

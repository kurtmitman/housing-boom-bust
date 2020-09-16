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
!  VERSION: Final, 26-Jan-2020
!
!  LAST EDITED BY: Kurt
!****************************************************************************

    program ConsHouse

    use omp_lib
    use params
    use globals
    use procedures
    use funcs

    implicit none

    integer::count,iter,iAgg,iAggp,mc,mcc,iter1,exuberance,bc,lc,hc
    double precision::step,lb,Hsupply,lwidth
    double precision::adiff,alamb,alamb2,exuval
    INTEGER         :: iseed(2),isize
    if(readparameters .eq. 1) then
        count = nargs()
        IF (count<2) THEN
		    write(*,*) "need input parameter file name as argument if ReadParameters==1"
		stop
	    else
		    call getarg(1,InputParamFile)
	    endif
    endif

    DoIRF=1
    Modify=0



    ! Variables
    call SetParameters
    call AllocateArrays
    Call AggGrids

    step=(0.6d0)/(ngpPh-1.0d0)
    lb=0.30d0
    giZh=ngpZh
    do giPh=1,ngpPh
       PhGrid(giPh)=lb+(giPh-1.0d0)*step
       Trans(giPh)=PhGrid(giPh)*(1.0d0-alpha_h)*((PhGrid(giPh))**(alpha_h/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
    enddo

    call Grids

    adiff=1.0d0
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

    print*,initadist(:,2)

    ssph=0.50d0


    aa0p=aa0
    aa1p=aa1
    alamb=0.025d0
    alamb2=0.05d0

    step=(maxval(ssph)-minval(ssph)+0.6d0)/(ngpPh-1.0d0)
    lb=minval(ssph)-0.2d0
    giZh=ngpZh
    do giPh=1,ngpPh
       PhGrid(giPh)=lb+(giPh-1.0d0)*step
       Trans(giPh)=PhGrid(giPh)*(1.0d0-alpha_h)*((PhGrid(giPh))**(alpha_h/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
    enddo
    exuberance=0

    adiff=1.0d0
    iter1=0


    print*,aa0
    print*,aa1
    Hsteady=2.0d0
    alamb=0.025d0
    distsso=0.0d0
    distssr=0.0d0
    do gij=1,Jwork
       distssr(bzerosim,:,gij)=zydist(gij,:)
    enddo
    do gij=Jwork+1,Jtot
       distssr(bzerosim,:,gij)=zydist(Jwork,:)
    enddo
    if(OnlyBankBelief .eq. 1) THEN
      OPEN(3, FILE = trim(OutputDir) // 'qm.txt')
      do gij=1,Jwork+1
         do giPh=1,ngpPh
            do giAgg=1,ngpAgg
               do giW=1,ngpW
                  do hc=1,nh
                     do mc=1,nm
                        do lc=1,nl
                           do bc=1,nb
                              READ(3,*) qm_e_exo(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo


      CLOSE(3)

      OPEN(3, FILE = trim(OutputDir) // 'qmret.txt')
      do gij=1,Jret
         do giPh=1,ngpPh
            do giAgg=1,ngpAgg
               do giR=1,ngpR
                  do hc=1,nh
                     do mc=1,nm
                        do lc=1,nl
                           do bc=1,nb
                              READ(3,*) qmret_e_exo(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij)
                           enddo
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo
      CLOSE(3)
    ENDIF


    do while (adiff .gt. 5.0d-3)
       iter1 = iter1 + 1



       DoIRF=0
       DoExu=0
       EVown=0.0d0
       Evrent=0.0d0
       VownRet=0.0d0
       VrentRet=0.0d0
       Vown=0.0d0
       Vrent=0.0d0

       if(adiff .lt. 1.0d0) then
          aa0=alamb*aa0p+(1.0d0-alamb)*aa0
          aa1=alamb*aa1p+(1.0d0-alamb)*aa1
       else
          aa0=alamb2*aa0p+(1.0d0-alamb2)*aa0
          aa1=alamb2*aa1p+(1.0d0-alamb2)*aa1
       endif


       call ComputeLOM
       call DecisionsRet
       call Decisions
       print*,'Decisions Computed'

       OPEN(3, FILE = trim(OutputDir) // 'creditscore.txt', STATUS = 'replace')
       do gij=1,Jwork
          do giW=1,ngpW
             write(3,'(3E16.8)') real(gij),real(giW),qm_e(nbneg,1,13,3,(ngpAgg-1)*ngpW+giW,5,gij)
          enddo
       enddo
       if(OnlyBankBelief .eq. 0 .and. NoBankBelief .eq. 0 .and. exogenous .eq. 0) THEN
          OPEN(3, FILE = trim(OutputDir) // 'qm.txt', STATUS = 'replace')
          do gij=1,Jwork+1
             do giPh=1,ngpPh
                do giAgg=1,ngpAgg
                   do giW=1,ngpW
                      do hc=1,nh
                         do mc=1,nm
                            do lc=1,nl
                               do bc=1,nb
                                  if(qm_e(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij) .lt. 1.d-20) qm_e(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij)=0.0d0
                                  WRITE(3,'(1e25.15)') qm_e(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,giPh,gij)
                               enddo
                            enddo
                         enddo
                      enddo
                   enddo
                enddo
             enddo
          enddo


          CLOSE(3)

          OPEN(3, FILE = trim(OutputDir) // 'qmret.txt', STATUS = 'replace')
          do gij=1,Jret
             do giPh=1,ngpPh
                do giAgg=1,ngpAgg
                   do giR=1,ngpR
                      do hc=1,nh
                         do mc=1,nm
                            do lc=1,nl
                               do bc=1,nb
                                  if(qmret_e(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij) .lt. 1.d-20) qmret_e(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij)=0.0d0
                                  WRITE(3,'(1e25.15)') qmret_e(bc,lc,mc,hc,(giAgg-1)*ngpR+giR,giPh,gij)
                               enddo
                            enddo
                         enddo
                      enddo
                   enddo
                enddo
             enddo
          enddo
          CLOSE(3)
      endif

       OPEN(3, FILE = trim(OutputDir) // 'qm1.txt', STATUS = 'replace')
       CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,1,:,:,5))
       OPEN(3, FILE = trim(OutputDir) // 'qm2.txt', STATUS = 'replace')
       CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,2,:,:,5))
       OPEN(3, FILE = trim(OutputDir) // 'qm3.txt', STATUS = 'replace')
       CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,3,:,:,5))
       OPEN(3, FILE = trim(OutputDir) // 'qm4.txt', STATUS = 'replace')
       CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,4,:,:,5))
       OPEN(3, FILE = trim(OutputDir) // 'qm5.txt', STATUS = 'replace')
       CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,5,:,:,5))


       if(Modify .eq. 1) DoIRF = 1
       if(exogenous .eq. 1) DoIRF = 1
       tstart=1
       tend=Tsim
       call SimulateStochGEDist


        if((Modify .eq. 1)) then
           tstart=60
           tend=450 !171

           if(DoSims .eq. 1) then

              call SimulateBPP
              print*,'BPP simulated'
              call ComputeBPPCoef
              OPEN(3, FILE = trim(OutputDir)// 'bppcoef.txt', STATUS = 'replace');WRITE(3,*) bppcoef;CLOSE(3)
           endif
           call SaveOutput
        elseif(Modify .eq. 0) then
          call updateforecast
          print*,aa0p
          print*,aa1p
          adiff=max(maxval(abs(aa0-aa0p)),maxval(abs(aa1-aa1p)))
          print*,count,adiff
          count=count+1
        endif

        OPEN(3, FILE = trim(OutputDir) // 'a0.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa0)
        CLOSE(3)
        OPEN(3, FILE = trim(OutputDir) // 'a1.txt', STATUS = 'replace')
        CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa1)
        CLOSE(3)

        call SaveOutput
        if(Modify .eq. 1) exit
        if(exogenous .eq. 1) exit
     enddo

    end program ConsHouse

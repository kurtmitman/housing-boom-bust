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

    program ConsHouse

    use omp_lib
    use params
    use globals
    use procedures
    use funcs

    implicit none

    integer::count,iter,iAgg,iAggp,mc,mcc,iter1,exuberance
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
    ! Set Modify = 1 for mortgage modification experiment
    Modify = 0
    DoIRF  = 1
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
    print*,'Pension points'
    print*,pgrid(Jwork+1,:)

!	call SetUpTimeVariables
    do giAgg=1,ngpAgg
       print*,LTVGrid(giAgg),LTIGrid(giAgg),HELOCGrid(giAgg),FCGrid(giAgg)
    enddo
    print*,GSE,GridC




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


    if(1 .eq. 0) then
    if(1 .eq. 1) then
       step=0.05d0
       lb=0.25d0

       do giPh=1,ngpPh
          PhGrid(giPh)=lb+(giPh-1.0d0)*step
          print*,PhGrid(giPh),lb,step
       enddo
       print*,PhGrid
       transMatAgg=0.0d0
       do iAgg=1,ngpAgg
          transMatAgg(iAgg,iAgg)=1.0d0
       enddo


       Call ComputeLOM
       print*,'Hi'
       print*,gridC
       print*,gridAy
       print*,gridRf
       print*,GridDemand
       print*,GridZh
       Call DecisionsRet
       Call Decisions
       giPh=1
       giAgg=ngpAgg
       giAy=AtoY(giAgg)
       giZh=AtoZ(giAgg)
       giC=AtoC(giAgg)


       do giPh=1,ngpPh
          call SimulateSteadyDist
          Hsupply = (((phgrid(giPh))**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h/GridAY(giAy))**(alpha_h/(1.0d0-alpha_h))))/deltah
          print*,phgrid(giph),Hsupply,sshdemand
          if(abs(Hsupply-sshdemand) .lt. 1.0d-4) exit
          if(sshdemand .lt. Hsupply) exit
       enddo

       ssph=phgrid(giPh)
       Hsteady=sshdemand
       print*,ssph
       print*,Hsteady
    else
       CALL ComputeSteady
    endif
 else
    ssph=0.50d0

 endif



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
    print*,ssph
    print*,'Steady state complete'
    print*,'Bzerosim: ',GridBsim(bzerosim)






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
    do while(adiff .gt. 5.0d-3 .and. iter1 .lt. 50)
       iter1=iter1+1
       DoIRF=0
       DoExu=0
       if(iter1 .eq. 10) alamb=0.15d0 !0.05d0
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
       !exit

       if(iter1 .eq. 0) then

          giPh=ngpPh/2+1
          giAgg=ngpAgg
          giAy=AtoY(giAgg)
          giZh=AtoZ(giAgg)
          giC=AtoC(giAgg)
          call SimulateSteadyDist
       endif


       if(iter .ge. 20) DoIRF=1
       DoIRF=0
       Modify=0
       if(Modify .eq. 1) DoIRF = 1

       tstart=1
       tend=Tsim
       call SimulateStochGEDist


       if(Modify .eq. 0) then
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


        if((iter1 .ge. 1) .or. (Modify .eq. 1)) then
           tstart=60
           tend=450

           if(DoSims .eq. 1) then

              call SimulateBPP
              print*,'BPP simulated'
              call ComputeBPPCoef
              OPEN(3, FILE = trim(OutputDir)// 'bppcoef.txt', STATUS = 'replace');WRITE(3,*) bppcoef;CLOSE(3)
           endif
           call SaveOutput

        endif

        if(Modify .eq. 1) iter1=1000
     enddo


    end program ConsHouse

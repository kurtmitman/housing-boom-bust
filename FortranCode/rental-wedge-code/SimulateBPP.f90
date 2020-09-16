
!
!   SimulateBPP.f90
!   Compute insurance coefficients a la Blundell, Pistaferri and Preston (2008)
!
!   Created by Kurt Mitman on 11/08/15.
subroutine SimulateBPP

use params
use globals
use funcs
use procedures

implicit none

INTEGER		:: iseed(2),isize,iAgg,it,Phinds(2),iindex


!Things that don't need to be private
double precision    :: Ph,Pr
double precision                    :: vftol
double precision    :: prandtemp
double precision    :: phptemp(ngpAgg),ephtemp
!Variables that need to be private
INTEGER		:: in,ij,ijret,iW,iR,polind,hjI,index,mcc,hcc,lcc,ljI,ip,ify,izy,iPh,mc
double precision    :: bccmax,ftemp1,ftemp2,bjps,coef1,coef2,coef3
integer     :: inds(2)
double precision    ::  vals(2)
double precision    :: helpWsell,helpWfore
double precision    :: HelpVals(5)
double precision    :: btemp, y, pm, fc
double precision    :: bj,bjp,mj,mjp,hj,lj
double precision    :: Wrentsim,consRpolsim,hconsRpolsim,bRpolsim
double precision    :: Wbuysim
double precision    :: Wrefisim,consNpolsim,hconsNpolsim,bNpolsim,mNpolsim,hNpolsim,lNpolsim
double precision    :: Wpaysim,consPpolsim,hconsPpolsim,bPpolsim,mPpolsim,hPpolsim,lPpolsim
double precision,dimension(nh*nm*nl)    :: helpWB,consB,bB,mPp
integer,dimension(nh*nm*nl) ::hB,mB,hconsB,lB
integer             :: hINpolsim,hIPpolsim,lINpolsim,lIPpolsim
double precision    :: mpcval,bjpmpc,bjmpc,bccmid,bccmin

double precision    :: phh,qmtemp,qltemp,qltemp2,RentVal,BuyVal,Phvals(2)

double precision    :: Ht,htrans

DOUBLE PRECISION, EXTERNAL :: FnVFR, golden, FnVFB, FnVFF, FnVFN,FnVFP,FnXSH
DOUBLE PRECISION, EXTERNAL          :: FnVFRRetJ, FnVFFRetJ, FnVFPRetJ
DOUBLE PRECISION, EXTERNAL          :: FnVFRRet, FnVFBRet, FnVFFRet, FnVFNRet,FnVFPRet

double precision,dimension(ngpPh)   ::Hdemand,Hsupply
double PRECISION, allocatable, dimension(:,:)::hsim,msim,lsim,bsim,hconssim,csim,beqsim
INTEGER, allocatable, dimension(:,:)::hsimI,buysim,ownsim,lsimI,refisim,foresim,sellsim
double precision     :: HInv(ngpAy,ngpZh,ngpPh)
double precision     :: Vmin,Wmin
integer              ::idsimI(nsim)
allocate(hsim(ngpPh,nsim))
allocate(msim(ngpPh,nsim))
allocate(lsim(ngpPh,nsim))
allocate(bsim(ngpPh,nsim))
allocate(hconssim(ngpPh,nsim))
allocate(csim(ngpPh,nsim))
allocate(beqsim(ngpPh,nsim))
allocate(hsimI(ngpPh,nsim))
allocate(lsimI(ngpPh,nsim))
allocate(buysim(ngpPh,nsim))
allocate(ownsim(ngpPh,nsim))
allocate(refisim(ngpPh,nsim))
allocate(foresim(ngpPh,nsim))
allocate(sellsim(ngpPh,nsim))



Vmin=-1.0d10
Wmin=Vmin

Ht=Hsteady
phh=ssph(iAggsteady)
print*,Hsteady,ssph(iAggsteady)
ownsim=0
hsim=0.0d0
hconssim=0.0d0
bsim=0.0d0




vftol=1.0d-5


!$OMP PARALLEL DO
DO in = 1,nsim

    idsimI(in) = mod(in-1,nsimss)+1
    simtrans(tstart)%jsim(in) = (in-1)/nsimss+1
    if(simtrans(tstart+1)%jsim(in) .LT. Jtot) then
       simtrans(tstart+1)%hsimI(in)=1
       simtrans(tstart+1)%lsimI(in)=1
       simtrans(tstart+1)%lsim(in)=0
       simtrans(tstart+1)%hsim(in)=0
       simtrans(tstart+1)%msim(in)=0
       simtrans(tstart+1)%bsim(in)=1.0d0
       simtrans(tstart+1)%ownsim(in)=0
    endif
    simtrans(tstart)%csim(in)=0.35d0
    simtrans(tstart)%hconssim(in)=1.0d0
    simtrans(tstart)%sellsim(in)=0
    simtrans(tstart)%refisim(in)=0
    simtrans(tstart)%foresim(in)=0
    simtrans(tstart)%buysim(in)=0
    fysimtransI(in)=fysimI(in)
    simtrans(tstart)%thetasim(in)=2

    simtrans(tstart)%zysimI(in)=max(zysimI(idsimI(in),simtrans(tstart)%jsim(in)),1)
    simtrans(tstart)%psimI(in) = (simtrans(tstart)%zysimI(in)-1)*ngpfy+fysimtransI(in)

END DO
!$OMP END PARALLEL DO



do it=tstart+1,tend-1

    iAgg=iAggpath(it)
    giAgg=iAgg
    AYpath(it)=GridAY(AtoY(iAgg))
    iAYpath(it)=AtoY(iAgg)
    HDpath(it)=GridDemand(AtoD(iAgg))
    iHDpath(it)=AtoD(iAgg)
    giHD=iHDpath(it)
    giAY=iAYpath(it)
    giZh=AtoZ(iAgg)
    giC=AtoC(iAgg)
    giRf = AtoR(giAgg)
    Ht=Htpath(it)
    Ph=Phpath(it)
    gPh=Ph
    giRf = AtoR(giAgg)
    !Changed 11/20/15 to make grid on Rf on Agg for comovement
    rf=RfGrid(giAgg)
    grf=rf
    rm=Rmgrid(giAgg)
    grm=rm
    rl=RlGrid(giAgg)
    grl=rl
    htrans=gPh*(1.0d0-alpha_h)*(gPh**(alpha_h/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))


    call basefun(Phgrid,ngpPh,Ph,Phvals,Phinds)

    !Simulate each guy

    !$OMP PARALLEL DO PRIVATE(ij,ijret,iW,iR,polind,hjI,index,mcc,mc,hcc,lcc,ljI,ip,ify,izy,bccmax,ftemp1,ftemp2,bjps,inds,vals,helpWsell,helpWfore,HelpVals,btemp, y, pm, fc,bj,bjp,mj,mjp,hj,lj,Wrentsim,consRpolsim,hconsRpolsim,bRpolsim,Wbuysim,Wrefisim,consNpolsim,hconsNpolsim,bNpolsim,mNpolsim,hNpolsim,lNpolsim,Wpaysim,consPpolsim,hconsPpolsim,bPpolsim,mPpolsim,hPpolsim,lPpolsim,helpWB,consB,bB,hB,mB,hconsB,lB,hINpolsim,hIPpolsim,lINpolsim,lIPpolsim,mPp,qmtemp,qltemp,qltemp2,RentVal,BuyVal,coef1,coef2,coef3,bjmpc,mpcval,bjpmpc,bccmin,bccmid) COPYIN(giAy,giHD,giAgg,giC,giZh,giPh,giRf, gPh,grl,grf,grm)

    do in=1,nsim

        IF(simtrans(it-1)%jsim(in) .eq. Jtot) THEN  !die with prob 1
            simtrans(it-1)%diesimI(in) = 1
            simtrans(it)%jsim(in) = 1
        ELSE
            simtrans(it-1)%diesimI(in) = 0
            simtrans(it)%jsim(in) = simtrans(it-1)%jsim(in) + 1
        END IF
        ij = simtrans(it)%jsim(in)
        ijret = ij-Jwork
        rf=grf
        rm=grm
        rl=grl
        !Use $500 to compute the MPC for households
        mpcval = 500.0d0/(DataAvAnnualEarns/Numeraire)

        simtrans(it)%eysimI(in)= 1

        select case(ij)

            case(1)
                CALL DiscreteDist1(simtrans(it)%zysimI(in), ngpzy, zydist(1,:),zyrand(in,it))
                simtrans(it)%ypssim(in) = ypsgrid(ij,fysimtransI(in),simtrans(it)%zysimI(in),giAy)
                simtrans(it)%psimI(in) = 1
                simtrans(it)%psim(in) = 0.0
                simtrans(it)%ysim(in) = ygrid(ij,fysimtransI(in),simtrans(it)%zysimI(in),simtrans(it)%eysimI(in),giAy)
                simtrans(it)%hsimI(in)=1
                simtrans(it)%lsimI(in)=1
                simtrans(it)%ownsim(in)=0
                simtrans(it)%bsim(in)=0
                simtrans(it)%hsim(in)=0
                simtrans(it)%lsim(in)=0
                simtrans(it)%msim(in)=0

            case(2:Jwork)
                CALL DiscreteDist1(simtrans(it)%zysimI(in), ngpzy, zytrans(ij-1,simtrans(it-1)%zysimI(in),:),zyrand(in,it))
                CALL DiscreteDist1(simtrans(it)%thetasim(in), ngptheta, (/thet(ij), 1.0d0-thet(ij) /), thetarand(in,it))
                simtrans(it)%ysim(in) = ygrid(ij,fysimtransI(in),simtrans(it)%zysimI(in),simtrans(it)%eysimI(in),giAy)
                simtrans(it)%ypssim(in) = ypsgrid(ij,fysimtransI(in),simtrans(it)%zysimI(in),giAy)
                simtrans(it)%psimI(in) = 1
                simtrans(it)%psim(in) = 0.0d0

            case(Jwork+1)
               simtrans(it)%psimI(in) = (simtrans(it-1)%zysimI(in)-1)*ngpfy+fysimtransI(in)
               simtrans(it)%psim(in) = pgrid(ij,simtrans(it)%psimI(in))
               simtrans(it)%ysim(in) = 0.0d0
               simtrans(it)%ypssim(in) = 0.0d0

            case(Jwork+2:Jtot)
               simtrans(it)%ysim(in) = 0.0d0
               simtrans(it)%ypssim(in) = 0.0d0
               simtrans(it)%psimI(in) = simtrans(it-1)%psimI(in)
               simtrans(it)%psim(in) = pgrid(ij,simtrans(it)%psimI(in))

        end select

        ip = simtrans(it)%psimI(in)
        ify = fysimtransI(in)
        izy = simtrans(it)%zysimI(in)
        bj=simtrans(it)%bsim(in)

        !Increase cah by $500 to calculate the MPC
        bjmpc=bj+mpcval

        select case(ij)

            case(1:Jwork)

                gij=ij

                iW=Wind(ify,izy)
                giW=iW
                y=simtrans(it)%ysim(in)
                giExo=(giAgg-1)*ngpW+giW



                !Check if comes into period with house
                if(simtrans(it)%ownsim(in) .eq. 0) then


                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wrent(:,giExo,:,gij),bj,gPh,RentVal)
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wbuy(:,giExo,:,gij),bj,gPh,BuyVal)

                    !Check to see if stay renter or buy house
                    if( RentVal .gt. BuyVal  ) then


                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,bRpol(:,giExo,:,gij),bj,gPh,simtrans(it+1)%bsim(in))
                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpol(:,giExo,:,gij),bj,gPh,simtrans(it)%csim(in))
                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,hconsRpol(:,giExo,:,gij),bj,gPh,simtrans(it+1)%hconssim(in))

                        simtrans(it)%buysim(in)=0
                        simtrans(it+1)%ownsim(in)=0
                        simtrans(it+1)%hsim(in)=0.0d0
                        simtrans(it+1)%msim(in)=0.0d0
                        simtrans(it+1)%lsim(in)=0.0d0
                        simtrans(it)%refisim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%sellsim(in)=0
                        simtrans(it+1)%hsimI(in)=1
                        simtrans(it+1)%lsimI(in)=1

                    else
                        simtrans(it)%buysim(in)=1
                        simtrans(it+1)%ownsim(in)=1
                        simtrans(it)%refisim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%sellsim(in)=0

                        EVown(:,:,:,:,gij)=Phvals(1)*EEVown(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVown(:,:,:,:,gij,giExo,Phinds(2))

                        do hcc=1,nh
                            fc=0.0d0
                            do mcc=1,nm
                                do lcc=1,nl
                                    index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                    mB(index)=mcc
                                    hB(index)=hcc
                                    lB(index)=lcc
                                    gih=hcc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gmort=GridM(gim)
                                    gloc=GridL(gil)
                                    gliq=bj+y-FnTax(y)-gPh*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans


                                    bccmax=(gliq+gmort*ghouse)*(1.0d0+rf)
                                    !Changed 10/13/15 to prevent new home buyers from using HELOCs for purchase
                                    if(giC .eq. ngpC) then
                                       bccmin=0.0d0
                                    elseif(HELOCVal .eq. 1) then
                                       bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                    else
                                       bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                    endif


                                    bccmid=(bccmax+bccmin)/2.0d0

                                    if(bccmax .gt. bccmin) then

                                        ftemp1=FnVFB(0.0d0)
                                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)
                                        if(ftemp2<ftemp1) then
                                            ftemp1=ftemp2
                                        else
                                            btemp=0.0d0
                                        endif
                                    else
                                       helpWB(index)=Wmin
                                       btemp=0.0d0
                                       consB(index)=cmin
                                    endif

                                    bB(index)=btemp
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)
                                    consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                    helpWB(index)=-ftemp1
                                enddo
                                fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                            enddo
                        enddo
                        polind=maxloc(helpWB,dim=1)

                        simtrans(it+1)%hsim(in)=GridH(hB(polind))
                        simtrans(it+1)%hsimI(in)=hB(polind)
                        simtrans(it+1)%msim(in)=GridM(mB(polind))
                        simtrans(it+1)%bsim(in)=bB(polind)
                        simtrans(it+1)%lsim(in)=GridL(lB(polind))
                        simtrans(it+1)%lsimI(in)=lB(polind)
                        simtrans(it)%hconssim(in)=GridH(hB(polind))
                        simtrans(it)%csim(in)=consB(polind)



                    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Now do calculations to get MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wrent(:,giExo,:,gij),bjmpc,gPh,RentVal)
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wbuy(:,giExo,:,gij),bjmpc,gPh,BuyVal)


                    !Check to see if stay renter or buy house
                    if( RentVal .gt. BuyVal  ) then


                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpol(:,giExo,:,gij),bjmpc,gPh,simtrans(it)%csim2(in))

                    else

                        EVown(:,:,:,:,gij)=Phvals(1)*EEVown(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVown(:,:,:,:,gij,giExo,Phinds(2))

                        do hcc=1,nh
                            fc=0.0d0
                            do mcc=1,nm
                                do lcc=1,nl
                                    index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                    mB(index)=mcc
                                    hB(index)=hcc
                                    lB(index)=lcc
                                    gih=hcc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gmort=GridM(gim)
                                    gloc=GridL(gil)
                                    gliq=bjmpc+y-FnTax(y)-gPh*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans

                                    bccmax=(gliq+gmort*ghouse)*(1.0d0+rf)
                                    !Changed 10/13/15 to prevent new home buyers from using HELOCs for purchase
                                    if(giC .eq. ngpC) then
                                       bccmin=0.0d0
                                    elseif(HELOCVal .eq. 1) then
                                       bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                    else
                                       bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                    endif


                                    bccmid=(bccmax+bccmin)/2.0d0

                                    if(bccmax .gt. bccmin) then

                                        ftemp1=FnVFB(0.0d0)
                                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)
                                        if(ftemp2<ftemp1) then
                                            ftemp1=ftemp2
                                        else
                                            btemp=0.0d0
                                        endif
                                    else
                                       helpWB(index)=Wmin
                                       btemp=0.0d0
                                       consB(index)=cmin
                                    endif
                                    bB(index)=btemp
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)
                                    consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                    helpWB(index)=-ftemp1
                                enddo
                                fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                            enddo
                        enddo
                        polind=maxloc(helpWB,dim=1)

                        simtrans(it)%csim2(in)=consB(polind)

                    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !END MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



                else ! Homeowner

                    if(Modify .eq. 1) then

                        if(it .eq. 166) simtrans(it)%msim(in)=min(simtrans(it)%msim(in),0.95*PhPath(106))
                        if(it .eq. 206) simtrans(it)%msim(in)=min(simtrans(it)%msim(in),0.90*PhPath(106))

                    endif


                    hj=simtrans(it)%hsim(in)
                    lj=simtrans(it)%lsim(in)*(1.0d0+rl)
                    ljI=simtrans(it)%lsimI(in)
                    mj=simtrans(it)%msim(in)*(1.0d0+rm)
                    hjI=simtrans(it)%hsimI(in)



                    bjp=simtrans(it)%bsim(in)+hj*(Ph-mj-lj)-FnAdj(Ph*hj)

                    !Compared to those who started off as renters, additional income from tax deduction
                    bjps=bjp+(Fntax(y)-FnTaxM(y,hj*(simtrans(it)%msim(in)+simtrans(it)%lsim(in)),min(0.0d0,bj)))

                    if(bjps .lt. GridBS(1)) then
                        Wrentsim=Vmin
                        Wbuysim=Vmin
                    elseif(bjps .lt. 0.0d0) then
                        !Renting value

                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WrentS(:,giExo,:,gij),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WbuyS(:,giExo,:,gij),bjps,gPh,Wbuysim)
                    else
                        !Renting value

                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wrent(:,giExo,:,gij),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wbuy(:,giExo,:,gij),bjps,gPh,Wbuysim)
                    endif

                    helpWsell=max(Wbuysim,Wrentsim)

                    ! Figure out foreclosure value
                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wfore(:,ljI,nm,hjI,giExo,:,gij),bj,gPh,helpWfore)
                    !First check if the moving shock hits


                    if(simtrans(it)%thetasim(in) .eq. 1) then !Hit by moving shock

                        !Only options are sell or foreclosure

                        if(helpWsell .ge. helpWfore) then !If selling is better

                            simtrans(it)%sellsim(in)=1
                            simtrans(it)%foresim(in)=0
                            if(Wrentsim .gt. Wbuysim) then !If renting is better

                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,bRpol(:,giExo,:,gij),bjps,gPh,simtrans(it+1)%bsim(in))
                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpol(:,giExo,:,gij),bjps,gPh,simtrans(it)%csim(in))
                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,hconsRpol(:,giExo,:,gij),bjps,gPh,simtrans(it+1)%hconssim(in))


                                simtrans(it)%buysim(in)=0
                                simtrans(it+1)%ownsim(in)=0
                                simtrans(it+1)%hsim(in)=0.0d0
                                simtrans(it+1)%msim(in)=0.0d0
                                simtrans(it+1)%lsim(in)=0.0d0
                                simtrans(it)%refisim(in)=0

                            else

                                EVown(:,:,:,:,gij)=Phvals(1)*EEVown(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVown(:,:,:,:,gij,giExo,Phinds(2))

                                do hcc=1,nh
                                    fc=0.0d0
                                    do mcc=1,nm
                                    do lcc=1,nl
                                        index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                        mB(index)=mcc
                                        hB(index)=hcc
                                        lB(index)=lcc
                                        gih=hcc
                                        gil=lcc
                                        gim=mcc
                                        ghouse=GridH(gih)
                                        gmort=GridM(gim)
                                        gloc=GridL(gil)
                                        gliq=bjps+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans

                                        bccmax=(gliq+gmort*ghouse)*(1.0d0+rf)
                                        !Changed 10/13/15 to prevent new home buyers from using HELOCs for purchase
                                        if(giC .eq. ngpC) then
                                           bccmin=0.0d0
                                        elseif(HELOCVal .eq. 1) then
                                           bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                        else
                                           bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                        endif


                                        bccmid=(bccmax+bccmin)/2.0d0

                                        if(bccmax .gt. bccmin) then

                                            ftemp1=FnVFB(0.0d0)
                                            ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)
                                            if(ftemp2<ftemp1) then
                                                ftemp1=ftemp2
                                            else
                                                btemp=0.0d0
                                            endif
                                        else
                                           helpWB(index)=Wmin
                                           btemp=0.0d0
                                           consB(index)=cmin
                                        endif

                                        bB(index)=btemp

                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)
                                        consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                        helpWB(index)=-ftemp1
                                    enddo
                                    fc=mortfc !Set fixed cost of mortgage for when mcc>1
                                    enddo
                                enddo
                                polind=maxloc(helpWB(1:nm*nh*nl),dim=1)

                                simtrans(it+1)%ownsim(in) = 1
                                simtrans(it)%buysim(in) = 1
                                simtrans(it+1)%hsim(in)=GridH(hB(polind))
                                simtrans(it+1)%hsimI(in)=hB(polind)
                                simtrans(it+1)%msim(in)=GridM(mB(polind))
                                simtrans(it+1)%bsim(in)=bB(polind)
                                simtrans(it+1)%lsim(in)=GridL(lB(polind))
                                simtrans(it+1)%lsimI(in)=lB(polind)
                                simtrans(it)%hconssim(in)=simtrans(it+1)%hsim(in)
                                simtrans(it)%csim(in)=consB(polind)
                             endif


                        else  !If foreclosing is better

                            simtrans(it)%buysim(in) = 0
                            simtrans(it+1)%ownsim(in) = 0
                            simtrans(it)%foresim(in) = 1
                            simtrans(it)%sellsim(in)=0
                            simtrans(it)%refisim(in)=0

                            simtrans(it)%hconssim(in) = simtrans(it)%hsim(in)
                            call BiLinInterp1(nb,GridB,ngpPh,PhGrid,bFpol(:,ljI,nm,hjI,giExo,:,gij),bj,gPh,simtrans(it+1)%bsim(in))
                            simtrans(it+1)%bsim(in)=max(0.0d0,simtrans(it+1)%bsim(in))
                            simtrans(it)%csim(in) = max(cmin,bj+y-FnTax(y)-FnQb(simtrans(it+1)%bsim(in),0.0d0)*simtrans(it+1)%bsim(in))
                            simtrans(it+1)%hsim(in) = 0.0d0
                            simtrans(it+1)%hsimI(in) = 1
                            simtrans(it+1)%msim(in) = 0.0d0
                            simtrans(it+1)%lsim(in) = 0.0d0
                            simtrans(it+1)%lsimI(in)=1


                        endif



                    else !Not hit by moving shock


                        if(mj .eq. 0.0d0) then
                            mc=1
                            call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wpay(:,ljI,mc,hjI,giExo,:,gij),bj,gPh,Wpaysim)
                            call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wrefin(:,ljI,mc,hjI,giExo,:,gij),bj,gPh,Wrefisim)

                        else

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,Wpay(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,Wpaysim)
                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,Wrefin(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,Wrefisim)

                        endif




                        helpVals(1) =   Wbuysim
                        helpVals(2) =   Wrentsim
                        helpVals(3) =   helpWfore
                        helpVals(4) =   Wpaysim
                        helpVals(5) =   Wrefisim

                        polind=maxloc(helpVals,dim=1)
                        select case(polind)





                            case(1) !Buying optimal
                                EVown(:,:,:,:,gij)=Phvals(1)*EEVown(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVown(:,:,:,:,gij,giExo,Phinds(2))

                                simtrans(it)%sellsim(in)=1
                                simtrans(it)%foresim(in)=0
                                simtrans(it)%buysim(in)=1
                                simtrans(it+1)%ownsim(in)=1
                                do hcc=1,nh
                                    fc=0.0d0
                                    do mcc=1,nm
                                    do lcc=1,nl
                                        index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                        mB(index)=mcc
                                        hB(index)=hcc
                                        lB(index)=lcc
                                        gih=hcc
                                        gil=lcc
                                        gim=mcc
                                        ghouse=GridH(gih)
                                        gmort=GridM(gim)
                                        gloc=GridL(gil)
                                        gliq=bjps+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))
                                        ftemp1=FnVFB(bmin)
                                        ftemp2=FnVFB(GridB(2))
                                        bccmax=gliq-fc
                                        if(ftemp2<ftemp1) then
                                            ftemp1=golden(bmin,GridB(2),bccmax,FnVFB,vftol,btemp)
                                        else
                                            btemp=bmin
                                        endif
                                        bB(index)=btemp
                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),btemp,Ph,qmtemp)
                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),btemp,Ph,qltemp)

                                        consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                        helpWB(index)=-ftemp1
                                    enddo
                                    fc=mortfc !Set fixed cost of mortgage for when mcc>1
                                    enddo
                                enddo
                                polind=maxloc(helpWB(1:nh*nl*nm),dim=1)


                                simtrans(it+1)%hsim(in)=GridH(hB(polind))
                                simtrans(it+1)%hsimI(in)=hB(polind)
                                simtrans(it+1)%msim(in)=GridM(mB(polind))
                                simtrans(it+1)%bsim(in)=bB(polind)
                                simtrans(it+1)%lsim(in)=GridL(lB(polind))
                                simtrans(it+1)%lsimI(in)=lB(polind)
                                simtrans(it)%hconssim(in)=simtrans(it+1)%hsim(in)
                                simtrans(it)%csim(in)=consB(polind)

                            case(2) !Renting optimal

                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,bRpol(:,giExo,:,gij),bjps,gPh,simtrans(it+1)%bsim(in))
                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpol(:,giExo,:,gij),bjps,gPh,simtrans(it)%csim(in))
                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,hconsRpol(:,giExo,:,gij),bjps,gPh,simtrans(it+1)%hconssim(in))


                                simtrans(it)%buysim(in)=0
                                simtrans(it+1)%ownsim(in)=0
                                simtrans(it+1)%hsim(in)=0.0d0
                                simtrans(it+1)%msim(in)=0.0d0
                                simtrans(it+1)%lsim(in)=0.0d0
                                simtrans(it)%refisim(in)=0
                                simtrans(it)%foresim(in)=0
                                simtrans(it)%sellsim(in)=0


                            case(3) !Foreclosure optimal
                                simtrans(it)%refisim(in)=0

                                simtrans(it)%buysim(in) = 0
                                simtrans(it+1)%ownsim(in) = 0
                                simtrans(it)%foresim(in) = 1
                                simtrans(it)%hconssim(in) = simtrans(it)%hsim(in)

                                call BiLinInterp1(nb,GridB,ngpPh,PhGrid,bFpol(:,ljI,nm,hjI,giExo,:,gij),bj,gPh,simtrans(it+1)%bsim(in))
                                simtrans(it)%csim(in) = bj+y-FnTax(y)-FnQb(simtrans(it+1)%bsim(in),0.0d0)*simtrans(it+1)%bsim(in)
                                simtrans(it+1)%hsim(in) = 0.0d0
                                simtrans(it+1)%hsimI(in) = 1
                                simtrans(it+1)%msim(in) = 0.0d0
                                simtrans(it+1)%lsim(in) = 0.0d0
                                simtrans(it+1)%lsimI(in)=1


                            case(4) !Payment optimal
                                simtrans(it)%sellsim(in)=0
                                simtrans(it)%foresim(in)=0
                                simtrans(it)%buysim(in)=0
                                simtrans(it+1)%ownsim(in)=1
                                simtrans(it)%refisim(in)=0



                                simtrans(it+1)%hsim(in)=hj
                                simtrans(it+1)%hsimI(in)=hjI
                                simtrans(it)%hconssim(in)=hj

                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bPpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%bsim(in))
                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,mPpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%msim(in))
                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,lPpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%lsim(in))
                                simtrans(it+1)%msim(in)=simtrans(it+1)%msim(in)/hj
                                call basefun(GridL,nl,simtrans(it+1)%lsim(in),vals,inds)
                                simtrans(it+1)%lsimI(in)=inds(1)

                            case(5) !Refinance optimal

                                simtrans(it)%sellsim(in)=0
                                simtrans(it)%foresim(in)=0
                                simtrans(it)%buysim(in)=0
                                simtrans(it+1)%ownsim(in)=1
                                simtrans(it)%refisim(in)=1

                                simtrans(it+1)%hsim(in)=hj
                                simtrans(it+1)%hsimI(in)=hjI
                                simtrans(it)%hconssim(in)=hj


                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bNpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%bsim(in))
                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,mNpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%msim(in))
                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,lNpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%lsim(in))
                                call basefun(GridL,nl,simtrans(it+1)%lsim(in),vals,inds)
                                simtrans(it+1)%lsimI(in)=inds(1)

                         end select

                    endif



        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now check the MPC
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                    !Compared to those who started off as renters, additional income from tax deduction
                    bjps=bjp+(Fntax(y)-FnTaxM(y,hj*(simtrans(it)%msim(in)+simtrans(it)%lsim(in)),min(0.0d0,bj)))+mpcval
                    bjmpc=bj+mpcval

                    if(bjps .lt. GridBS(1)) then
                        Wrentsim=Vmin
                        Wbuysim=Vmin
                    elseif(bjps .lt. 0.0d0) then
                        !Renting value

                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WrentS(:,giExo,:,gij),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WbuyS(:,giExo,:,gij),bjps,gPh,Wbuysim)
                    else
                        !Renting value

                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wrent(:,giExo,:,gij),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wbuy(:,giExo,:,gij),bjps,gPh,Wbuysim)
                    endif

                    helpWsell=max(Wbuysim,Wrentsim)

                    ! Figure out foreclosure value
                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wfore(:,ljI,nm,hjI,giExo,:,gij),bjmpc,gPh,helpWfore)
                    !First check if the moving shock hits

                    if(simtrans(it)%thetasim(in) .eq. 1) then !Hit by moving shock

                        !Only options are sell or foreclosure

                        if(helpWsell .ge. helpWfore) then !If selling is better

                            if(Wrentsim .gt. Wbuysim) then !If renting is better

                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpol(:,giExo,:,gij),bjps,gPh,simtrans(it)%csim2(in))
                            else

                                EVown(:,:,:,:,gij)=Phvals(1)*EEVown(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVown(:,:,:,:,gij,giExo,Phinds(2))

                                do hcc=1,nh
                                    fc=0.0d0
                                    do mcc=1,nm
                                    do lcc=1,nl
                                        index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                        mB(index)=mcc
                                        hB(index)=hcc
                                        lB(index)=lcc
                                        gih=hcc
                                        gil=lcc
                                        gim=mcc
                                        ghouse=GridH(gih)
                                        gmort=GridM(gim)
                                        gloc=GridL(gil)
                                        gliq=bjps+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans
                                        bccmax=(gliq+gmort*ghouse)*(1.0d0+rf)
                                        !Changed 10/13/15 to prevent new home buyers from using HELOCs for purchase
                                        if(giC .eq. ngpC) then
                                           bccmin=0.0d0
                                        elseif(HELOCVal .eq. 1) then
                                           bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                        else
                                           bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                        endif


                                        bccmid=(bccmax+bccmin)/2.0d0

                                        if(bccmax .gt. bccmin) then

                                            ftemp1=FnVFB(0.0d0)
                                            ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)
                                            if(ftemp2<ftemp1) then
                                                ftemp1=ftemp2
                                            else
                                                btemp=0.0d0
                                            endif
                                        else
                                           helpWB(index)=Wmin
                                           btemp=0.0d0
                                           consB(index)=cmin
                                        endif
                                        bB(index)=btemp

                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)
                                        consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                        helpWB(index)=-ftemp1
                                    enddo
                                    fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                                    enddo
                                enddo
                                polind=maxloc(helpWB(1:nm*nh*nl),dim=1)


                                simtrans(it)%csim2(in)=consB(polind)
                             endif


                        else  !If foreclosing is better

                            call BiLinInterp1(nb,GridB,ngpPh,PhGrid,bFpol(:,ljI,nm,hjI,giExo,:,gij),bjmpc,gPh,btemp)
                            simtrans(it)%csim2(in) = max(cmin,bjmpc+y-FnTax(y)-FnQb(btemp,0.0d0)*btemp)
                        endif



                    else !Not hit by moving shock


                        if(mj .eq. 0.0d0) then
                            mc=1
                            call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wpay(:,ljI,mc,hjI,giExo,:,gij),bjmpc,gPh,Wpaysim)
                            call BiLinInterp1(nb,GridB,ngpPh,PhGrid,Wrefin(:,ljI,mc,hjI,giExo,:,gij),bjmpc,gPh,Wrefisim)

                        else

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,Wpay(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,Wpaysim)
                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,Wrefin(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,Wrefisim)

                        endif




                        helpVals(1) =   Wbuysim
                        helpVals(2) =   Wrentsim
                        helpVals(3) =   helpWfore
                        helpVals(4) =   Wpaysim
                        helpVals(5) =   Wrefisim

                        polind=maxloc(helpVals,dim=1)
                        !print*,helpVals
                        !pause
                        select case(polind)


                            case(1) !Buying optimal
                                EVown(:,:,:,:,gij)=Phvals(1)*EEVown(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVown(:,:,:,:,gij,giExo,Phinds(2))
                                do hcc=1,nh
                                    fc=0.0d0
                                    do mcc=1,nm
                                    do lcc=1,nl
                                        index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                        mB(index)=mcc
                                        hB(index)=hcc
                                        lB(index)=lcc
                                        gih=hcc
                                        gil=lcc
                                        gim=mcc
                                        ghouse=GridH(gih)
                                        gmort=GridM(gim)
                                        gloc=GridL(gil)
                                        gliq=bjps+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans
                                        bccmax=(gliq+gmort*ghouse)*(1.0d0+rf)
                                        !Changed 10/13/15 to prevent new home buyers from using HELOCs for purchase
                                        if(giC .eq. ngpC) then
                                           bccmin=0.0d0
                                        elseif(HELOCVal .eq. 1) then
                                           bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                        else
                                           bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                        endif


                                        bccmid=(bccmax+bccmin)/2.0d0

                                        if(bccmax .gt. bccmin) then

                                            ftemp1=FnVFB(0.0d0)
                                            ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)
                                            if(ftemp2<ftemp1) then
                                                ftemp1=ftemp2
                                            else
                                                btemp=0.0d0
                                            endif
                                        else
                                           helpWB(index)=Wmin
                                           btemp=0.0d0
                                           consB(index)=cmin
                                        endif
                                        bB(index)=btemp
                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),btemp,Ph,qmtemp)
                                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),btemp,Ph,qltemp)

                                        consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                        helpWB(index)=-ftemp1
                                    enddo
                                    fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                                    enddo
                                enddo
                                polind=maxloc(helpWB(1:nh*nl*nm),dim=1)


                                simtrans(it)%csim2(in)=consB(polind)

                            case(2) !Renting optimal

                                CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpol(:,giExo,:,gij),bjps,gPh,simtrans(it)%csim2(in))


                            case(3) !Foreclosure optimal

                                call BiLinInterp1(nb,GridB,ngpPh,PhGrid,bFpol(:,ljI,nm,hjI,giExo,:,gij),bjmpc,gPh,btemp)
                                simtrans(it)%csim2(in) = bjmpc+y-FnTax(y)-FnQb(btemp,0.0d0)*btemp

                            case(4) !Payment optimal

                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpol(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                            case(5) !Refinance optimal

                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpol(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                        end select
                    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! END MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                endif
                simtrans(it+1)%bsim(in)=max(bmin,simtrans(it+1)%bsim(in))
                simtrans(it+1)%msim(in)=max(0.0d0,simtrans(it+1)%msim(in))
                simtrans(it+1)%lsim(in)=max(0.0d0,simtrans(it+1)%lsim(in))
                simtrans(it)%csim(in)=max(cmin,simtrans(it)%csim(in))
                simtrans(it)%csim2(in)=max(cmin,simtrans(it)%csim2(in))
                simtrans(it)%beqsim(in)=0.0d0
                simtrans(it)%mpcsim(in)=(simtrans(it)%csim2(in)-simtrans(it)%csim(in))/mpcval






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! RETIREMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            case(Jwork+1:Jtot-1)


                gij=ijret
                y=simtrans(it)%psim(in)
                giR=ip
                iR=giR

                giExo=(giAgg-1)*ngpR+giR


               !Check if comes into period with house
                if(simtrans(it)%ownsim(in) .eq. 0) then



                    !Check to see if stay renter or buy house
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,WrentRet(:,giExo,:,gij),bj,gPh,RentVal)
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,WbuyRet(:,giExo,:,gij),bj,gPh,BuyVal)

                    if(RentVal .ge. BuyVal ) then


                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,bRpolRet(:,giExo,:,gij),bj,gPh,simtrans(it+1)%bsim(in))
                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpolRet(:,giExo,:,gij),bj,gPh,simtrans(it)%csim(in))
                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,hconsRpolRet(:,giExo,:,gij),bj,gPh,simtrans(it+1)%hconssim(in))

                        simtrans(it)%buysim(in)=0
                        simtrans(it)%refisim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%sellsim(in)=0
                        simtrans(it+1)%ownsim(in)=0
                        simtrans(it+1)%hsim(in)=0.0d0
                        simtrans(it+1)%msim(in)=0.0d0
                        simtrans(it+1)%lsim(in)=0.0d0
                        simtrans(it+1)%lsimI(in)=1

                    else !Buyer
                        EVownRet(:,:,:,:,gij)=Phvals(1)*EEVownRet(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVownRet(:,:,:,:,gij,giExo,Phinds(2))

                        do hcc=1,nh
                            fc=0.0d0
                            do mcc=1,nm
                                do lcc=1,nl



                                    index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                    mB(index)=mcc
                                    hB(index)=hcc
                                    lB(index)=lcc
                                    gih=hcc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gmort=GridM(gim)
                                    gloc=GridL(gil)
                                    gliq=bj+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))
                                    ftemp1=FnVFBRet(bmin)
                                    ftemp2=FnVFBRet(GridB(2))
                                    bccmax=bj+y-FnTax(y)-fc
                                    if(ftemp2<ftemp1) then
                                        ftemp1=golden(bmin,GridB(2),bccmax,FnVFBRet,vftol,btemp)
                                    else
                                        btemp=bmin
                                    endif
                                    bB(index)=btemp
                                    call basefun(GridB,nb,btemp,vals,inds)

                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qmret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qlret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)



                                    consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                    helpWB(index)=-ftemp1

                                enddo
                                fc=FCGrid(giAgg)
                            enddo
                        enddo

                        !maximize over the portfolio choice
                        polind=maxloc(helpWB,dim=1)

                        simtrans(it)%buysim(in)=1
                        simtrans(it+1)%bsim(in)=bB(polind)
                        simtrans(it)%hconssim(in)=GridH(hB(polind))
                        simtrans(it)%csim(in)=consB(polind)
                        simtrans(it)%refisim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%sellsim(in)=0
                        simtrans(it+1)%ownsim(in)=1
                        simtrans(it+1)%hsim(in)=GridH(hB(polind))
                        simtrans(it+1)%msim(in)=GridM(mB(polind))
                        simtrans(it+1)%lsim(in)=GridL(lB(polind))
                        simtrans(it+1)%lsimI(in)=lB(polind)
                        simtrans(it+1)%hsimI(in)=hB(polind)



                    endif




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Now check the MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                    !Check to see if stay renter or buy house
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,WrentRet(:,giExo,:,gij),bjmpc,gPh,RentVal)
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,WbuyRet(:,giExo,:,gij),bjmpc,gPh,BuyVal)
                    if(RentVal .ge. BuyVal ) then


                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpolRet(:,giExo,:,gij),bjmpc,gPh,simtrans(it)%csim2(in))

                    else !Buyer
                        EVownRet(:,:,:,:,gij)=Phvals(1)*EEVownRet(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVownRet(:,:,:,:,gij,giExo,Phinds(2))

                        do hcc=1,nh
                            fc=0.0d0
                            do mcc=1,nm
                                do lcc=1,nl



                                    index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                    mB(index)=mcc
                                    hB(index)=hcc
                                    lB(index)=lcc
                                    gih=hcc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gmort=GridM(gim)
                                    gloc=GridL(gil)
                                    gliq=bjmpc+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans

                                    bccmax=gliq+gmort*ghouse
                                    !Changed 10/13/15 to prevent new home buyers from using HELOCs for downpayment
                                    if(giC .eq. ngpC) then
                                       bccmin=0.0d0
                                    elseif(HELOCVal .eq. 1) then
                                       bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                    else
                                       bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                    endif

                                    bccmid=(bccmax+bccmin)/2.0d0



                                    if(bccmax .gt. bccmin) then
                                        ftemp1=FnVFBRet(0.0d0)
                                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFBRet,vftol,btemp)
                                        if(ftemp2<ftemp1) then
                                            ftemp1=ftemp2
                                        else
                                            btemp=0.0d0
                                        endif
                                        helpWB(index)=-ftemp1
                                    else
                                        helpWB(index)=Wmin
                                        btemp=0.0d0
                                    endif
                                    bB(index)=btemp
                                    call basefun(GridB,nb,btemp,vals,inds)

                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qmret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qlret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)



                                    consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                    helpWB(index)=-ftemp1

                                enddo
                                fc=FCGrid(giAgg)
                            enddo
                        enddo

                        !maximize over the portfolio choice
                        polind=maxloc(helpWB,dim=1)

                        simtrans(it)%csim2(in)=consB(polind)



                    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!END MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




                else  ! Homeowner

                    if(Modify .eq. 1) then

                        if(it .eq. 166) simtrans(it)%msim(in)=min(simtrans(it)%msim(in),0.95*PhPath(106))
                        if(it .eq. 206) simtrans(it)%msim(in)=min(simtrans(it)%msim(in),0.90*PhPath(106))

                    endif


                    hj=simtrans(it)%hsim(in)
                    lj=simtrans(it)%lsim(in)*(1.0d0+rl)
                    ljI=simtrans(it)%lsimI(in)
                    mj=simtrans(it)%msim(in)*(1.0d0+rm)
                    hjI=simtrans(it)%hsimI(in)


                    bjp=simtrans(it)%bsim(in)+hj*(Ph-mj-lj)-FnAdj(Ph*hj)

                    call basefun(GridB,nb,bj,vals,inds)


                    if(mj .eq. 0.0d0) then
                        mc=1
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WpayRet(:,ljI,mc,hjI,giExo,:,gij),bj,gPh,Wpaysim)
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WrefinRet(:,ljI,mc,hjI,giExo,:,gij),bj,gPh,Wrefisim)


                    else

                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WpayRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,Wpaysim)
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WrefinRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,Wrefisim)

                    endif



                    !Compared to those who started off as renters, additional income from tax deduction
                    bjps=bjp+(Fntax(y)-FnTaxM(y,hj*(simtrans(it)%msim(in)+simtrans(it)%lsim(in)),min(0.0d0,bj)))
                    if(bjps .lt. GridBS(1)) then
                        Wrentsim=Vmin
                        Wbuysim=Vmin
                    elseif(bjps .lt. 0.0d0) then
                        !Renting value

                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WrentRetS(:,giExo,:,gij+1),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WbuyRetS(:,giExo,:,gij+1),bjps,gPh,Wbuysim)
                    else
                        !Renting value

                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WrentRet(:,giExo,:,gij+1),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WbuyRet(:,giExo,:,gij+1),bjps,gPh,Wbuysim)
                    endif






                    helpWsell=max(Wbuysim,Wrentsim)

                    ! Figure out foreclosure value
                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WforeRet(:,ljI,nm,hjI,giExo,:,gij),bj,gPh,helpWfore)
                    !First check if the moving shock hits



                    helpVals(1) =   Wbuysim
                    helpVals(2) =   Wrentsim
                    helpVals(3) =   helpWfore
                    helpVals(4) =   Wpaysim
                    helpVals(5) =   Wrefisim

                    polind=maxloc(helpVals,dim=1)

                    select case(polind)





                        case(1) !Buying optimal

                            EVownRet(:,:,:,:,gij)=Phvals(1)*EEVownRet(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVownRet(:,:,:,:,gij,giExo,Phinds(2))

                            do hcc=1,nh
                                fc=0.0d0
                                do mcc=1,nm
                                do lcc=1,nl
                                    index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                    mB(index)=mcc
                                    hB(index)=hcc
                                    lB(index)=lcc
                                    gih=hcc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gmort=GridM(gim)
                                    gloc=GridL(gil)
                                    gliq=bjps+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))
                                    ftemp1=FnVFBRet(bmin)
                                    ftemp2=FnVFBRet(GridB(2))
                                    bccmax=gliq-fc
                                    if(ftemp2<ftemp1) then
                                        ftemp1=golden(bmin,GridB(2),bccmax,FnVFBRet,vftol,btemp)
                                    else
                                        btemp=bmin
                                    endif
                                    bB(index)=btemp


                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qmret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qlret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)

                                    consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                    helpWB(index)=-ftemp1

                                enddo
                                fc=mortfc !Set fixed cost of mortgage for when mcc>1
                                enddo
                            enddo
                            polind=maxloc(helpWB(1:nh*nl*nm),dim=1)

                            if(hB(polind) .eq. hjI .and. ( (helpWB(polind) .lt. Wpaysim) .or. (helpWB(polind) .lt. Wrefisim))) then

                                if(Wpaysim .ge. Wrefisim) then
                                    simtrans(it)%sellsim(in)=0
                                    simtrans(it)%foresim(in)=0
                                    simtrans(it)%buysim(in)=0
                                    simtrans(it)%refisim(in)=0
                                    simtrans(it+1)%ownsim(in)=1

                                    simtrans(it+1)%hsim(in)=hj
                                    simtrans(it+1)%hsimI(in)=hjI
                                    simtrans(it)%hconssim(in)=hj


                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%bsim(in))
                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,mPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%msim(in))
                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,lPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%lsim(in))

                                    call basefun(GridL,nl,simtrans(it+1)%lsim(in),vals,inds)
                                    simtrans(it+1)%lsimI(in)=inds(1)
                                    simtrans(it+1)%msim(in)=simtrans(it+1)%msim(in)/hj
                                else
                                    simtrans(it)%sellsim(in)=0
                                    simtrans(it)%foresim(in)=0
                                    simtrans(it)%buysim(in)=0
                                    simtrans(it+1)%ownsim(in)=1

                                    simtrans(it)%refisim(in)=1


                                    simtrans(it+1)%hsim(in)=hj
                                    simtrans(it+1)%hsimI(in)=hjI
                                    simtrans(it)%hconssim(in)=hj


                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%bsim(in))
                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,mNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%msim(in))
                                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,lNpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%lsim(in))

                                    call basefun(GridL,nl,simtrans(it+1)%lsim(in),vals,inds)
                                    simtrans(it+1)%lsimI(in)=inds(1)
                                    simtrans(it+1)%msim(in)=simtrans(it+1)%msim(in)

                                endif



                            else
                                simtrans(it)%sellsim(in)=1
                                simtrans(it)%foresim(in)=0
                                simtrans(it)%buysim(in)=1
                                simtrans(it+1)%ownsim(in)=1
                                simtrans(it+1)%hsim(in)=GridH(hB(polind))
                                simtrans(it+1)%hsimI(in)=hB(polind)
                                simtrans(it+1)%msim(in)=GridM(mB(polind))
                                simtrans(it+1)%bsim(in)=bB(polind)
                                simtrans(it+1)%lsim(in)=GridL(lB(polind))
                                simtrans(it+1)%lsimI(in)=lB(polind)
                                simtrans(it)%hconssim(in)=simtrans(it+1)%hsim(in)
                                simtrans(it)%csim(in)=consB(polind)
                            endif
                    case(2) !Renting optimal

                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,bRpolRet(:,giExo,:,gij),bjps,gPh,simtrans(it+1)%bsim(in))
                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpolRet(:,giExo,:,gij),bjps,gPh,simtrans(it)%csim(in))
                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,hconsRpolRet(:,giExo,:,gij),bjps,gPh,simtrans(it+1)%hconssim(in))


                        simtrans(it)%buysim(in)=0
                        simtrans(it)%refisim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%sellsim(in)=0
                        simtrans(it+1)%ownsim(in)=0
                        simtrans(it+1)%hsim(in)=0.0d0
                        simtrans(it+1)%msim(in)=0.0d0
                        simtrans(it+1)%lsim(in)=0.0d0
                        simtrans(it+1)%lsimI(in)=1

                    case(3) !Foreclosure optimal

                        simtrans(it)%buysim(in) = 0
                        simtrans(it+1)%ownsim(in) = 0
                        simtrans(it)%foresim(in) = 1
                        simtrans(it)%hconssim(in) = simtrans(it)%hsim(in)
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,bFpolRet(:,ljI,nm,hjI,giExo,:,ijret),bj,gPh,simtrans(it+1)%bsim(in))
                        simtrans(it)%csim(in) = bj+y-FnTax(y)-FnQb(simtrans(it+1)%bsim(in),0.0d0)*simtrans(it+1)%bsim(in)
                        simtrans(it+1)%hsim(in) = 0.0d0
                        simtrans(it+1)%hsimI(in) = 1
                        simtrans(it+1)%msim(in) = 0.0d0
                        simtrans(it+1)%lsim(in) = 0.0d0
                        simtrans(it+1)%lsimI(in)=1


                    case(4) !Payment optimal
                        simtrans(it)%sellsim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%buysim(in)=0
                        simtrans(it)%refisim(in)=0
                        simtrans(it+1)%ownsim(in)=1

                        simtrans(it+1)%hsim(in)=hj
                        simtrans(it+1)%hsimI(in)=hjI
                        simtrans(it)%hconssim(in)=hj


                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%bsim(in))
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,mPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%msim(in))
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,lPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%lsim(in))

                        call basefun(GridL,nl,simtrans(it+1)%lsim(in),vals,inds)
                        simtrans(it+1)%lsimI(in)=inds(1)
                        simtrans(it+1)%msim(in)=simtrans(it+1)%msim(in)/hj



                    case(5) !Refinance optimal

                        simtrans(it)%sellsim(in)=0
                        simtrans(it)%foresim(in)=0
                        simtrans(it)%buysim(in)=0
                        simtrans(it+1)%ownsim(in)=1

                        simtrans(it)%refisim(in)=1


                        simtrans(it+1)%hsim(in)=hj
                        simtrans(it+1)%hsimI(in)=hjI
                        simtrans(it)%hconssim(in)=hj


                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%bsim(in))
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,mNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%msim(in))
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,lNpol(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it+1)%lsim(in))

                        call basefun(GridL,nl,simtrans(it+1)%lsim(in),vals,inds)
                        simtrans(it+1)%lsimI(in)=inds(1)
                        simtrans(it+1)%msim(in)=simtrans(it+1)%msim(in)


                     end select


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Now check the MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


                    bjpmpc=simtrans(it)%bsim(in)+hj*(Ph-mj-lj)-FnAdj(Ph*hj)+mpcval


                    call basefun(GridB,nb,bjmpc,vals,inds)


                    if(mj .eq. 0.0d0) then
                        mc=1
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WpayRet(:,ljI,mc,hjI,giExo,:,gij),bjmpc,gPh,Wpaysim)
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WrefinRet(:,ljI,mc,hjI,giExo,:,gij),bjmpc,gPh,Wrefisim)


                    else

                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WpayRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,Wpaysim)
                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WrefinRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,Wrefisim)

                    endif



                    !Compared to those who started off as renters, additional income from tax deduction
                    bjps=bjpmpc+(Fntax(y)-FnTaxM(y,hj*(simtrans(it)%msim(in)+simtrans(it)%lsim(in)),min(0.0d0,bj)))
                    if(bjps .lt. GridBS(1)) then
                        Wrentsim=Vmin
                        Wbuysim=Vmin
                    elseif(bjps .lt. 0.0d0) then
                        !Renting value

                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WrentRetS(:,giExo,:,gij+1),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nbs,GridBS,ngpPh,PhGrid,WbuyRetS(:,giExo,:,gij+1),bjps,gPh,Wbuysim)
                    else
                        !Renting value

                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WrentRet(:,giExo,:,gij+1),bjps,gPh,Wrentsim)

                        !Buying value
                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WbuyRet(:,giExo,:,gij+1),bjps,gPh,Wbuysim)
                    endif






                    helpWsell=max(Wbuysim,Wrentsim)

                    ! Figure out foreclosure value
                    call BiLinInterp1(nb,GridB,ngpPh,PhGrid,WforeRet(:,ljI,nm,hjI,giExo,:,gij),bjmpc,gPh,helpWfore)
                    !First check if the moving shock hits


                    helpVals(1) =   Wbuysim
                    helpVals(2) =   Wrentsim
                    helpVals(3) =   helpWfore
                    helpVals(4) =   Wpaysim
                    helpVals(5) =   Wrefisim

                    polind=maxloc(helpVals,dim=1)

                    select case(polind)


                    case(1) !Buying optimal

                        EVownRet(:,:,:,:,gij)=Phvals(1)*EEVownRet(:,:,:,:,gij,giExo,Phinds(1))+Phvals(2)*EEVownRet(:,:,:,:,gij,giExo,Phinds(2))

                        do hcc=1,nh
                            fc=0.0d0
                            do mcc=1,nm
                            do lcc=1,nl
                                index=(hcc-1)*nm*nl+(mcc-1)*nl+lcc
                                mB(index)=mcc
                                hB(index)=hcc
                                lB(index)=lcc
                                gih=hcc
                                gil=lcc
                                gim=mcc
                                ghouse=GridH(gih)
                                gmort=GridM(gim)
                                gloc=GridL(gil)
                                gliq=bjps+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))+htrans
                                bccmax=gliq+gmort*ghouse
                                !Changed 10/13/15 to prevent new home buyers from using HELOCs for downpayment
                                !bccmin=0.0d0
    !                            bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                if(giC .eq. ngpC) then
                                   bccmin=0.0d0
                                elseif(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif

                                bccmid=(bccmax+bccmin)/2.0d0



                                if(bccmax .gt. bccmin) then
                                    ftemp1=FnVFBRet(0.0d0)
                                    ftemp2=golden(bccmin,bccmid,bccmax,FnVFBRet,vftol,btemp)
                                    if(ftemp2<ftemp1) then
                                        ftemp1=ftemp2
                                    else
                                        btemp=0.0d0
                                    endif
                                    helpWB(index)=-ftemp1
                                else
                                    helpWB(index)=Wmin
                                    btemp=0.0d0
                                endif
                                bB(index)=btemp


                                call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qmret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qmtemp)
                                call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qlret(:,gil,gim,gih,giExo,:,gij+1),btemp,gPh,qltemp)

                                consB(index)=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(btemp,0.0d0)*btemp
                                helpWB(index)=-ftemp1

                            enddo
                            fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                            enddo
                        enddo
                        polind=maxloc(helpWB(1:nh*nl*nm),dim=1)

                        if(hB(polind) .eq. hjI .and. ( (helpWB(polind) .lt. Wpaysim) .or. (helpWB(polind) .lt. Wrefisim))) then

                            if(Wpaysim .ge. Wrefisim) then

                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                            else

                                call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                            endif



                        else

                           simtrans(it)%csim2(in)=consB(polind)

                        endif
                    case(2) !Renting optimal

                        CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpolRet(:,giExo,:,gij),bjps,gPh,simtrans(it)%csim2(in))


                    case(3) !Foreclosure optimal


                        call BiLinInterp1(nb,GridB,ngpPh,PhGrid,bFpolRet(:,ljI,nm,hjI,giExo,:,ijret),bjmpc,gPh,btemp)
                        simtrans(it)%csim2(in) = bjmpc+y-FnTax(y)-FnQb(btemp,0.0d0)*btemp


                    case(4) !Payment optimal

                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                    case(5) !Refinance optimal

                        call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                     end select


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!END MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









                  endif


                simtrans(it)%beqsim(in)=0.0d0
                simtrans(it+1)%bsim(in)=max(bmin,simtrans(it+1)%bsim(in))
                simtrans(it+1)%msim(in)=max(0.0d0,simtrans(it+1)%msim(in))
                simtrans(it+1)%lsim(in)=max(0.0d0,simtrans(it+1)%lsim(in))
                simtrans(it)%csim(in)=max(cmin,simtrans(it)%csim(in))
                simtrans(it)%csim2(in)=max(cmin,simtrans(it)%csim2(in))
                simtrans(it)%mpcsim(in)=(simtrans(it)%csim2(in)-simtrans(it)%csim(in))/mpcval
                simtrans(it)%beqsim(in)=0.0d0




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! LAST PERIOD OF LIFE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




            case(Jtot)

                iR=ip
                giR=iR
                y=simtrans(it)%psim(in)

                gij=ijret

                giExo=(giAgg-1)*ngpR+giR

               !Check if comes into period with house
                if(simtrans(it)%ownsim(in) .eq. 0) then


                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,bRpolRet(:,giExo,:,gij),bj,gPh,simtrans(it)%beqsim(in))
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpolRet(:,giExo,:,gij),bj,gPh,simtrans(it)%csim(in))
                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,hconsRpolRet(:,giExo,:,gij),bj,gPh,simtrans(it+1)%hconssim(in))


                    simtrans(it)%buysim(in)=0
                    simtrans(it)%refisim(in)=0
                    simtrans(it)%foresim(in)=0
                    simtrans(it)%sellsim(in)=0

                    CALL BiLinInterp1(nb,GridB,ngpPh,PhGrid,consRpolRet(:,giExo,:,gij),bjmpc,gPh,simtrans(it)%csim2(in))




                else


                    if(Modify .eq. 1) then

                        if(it .eq. 166) simtrans(it)%msim(in)=min(simtrans(it)%msim(in),0.95*PhPath(106))
                        if(it .eq. 206) simtrans(it)%msim(in)=min(simtrans(it)%msim(in),0.90*PhPath(106))

                    endif


                    hj=simtrans(it)%hsim(in)
                    lj=simtrans(it)%lsim(in)*(1.0d0+rl)
                    ljI=simtrans(it)%lsimI(in)
                    mj=simtrans(it)%msim(in)*(1.0d0+rm)
                    hjI=simtrans(it)%hsimI(in)


                    bjp=bj+hj*(Ph-mj-lj)-FnAdj(Ph*hj)


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Start off with the sellers problem
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Compute value of liquid assets after the sale


                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WforeRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,helpWfore)
                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WforeRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,WpaySim)
                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WforeRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,helpWsell)



                    HelpVals(1)=helpWsell
                    HelpVals(2)=Wpaysim
                    HelpVals(3)=helpWfore

                    polind=maxloc(HelpVals(1:3),dim=1)

                    simtrans(it)%buysim(in)=0
                    simtrans(it)%refisim(in)=0

                    select case(polind)

                        case(1)   ! Sell

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,hconsNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%hconssim(in))
                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bNpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%beqsim(in))


                            simtrans(it)%sellsim(in)=1
                            simtrans(it)%foresim(in)=0
                            simtrans(it)%buysim(in)=0
                            simtrans(it)%refisim(in)=0


                        case(2)   ! Pay
                            simtrans(it)%buysim(in)=0
                            simtrans(it)%refisim(in)=0
                            simtrans(it)%sellsim(in)=0

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bPpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%beqsim(in))


                            simtrans(it)%beqsim(in)=simtrans(it)%beqsim(in)+gPh*hj
                            simtrans(it)%hconssim(in)=hj
                            simtrans(it)%foresim(in)=0

                        case(3)   ! Foreclose
                            simtrans(it)%sellsim(in)=0

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consFpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%csim(in))
                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,bFpolRet(:,ljI,:,hjI,giExo,:,gij),bj,simtrans(it)%msim(in),gPh,simtrans(it)%beqsim(in))


                            simtrans(it)%hconssim(in)=hj*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
                            simtrans(it)%foresim(in)=1
                            simtrans(it)%buysim(in)=0
                            simtrans(it)%refisim(in)=0




                    end select

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Do MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WforeRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,helpWfore)
                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WforeRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,WpaySim)
                    call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,WforeRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,helpWsell)



                    HelpVals(1)=helpWsell
                    HelpVals(2)=Wpaysim
                    HelpVals(3)=helpWfore

                    polind=maxloc(HelpVals(1:3),dim=1)

                    select case(polind)

                        case(1)   ! Sell

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consNpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                        case(2)   ! Pay

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consPpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                        case(3)   ! Foreclose

                            call TriLinInterp1(nb,GridB,nm,GridM,ngpPh,PhGrid,consFpolRet(:,ljI,:,hjI,giExo,:,gij),bjmpc,simtrans(it)%msim(in),gPh,simtrans(it)%csim2(in))

                    end select

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!END MPC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



            endif


            simtrans(it)%csim(in)=max(cmin,simtrans(it)%csim(in))
            simtrans(it)%csim2(in)=max(cmin,simtrans(it)%csim2(in))
            simtrans(it)%mpcsim(in)=(simtrans(it)%csim2(in)-simtrans(it)%csim(in))/mpcval




            !Then you die


         end select
      end do
      !$OMP END PARALLEL DO






   end do


end subroutine SimulateBPP

!
!   SimulateSteadyDist.f90
!
!
!   Created by Kurt Mitman on 30/04/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

subroutine SimulateSteadyDist

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
integer     :: inds(2),indsm(2),indsS(2),indsl(2)
double precision    ::  vals(2),valsm(2),valsS(2),valsl(2)
double precision    :: helpWsell,helpWfore,helpWpay,helpWrefin
double precision    :: HelpVals(4)
double precision    :: btemp, y, pm, fc
double precision    :: bj,bjp,mj,mjp,hj,lj,cj
double precision    :: Wrentsim,consRpolsim,hconsRpolsim,bRpolsim
double precision    :: Wbuysim
double precision    :: Wrefisim,consNpolsim,hconsNpolsim,bNpolsim,mNpolsim,hNpolsim,lNpolsim
double precision    :: Wpaysim,consPpolsim,hconsPpolsim,bPpolsim,mPpolsim,hPpolsim,lPpolsim
double precision,dimension(nh*nm*nl)    :: helpWB,consB,bB,mPp
integer,dimension(nh*nm*nl) ::hB,mB,hconsB,lB
integer             :: hINpolsim,hIPpolsim,lINpolsim,lIPpolsim

double precision    :: phh,qmtemp,qltemp,qltemp2,RentVal,BuyVal,Phvals(2)

double precision    :: Ht,temp

DOUBLE PRECISION, EXTERNAL :: FnVFR, golden, FnVFB, FnVFF, FnVFN,FnVFP,FnXSH
DOUBLE PRECISION, EXTERNAL          :: FnVFRRetJ, FnVFFRetJ, FnVFPRetJ
DOUBLE PRECISION, EXTERNAL          :: FnVFRRet, FnVFBRet, FnVFFRet, FnVFNRet,FnVFPRet

double precision,dimension(ngpPh)   ::Hdemand,Hsupply
!double PRECISION, allocatable, dimension(:,:)::hsim,msim,lsim,bsim,hconssim,csim,beqsim
!INTEGER, allocatable, dimension(:,:)::hsimI,buysim,ownsim,lsimI,refisim,foresim,sellsim,psim
double precision     :: HInv(ngpAy,ngpZh,ngpPh)
double precision     :: Vmin,Wmin,Wsellbuy,Wsellrent
!double precision     :: poprand(nsim)

integer :: bc,bcc,lc,hc,kcc


Vmin=-1.0d10
Wmin=Vmin

Ht=1.55d0
phh=1.0d0
ownsimss=0
hsimss=0.0d0
hconssimss=0.0d0
bsimss=0.0d0
vftol=1.0d-5
print*,'Starting simulation'
hsimIss=1
lsimIss=1
print*,giPh,phgrid(giPh),prgrid(giPh)

Ph=phgrid(giPh)
distssr=0.0d0
distsso=0.0d0
if(inithouse .eq. 0) then
    distssr(1,:,1)=zydist(1,:)
else
    distssr(1,:,1)=zydist(1,:)*(1.0d0-sum(initdistsso))
    distsso(:,:,:,:,:,1)=initdistsso
endif


consdistr=0.0d0
hconsdistr=0.0d0
bdistr=0.0d0
buydistr=0.0d0
consdisto=0.0d0
hconsdisto=0.0d0
bdisto=0.0d0
buydisto=0.0d0
foredisto=0.0d0
refidisto=0.0d0
selldisto=0.0d0
hdemdistr=0.0d0
hdemdisto=0.0d0
giAY = AtoY(giAgg)

giRf = AtoR(giAgg)
rf=GridRf(giRf)
grf=rf
rm=Rmgrid(giAgg)
!rf+mortmarkup
grm=rm
rl=RlGrid(giAgg)
grl=rl
gPh=Ph
print*,giAy,giRf,giC


DO gij=1,Jwork

!    print*,gij,sum(distssr),sum(distsso),sum(distssr)+sum(distsso)
!    print*,maxval(hconsdisto),maxval(hconsdistr)
    ij=gij
    ijret = ij-Jwork

!    !$OMP PARALLEL COPYIN(gij,giAY)
    !First Simulate Renters

!    !$OMP DO PRIVATE(iW,bj,bc,mj,hjI,vals,inds,valsm,indsm,hcc,lcc,mcc,bcc)
    do giW=1,ngpW
        do bc=1,nbsim
             if(distssr(bc,giW,gij) .eq. 0.0d0) cycle

            iW=giW
            giExo=(giAgg-1)*ngpW+giW


            Wsellrent=sum(bcvals(:,bc)*Wrent(bcinds(:,bc),giExo,giPh,gij))
            Wsellbuy=sum(bcvals(:,bc)*Wbuy(bcinds(:,bc),giExo,giPh,gij))
            !Check to see if stay renter or buy house
            if(Wsellrent .ge. Wsellbuy ) then
                bj=sum(bcvals(:,bc)*bRpol(bcinds(:,bc),giExo,giPh,gij))
                hconsdistr(bc,giW,gij)=sum(bcvals(:,bc)*hconsRpol(bcinds(:,bc),giExo,giPh,gij))
                consdistr(bc,giW,gij)=sum(bcvals(:,bc)*consRpol(bcinds(:,bc),giExo,giPh,gij))
                call basefun(GridBsim,nbsim,bj,vals,inds)

                do giWp=1,ngpW
                    distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distssr(bc,giW,gij)
                    distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distssr(bc,giW,gij)
                enddo
            else
                bj=sum(bcvals(:,bc)*bBpol(bcinds(:,bc),giExo,giPh,gij))
                mj=sum(bcvals(:,bc)*mBpol(bcinds(:,bc),giExo,giPh,gij))

                hconsdistr(bc,giW,gij)=sum(bcvals(:,bc)*hconsBpol(bcinds(:,bc),giExo,giPh,gij))
                hdemdistr(bc,giW,gij)=hconsdistr(bc,giW,gij)
                !hdistr(hc,giW,gij)=hconsBpol(bc,giExo,giPh,gij)

                consdistr(bc,giW,gij)=sum(bcvals(:,bc)*consBpol(bcinds(:,bc),giExo,giPh,gij))
                bdistr(bc,giW,gij)=bj
                buydistr(bc,giW,gij)=1

                call basefun(GridBsim,nbsim,bj,vals,inds)
                call basefun(GridMsim,nbsim,mj,valsm,indsm)


                do giWp=1,ngpW
                   do mcc=1,2
                    do bcc=1,2
                       hcc=hIBpol(bcinds(mcc,bc),giExo,giPh,gij)
                       lcc=lIBpol(bcinds(mcc,bc),giExo,giPh,gij)
                       kcc=MtoMsim(mIBpol(bcinds(mcc,bc),giExo,giPh,gij))

                        if(GridMsim(kcc) .gt. gridC(giC)*Ph) then
                            !print*,'Renter'
                            !print*,bc,giW,gij
                            !print*,hcc,lcc,kcc
                        endif

                       distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+bcvals(mcc,bc)*vals(bcc)*zytrans(gij,giW,giWp)*distssr(bc,giW,gij)
                    enddo
                    enddo
                enddo


            endif
            if(consdistr(bc,giW,gij) .le. 0.0d0) then
                print*,gij,giW,bc
                print*,consdistr(bc,giW,gij)
                print*,Wsellbuy,Wsellrent
                print*,bcinds(:,bc)
                print*,bcvals(:,bc)
                print*,consRpol(bcinds(:,bc),giExo,giPh,gij)
                print*,consBpol(bcinds(:,bc),giExo,giPh,gij)
            endif
        enddo
    enddo
!    !$OMP END DO NOWAIT



!    !$OMP DO PRIVATE(iW,hc,mc,lc,bc,bj,mj,hjI,vals,inds,valsm,indsm,valsS,indsS,hcc,lcc,mcc,bcc,kcc,bjp,bjps,Wsellbuy,Wsellrent,helpVals,hj,polind,y)
    do giW=1,ngpW
        do hc=1,nh
            do mc=1,nmsim
                do lc = 1,nl
                    do bc = 1,nbsim
                         if(distsso(bc,lc,mc,hc,giW,gij) .eq. 0.0d0) cycle
                        iW=giW
                        gif=fWind(giW)
                        giz=zWind(giW)
                        y=ypsgrid(gij,gif,giz,giAY)
                        giExo=(giAgg-1)*ngpW+giW


                        !Compute value of liquid assets after the sale

                        bjp=GridBsim(bc)+GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
                        bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj)))

                        if(bjps .lt. 0.0d0) then
                            !Negative Liquid Assets
                            call basefun(GridBS,nbs,bjps,valsS,indsS)
                            Wsellbuy=valsS(1)*WbuyS(indsS(1),giExo,giPh,gij)+valsS(2)*WbuyS(indsS(2),giExo,giPh,gij)
                            Wsellrent=valsS(1)*WrentS(indsS(1),giExo,giPh,gij)+valsS(2)*WrentS(indsS(2),giExo,giPh,gij)


                        else
                            !Positive Liquid Assets
                            call basefun(GridB,nb,bjps,valsS,indsS)
                            Wsellbuy=valsS(1)*Wbuy(indsS(1),giExo,giPh,gij)+valsS(2)*Wbuy(indsS(2),giExo,giPh,gij)
                            Wsellrent=valsS(1)*Wrent(indsS(1),giExo,giPh,gij)+valsS(2)*Wrent(indsS(2),giExo,giPh,gij)


                        endif

                        helpWsell=max(Wsellbuy,Wsellrent)

                        call BiLinInterp1(nb,GridB,nm,GridM,Wfore(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),helpWfore)



                        !See if better to foreclose or sell
                        if(helpWfore .gt. helpWsell) then

!                           if(GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc)) .ge. 0.4d0) then
!                              print*,'Foreclose pos equity'
!                              print*,gij,hc,mc,bc
!                              print*,helpWfore,helpWsell
!                              print*,bjp,bjps
!                              print*,Wsellbuy,Wsellrent
!                              print*,GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
                              !pause
!                           endif

                            call BiLinInterp1(nb,GridB,nm,GridM,consFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),cj)
                            call BiLinInterp1(nb,GridB,nm,GridM,hconsFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),hj)
                            call BiLinInterp1(nb,GridB,nm,GridM,bFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),bj)


                            consdisto(bc,lc,mc,hc,giW,gij)=cj*thet(gij)
                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                            foredisto(bc,lc,mc,hc,giW,gij)=thet(gij)
                            hdemdisto(bc,lc,mc,hc,giW,gij)=GridH(hc)*thet(gij)
                            call basefun(GridBsim,nbsim,bj,vals,inds)

                            do giWp=1,ngpW
                                distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)
                                distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)
                            enddo

                        else

                            selldisto(bc,lc,mc,hc,giW,gij)=selldisto(bc,lc,mc,hc,giW,gij)+(thet(gij))

                            if(bjps .lt. 0.0d0) then
                            !Negative Liquid Assets

                                if(Wsellbuy .gt. Wsellrent) then

                                    consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consBpolS(indsS(:),giExo,giPh,gij))*thet(gij)
                                    hj=sum(valsS*hconsBpolS(indsS(:),giExo,giPh,gij))
                                    hconsdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                                    hdemdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                                    buydisto(bc,lc,mc,hc,giW,gij)=thet(gij)
                                    bj=sum(valsS*bBpolS(indsS(:),giExo,giPh,gij))
                                    call basefun(GridBsim,nbsim,bj,vals,inds)


                                    do giWp=1,ngpW
                                        do mcc=1,2
                                            do bcc=1,2
                                                hcc=hIBpolS(indsS(mcc),giExo,giPh,gij)
                                                kcc=MtoMsim(mIBpolS(indsS(mcc),giExo,giPh,gij))
                                                lcc=lIBpolS(indsS(mcc),giExo,giPh,gij)
                                                distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)


                                                    if(GridMsim(kcc) .gt. gridC(giC)*Ph) then
                                                       ! print*,'Sell buy neg'
                                                       ! print*,bc,giW,gij
                                                       ! print*,hcc,lcc,kcc
                                                       ! print*,GridMsim(kcc),gridC(giC)*Ph
                                                    endif




                                            enddo

                                        enddo
                                    enddo




                                else

                                    consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consRpolS(indsS(:),giExo,giPh,gij))*thet(gij)
                                    hconsdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*hconsRpolS(indsS(:),giExo,giPh,gij))*thet(gij)

                                    bj=sum(valsS*bRpolS(indsS(:),giExo,giPh,gij))
                                    call basefun(GridBsim,nbsim,bj,vals,inds)
                                    do giWp=1,ngpW
                                        distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)
                                        distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)
                                    enddo



                                endif

                            else
                            !Positive Liquid Assets

                                if(Wsellbuy .gt. Wsellrent) then

                                    consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consBpol(indsS(:),giExo,giPh,gij))*thet(gij)
                                    hj=sum(valsS*hconsBpol(indsS(:),giExo,giPh,gij))
                                    hconsdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                                    hdemdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                                    buydisto(bc,lc,mc,hc,giW,gij)=thet(gij)
                                    bj=sum(valsS*bBpol(indsS(:),giExo,giPh,gij))
                                    call basefun(GridBsim,nbsim,bj,vals,inds)

                                    do giWp=1,ngpW
                                        do mcc=1,2
                                            do bcc=1,2
                                                hcc=hIBpol(indsS(mcc),giExo,giPh,gij)
                                                kcc=MtoMsim(mIBpol(indsS(mcc),giExo,giPh,gij))
                                                lcc=lIBpol(indsS(mcc),giExo,giPh,gij)
                                                distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)


                                                    if(GridMsim(kcc) .gt. gridC(giC)*Ph) then
                                                       ! print*,'Sell buy pos'
                                                       ! print*,bc,giW,gij
                                                       ! print*,hcc,lcc,kcc
                                                       ! print*,GridMsim(kcc),gridC(giC)*Ph
                                                    endif



                                            enddo



                                        enddo
                                    enddo




                                else

                                    consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consRpol(indsS(:),giExo,giPh,gij))*thet(gij)
                                    hconsdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*hconsRpol(indsS(:),giExo,giPh,gij))*thet(gij)

                                    bj=sum(valsS*bRpol(indsS(:),giExo,giPh,gij))
                                    call basefun(GridBsim,nbsim,bj,vals,inds)
                                    do giWp=1,ngpW
                                        distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)
                                        distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*thet(gij)
                                    enddo



                                endif










                            endif








                        endif


                        call BiLinInterp1(nb,GridB,nm,GridM,Wrefin(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),helpWrefin)
                        call BiLinInterp1(nb,GridB,nm,GridM,Wpay(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),helpWpay)


                        helpVals(1) =   helpWfore
                        helpVals(2) =   helpWsell
                        helpVals(3) =   helpWrefin
                        helpVals(4) =   helpWpay


                        polind=maxloc(helpVals(1:4),dim=1)


                        select case(polind)

                            case(1) !Foreclosure optimal


!                           if(GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc)) .ge. 0.40d0) then
                              !print*,'Foreclose pos equity'
                              !print*,gij,hc,mc,bc
                              !print*,helpWfore,helpWsell
                              !print*,bjp,bjps
                              !print*,Wsellbuy,Wsellrent
                              !print*,GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
                              !pause
!                           endif



                                call BiLinInterp1(nb,GridB,nm,GridM,consFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),cj)
                                call BiLinInterp1(nb,GridB,nm,GridM,hconsFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),hj)
                                call BiLinInterp1(nb,GridB,nm,GridM,bFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),bj)


                                consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0-thet(gij))
                                hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0-thet(gij))
                                hdemdisto(bc,lc,mc,hc,giW,gij)=hdemdisto(bc,lc,mc,hc,giW,gij)+GridH(hc)*(1.0-thet(gij))
                                foredisto(bc,lc,mc,hc,giW,gij)=foredisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))

                                call basefun(GridBsim,nbsim,bj,vals,inds)

                                do giWp=1,ngpW
                                    distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                    distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                enddo





                            case(2) !Selling optimal





                                selldisto(bc,lc,mc,hc,giW,gij)=selldisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))


                                if(bjps .lt. 0.0d0) then
                                !Negative Liquid Assets

                                    if(Wsellbuy .gt. Wsellrent) then

                                        consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+sum(valsS*consBpolS(indsS(:),giExo,giPh,gij))*(1.0d0-thet(gij))
                                        hj=sum(valsS*hconsBpolS(indsS(:),giExo,giPh,gij))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                        hdemdisto(bc,lc,mc,hc,giW,gij)=hdemdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                        buydisto(bc,lc,mc,hc,giW,gij)=buydisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))
                                        bj=sum(valsS*bBpolS(indsS(:),giExo,giPh,gij))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)

                                        do giWp=1,ngpW
                                            do mcc=1,2
                                                do bcc=1,2
                                                    hcc=hIBpolS(indsS(mcc),giExo,giPh,gij)
                                                    kcc=MtoMsim(mIBpolS(indsS(mcc),giExo,giPh,gij))
                                                    lcc=lIBpolS(indsS(mcc),giExo,giPh,gij)
                                                    distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))

                                                    if(GridMsim(kcc) .gt. gridC(giC)*Ph) then
                                                        !print*,'Sell buy neg'
                                                        !print*,bc,giW,gij
                                                        !print*,hcc,lcc,kcc
                                                        !print*,GridMsim(kcc),gridC(giC)*Ph
                                                    endif



                                                enddo

                                            enddo
                                        enddo




                                    else

                                        consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+sum(valsS*consRpolS(indsS(:),giExo,giPh,gij))*(1.0d0-thet(gij))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+sum(valsS*hconsRpolS(indsS(:),giExo,giPh,gij))*(1.0d0-thet(gij))

                                        bj=sum(valsS*bRpolS(indsS(:),giExo,giPh,gij))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)
                                        do giWp=1,ngpW
                                            distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                            distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                        enddo



                                    endif




                                else
                                    !Positive Liquid Assets


                                    if(Wsellbuy .gt. Wsellrent) then

                                        consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+sum(valsS*consBpol(indsS(:),giExo,giPh,gij))*(1.0d0-thet(gij))
                                        hj=sum(valsS*hconsBpol(indsS(:),giExo,giPh,gij))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                        hdemdisto(bc,lc,mc,hc,giW,gij)=hdemdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                        buydisto(bc,lc,mc,hc,giW,gij)=buydisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))
                                        bj=sum(valsS*bBpol(indsS(:),giExo,giPh,gij))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)

                                        do giWp=1,ngpW
                                            do mcc=1,2
                                                do bcc=1,2
                                                    hcc=hIBpol(indsS(mcc),giExo,giPh,gij)
                                                    kcc=MtoMsim(mIBpol(indsS(mcc),giExo,giPh,gij))
                                                    lcc=lIBpol(indsS(mcc),giExo,giPh,gij)
                                                    distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))

                                                    if(GridMsim(kcc) .gt. gridC(giC)*Ph) then
                                                        !print*,'Sell buy pos'
                                                        !print*,bc,giW,gij
                                                        !print*,hcc,lcc,kcc
                                                        !print*,GridMsim(kcc),gridC(giC)*Ph
                                                    endif



                                                enddo

                                            enddo
                                        enddo




                                    else

                                        consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+sum(valsS*consRpol(indsS(:),giExo,giPh,gij))*(1.0d0-thet(gij))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+sum(valsS*hconsRpol(indsS(:),giExo,giPh,gij))*(1.0d0-thet(gij))

                                        bj=sum(valsS*bRpol(indsS(:),giExo,giPh,gij))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)
                                        do giWp=1,ngpW
                                            distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                            distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                        enddo



                                    endif


                                endif

                            case(3) !Refinance optimal


                                call BiLinInterp1(nb,GridB,nm,GridM,consNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),cj)
                                hj=GridH(hc)
                                call BiLinInterp1(nb,GridB,nm,GridM,bNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),bj)
                                call BiLinInterp1(nb,GridB,nm,GridM,mNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),mj)
                                call BiLinInterp1(nb,GridB,nm,GridM,lNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),lj)


                                consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0d0-thet(gij))
                                hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                hdemdisto(bc,lc,mc,hc,giW,gij)=hdemdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                refidisto(bc,lc,mc,hc,giW,gij)=(1.0d0-thet(gij))


                                hcc=hc
                                mj=min(mj,gridC(giC)*Ph)
!                                if( (mj - gridC(giC)*Ph) .gt. 1.0d-6) then
!                                    !print*,'Refi'
!                                    !print*,bc,lc,mc,hc,giW,gij
!                                    !print*,mj,gridC(giC)*Ph
!                                endif

                                call basefun(GridBsim,nbsim,bj,vals,inds)
                                call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                call basefun(GridL,nl,lj,valsl,indsl)
                                do giWp=1,ngpW
                                    do mcc=1,2
                                    do lcc=1,2

                                        distsso(inds(1),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)=distsso(inds(1),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)+vals(1)*valsm(mcc)*valsl(lcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                        distsso(inds(2),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)=distsso(inds(2),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)+vals(2)*valsm(mcc)*valsl(lcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                    enddo
                                    enddo
                                enddo




                            case(4) !Payment optimal

                                call BiLinInterp1(nb,GridB,nm,GridM,consPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),cj)
                                hj=GridH(hc)
                                call BiLinInterp1(nb,GridB,nm,GridM,bPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),bj)
!                                call BiLinInterp1(nb,GridB,nm,GridM,mPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),mj)
                                call BiLinInterp1(nb,GridB,nm,GridM,lPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),lj)

                                pm=FnPm(GridMsim(mc)*GridH(hc),gij)
                                mj=((GridMsim(mc)*GridH(hc))*(1.0d0+rm)-pm)/GridH(hc)

                                if(((mj - gridC(giC)*Ph) .gt. 1.0d-6) .and. distsso(bc,lc,mc,hc,giW,gij) .gt. 1.0d-8) then
                                    !print*,'Payment'
                                    !print*,bc,lc,mc,hc,giW,gij
                                    !print*,mj,GridMsim(mc),gridC(giC)*Ph
                                    !print*,pm,rm,grm

                                endif



                                consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0d0-thet(gij))
                                hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                hdemdisto(bc,lc,mc,hc,giW,gij)=hdemdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))

                                hcc=hc

                                call basefun(GridBsim,nbsim,bj,vals,inds)
                                call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                call basefun(GridL,nl,lj,valsl,indsl)


                                do giWp=1,ngpW
                                    do bcc=1,2
                                        do mcc=1,2
                                        do lcc=1,2
                                            distsso(inds(bcc),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)=distsso(inds(bcc),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*zytrans(gij,giW,giWp)*distsso(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                        enddo
                                        enddo
                                    enddo
                                enddo


                        end select
                        if(consdisto(bc,lc,mc,hc,giW,gij) .le. 0.0d0) then
                           print*,'owner, neg consumption'
                           print*,gij,giW,bc,lc,mc,hc
                           print*,polind,bjps
                           print*,helpvals
                           print*,consdisto(bc,lc,mc,hc,giW,gij),distsso(bc,lc,mc,hc,giW,gij)

                        endif
                    enddo
                enddo
            enddo
        enddo
    enddo
!    !$OMP END DO

!    !$OMP END PARALLEL

enddo

DO gij=Jwork+1,Jtot-1


    ij=gij
    ijret = ij-Jwork
!    print*,gij,sum(distssr),sum(distsso),sum(distssr)+sum(distsso)
!    !$OMP PARALLEL COPYIN(gij,giAY)
    !First Simulate Renters

!    !$OMP DO PRIVATE(iW,bc,bj,mj,hjI,vals,inds,valsm,indsm,hcc,lcc,mcc,bcc)
    do giW=1,ngpW
        do bc=1,nbsim
            iW=giW
            giWp=giW
            giExo=(giAgg-1)*ngpW+giW

            !Check to see if stay renter or buy house
            if(sum(bcvals(:,bc)*WrentRet(bcinds(:,bc),giExo,giPh,ijret)) .gt. sum(bcvals(:,bc)*WbuyRet(bcinds(:,bc),giExo,giPh,ijret)) ) then
                bj=sum(bcvals(:,bc)*bRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                hconsdistr(bc,giW,gij)=sum(bcvals(:,bc)*hconsRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                consdistr(bc,giW,gij)=sum(bcvals(:,bc)*consRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                call basefun(GridBsim,nbsim,bj,vals,inds)

                distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*distssr(bc,giW,gij)
                distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*distssr(bc,giW,gij)


            else
                bj=sum(bcvals(:,bc)*bBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                mj=sum(bcvals(:,bc)*mBpolRet(bcinds(:,bc),giExo,giPh,ijret))

                hconsdistr(bc,giW,gij)=sum(bcvals(:,bc)*hconsBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                hdemdistr(bc,giW,gij)=sum(bcvals(:,bc)*hconsBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                !hdistr(hc,giW,gij)=hconsBpolRet(bc,giExo,giPh,ijret)


                consdistr(bc,giW,gij)=sum(bcvals(:,bc)*consBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                bdistr(bc,giW,gij)=bj
                buydistr(bc,giW,gij)=1

                call basefun(GridBsim,nbsim,bj,vals,inds)
                !call basefun(GridM,nm,mj,valsm,indsm)

                !temp=0.0d0
                do mcc=1,2
                do bcc=1,2
                    hcc=hIBpolRet(bcinds(mcc,bc),giExo,giPh,ijret)
                    lcc=lIBpolRet(bcinds(mcc,bc),giExo,giPh,ijret)
                    kcc=MtoMsim(mIBpolRet(bcinds(mcc,bc),giExo,giPh,ijret))
                    distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+vals(bcc)*bcvals(mcc,bc)*distssr(bc,giW,gij)
                    !temp=temp+vals(bcc)*bcvals(mcc,bc)
                enddo
                enddo
                !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'1 ', temp
           endif
            if(consdistr(bc,giW,gij) .le. 0.0d0) print*,gij,giW,bc


        enddo
    enddo
!    !$OMP END DO NOWAIT



!    !$OMP DO PRIVATE(iW,hc,mc,lc,bc,bj,mj,hjI,vals,inds,valsm,indsm,valsS,indsS,hcc,lcc,mcc,bcc,kcc,bjp,bjps,Wsellbuy,Wsellrent,helpVals,hj,polind,y)
    do giW=1,ngpW
        do hc=1,nh
            do mc=1,nmsim
                do lc = 1,nl
                    do bc = 1,nbsim
                        if(distsso(bc,lc,mc,hc,giW,gij) .eq. 0.0d0) cycle

                        iW=giW
                        giWp=giW
                        giExo=(giAgg-1)*ngpW+giW
                        y=pgrid(gij,giW)


                        !Compute value of liquid assets after the sale
                        bjp=GridBsim(bc)+GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
                        bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj)))
                        if(bjps .lt. 0.0d0) then

                            !Negative Liquid Assets
                            call basefun(GridBS,nbs,bjps,valsS,indsS)
                            Wsellbuy=valsS(1)*WbuyRetS(indsS(1),giExo,giPh,ijret)+valsS(2)*WbuyRetS(indsS(2),giExo,giPh,ijret)
                            Wsellrent=valsS(1)*WrentRetS(indsS(1),giExo,giPh,ijret)+valsS(2)*WrentRetS(indsS(2),giExo,giPh,ijret)
                        else

                            !Positive Liquid Assets


                            call basefun(GridB,nb,bjps,valsS,indsS)
                            Wsellbuy=valsS(1)*WbuyRet(indsS(1),giExo,giPh,ijret)+valsS(2)*WbuyRet(indsS(2),giExo,giPh,ijret)
                            Wsellrent=valsS(1)*WrentRet(indsS(1),giExo,giPh,ijret)+valsS(2)*WrentRet(indsS(2),giExo,giPh,ijret)
                        endif

                        helpWsell=max(Wsellbuy,Wsellrent)


                        call BiLinInterp1(nb,GridB,nm,GridM,WforeRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWfore)
                        call BiLinInterp1(nb,GridB,nm,GridM,WrefinRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWrefin)
                        call BiLinInterp1(nb,GridB,nm,GridM,WpayRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWpay)


                        helpVals(1) =   helpWfore
                        helpVals(2) =   helpWsell
                        helpVals(3) =   helpWrefin
                        helpVals(4) =   helpWpay



                        polind=maxloc(helpVals(1:4),dim=1)


                        select case(polind)

                            case(1) !Foreclosure optimal

!                           if(GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc)) .ge. 0.40d0) then
!                              print*,'Foreclose pos equity'
!                              print*,gij,hc,mc,bc
!                              print*,helpWfore,helpWsell
!                              print*,bjp,bjps
!                              print*,Wsellbuy,Wsellrent
!                              print*,GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
!                              !pause
!                           endif

                                call BiLinInterp1(nb,GridB,nm,GridM,consFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),cj)
                                call BiLinInterp1(nb,GridB,nm,GridM,hconsFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),hj)
                                call BiLinInterp1(nb,GridB,nm,GridM,bFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bj)

                                consdisto(bc,lc,mc,hc,giW,gij)=cj
                                hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                hdemdisto(bc,lc,mc,hc,giW,gij)=GridH(hc)
                                foredisto(bc,lc,mc,hc,giW,gij)=1.0d0

                                call basefun(GridBsim,nbsim,bj,vals,inds)


                                distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*distsso(bc,lc,mc,hc,giW,gij)
                                distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*distsso(bc,lc,mc,hc,giW,gij)
                                !temp=sum(vals)
                                !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'2 ', temp




                            case(2) !Selling optimal





                                selldisto(bc,lc,mc,hc,giW,gij)=1.0d0



                                if(bjps .lt. 0.0d0) then

                                    !Negative Liquid Assets

                                    if(Wsellbuy .gt. Wsellrent) then

                                        consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consBpolRetS(indsS(:),giExo,giPh,ijret))
                                        hj=sum(valsS*hconsBpolRetS(indsS(:),giExo,giPh,ijret))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                        hdemdisto(bc,lc,mc,hc,giW,gij)=hj
                                        buydisto(bc,lc,mc,hc,giW,gij)=1.0d0
                                        bj=sum(valsS*bBpolRetS(indsS(:),giExo,giPh,ijret))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)
                                        !temp=0.0d0
                                        do mcc=1,2
                                            do bcc=1,2
                                                hcc=hIBpolRetS(indsS(mcc),giExo,giPh,ijret)
                                                kcc=MtoMsim(mIBpolRetS(indsS(mcc),giExo,giPh,ijret))
                                                lcc=lIBpolRetS(indsS(mcc),giExo,giPh,ijret)
                                                !temp=temp+vals(bcc)*valsS(mcc)
                                                distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*distsso(bc,lc,mc,hc,giW,gij)
                                            enddo
                                        enddo
                                        !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'3 ', temp



                                    else

                                        consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consRpolRetS(indsS(:),giExo,giPh,ijret))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*hconsRpolRetS(indsS(:),giExo,giPh,ijret))

                                        bj=sum(valsS*bRpolRetS(indsS(:),giExo,giPh,ijret))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)

                                        distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*distsso(bc,lc,mc,hc,giW,gij)
                                        distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*distsso(bc,lc,mc,hc,giW,gij)
                                        !temp=sum(vals)
                                        !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'4 ', temp

                                    endif




                                else
                                    !Positive Liquid Assets



                                    if(Wsellbuy .gt. Wsellrent) then

                                        consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consBpolRet(indsS(:),giExo,giPh,ijret))
                                        hj=sum(valsS*hconsBpolRet(indsS(:),giExo,giPh,ijret))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                        hdemdisto(bc,lc,mc,hc,giW,gij)=hj
                                        buydisto(bc,lc,mc,hc,giW,gij)=1.0d0
                                        bj=sum(valsS*bBpolRet(indsS(:),giExo,giPh,ijret))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)
                                        !temp=0.0d0
                                         do mcc=1,2
                                            do bcc=1,2
                                                hcc=hIBpolRet(indsS(mcc),giExo,giPh,ijret)
                                                kcc=MtoMsim(mIBpolRet(indsS(mcc),giExo,giPh,ijret))
                                                lcc=lIBpolRet(indsS(mcc),giExo,giPh,ijret)
                                                !temp=temp+vals(bcc)*valsS(mcc)
                                                distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distsso(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*distsso(bc,lc,mc,hc,giW,gij)
                                            enddo

                                        enddo
                                        !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'5 ', temp
                                    else

                                        consdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*consRpolRet(indsS(:),giExo,giPh,ijret))
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=sum(valsS*hconsRpolRet(indsS(:),giExo,giPh,ijret))

                                        bj=sum(valsS*bRpolRet(indsS(:),giExo,giPh,ijret))
                                        call basefun(GridBsim,nbsim,bj,vals,inds)
                                        distssr(inds(1),giWp,gij+1)=distssr(inds(1),giWp,gij+1)+vals(1)*distsso(bc,lc,mc,hc,giW,gij)
                                        distssr(inds(2),giWp,gij+1)=distssr(inds(2),giWp,gij+1)+vals(2)*distsso(bc,lc,mc,hc,giW,gij)



                                    endif


                                endif

                            case(3) !Refinance optimal


                                call BiLinInterp1(nb,GridB,nm,GridM,consNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),cj)
                                hj=GridH(hc)
                                call BiLinInterp1(nb,GridB,nm,GridM,bNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bj)
                                call BiLinInterp1(nb,GridB,nm,GridM,mNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),mj)
                                call BiLinInterp1(nb,GridB,nm,GridM,lNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),lj)


                                consdisto(bc,lc,mc,hc,giW,gij)=cj
                                hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                hdemdisto(bc,lc,mc,hc,giW,gij)=hj
                                refidisto(bc,lc,mc,hc,giW,gij)=1.0d0


!                                lcc=lINpolRet(bc,lc,mc,hc,giExo,giPh,ijret)
                                hcc=hc
                                mj=min(mj,gridC(giC)*Ph)

                                call basefun(GridBsim,nbsim,bj,vals,inds)
                                call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                call basefun(GridL,nl,lj,valsl,indsl)

                                lcc=1
                                !temp=0.0d0
                                do bcc=1,2
                                do mcc=1,2
                                do lcc=1,2
                                    kcc=indsm(mcc)
                                    !temp=temp+vals(bcc)*valsm(mcc)
                                    distsso(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)=distsso(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*distsso(bc,lc,mc,hc,giW,gij)
                                enddo
                                enddo
                                enddo
                                !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'6 ', temp


                            case(4) !Payment optimal


                                call BiLinInterp1(nb,GridB,nm,GridM,consPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),cj)
                                hj=GridH(hc)
                                call BiLinInterp1(nb,GridB,nm,GridM,bPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bj)
!                                call BiLinInterp1(nb,GridB,nm,GridM,mPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),mj)

                                consdisto(bc,lc,mc,hc,giW,gij)=cj
                                hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                hdemdisto(bc,lc,mc,hc,giW,gij)=hj


                                lcc=1 !lIPpolRet(bc,lc,mc,hc,giExo,giPh,ijret)
                                hcc=hc

                                pm=FnPm(GridMsim(mc)*GridH(hc),gij)
                                mj=((GridMsim(mc)*GridH(hc))*(1.0d0+rm)-pm)/GridH(hc)

                                call basefun(GridBsim,nbsim,bj,vals,inds)
                                call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                call basefun(GridL,nl,lj,valsl,indsl)


                                !temp=0.0d0
                                do bcc=1,2
                                    do mcc=1,2
                                    do lcc=1,2
                                       kcc=indsm(mcc)
                                       !temp=temp+vals(bcc)*valsm(mcc)
                                        distsso(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)=distsso(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*distsso(bc,lc,mc,hc,giW,gij)
                                    enddo
                                    enddo
                                enddo
                                !if(abs(temp-1.0d0) .gt. 1.0d-6) print*,'7 ', temp


                        end select
                        if(consdisto(bc,lc,mc,hc,giW,gij) .le. 0.0d0) print*,gij,giW,bc,lc,mc,hc

                    enddo
                enddo
            enddo
        enddo
    enddo
!    !$OMP END DO

!    !$OMP END PARALLEL

enddo



gij=Jtot
!print*,gij,sum(distssr),sum(distsso),sum(distssr)+sum(distsso)
ij=gij
ijret = ij-Jwork
! !$OMP PARALLEL COPYIN(gij,giAY)
! !$OMP DO PRIVATE(iW,bc,bj,mj,hjI,vals,inds,valsm,indsm,hcc,lcc,mcc,bcc)
do giW=1,ngpW
    do bc=1,nbsim
        iW=giW
        giWp=giW
        giExo=(giAgg-1)*ngpW+giW

        bj=sum(bcvals(:,bc)*bRpolRet(bcinds(:,bc),giExo,giPh,ijret))
        hconsdistr(bc,giW,gij)=sum(bcvals(:,bc)*hconsRpolRet(bcinds(:,bc),giExo,giPh,ijret))
        consdistr(bc,giW,gij)=sum(bcvals(:,bc)*consRpolRet(bcinds(:,bc),giExo,giPh,ijret))

    enddo
enddo
! !$OMP END DO NOWAIT


! !$OMP DO PRIVATE(iW,hc,mc,lc,bc,bj,mj,hjI,vals,inds,valsm,indsm,valsS,indsS,hcc,lcc,mcc,bcc,kcc,bjp,bjps,Wsellbuy,Wsellrent,helpVals,hj,polind,y)
do giW=1,ngpW
    do hc=1,nh
        do mc=1,nmsim
            do lc = 1,nl
                do bc = 1,nb
                    iW=giW
                    giWp=giW
                    giExo=(giAgg-1)*ngpW+giW
                    y=pgrid(gij,giW)

                    call BiLinInterp1(nb,GridB,nm,GridM,WforeRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWfore)
                    call BiLinInterp1(nb,GridB,nm,GridM,WsellRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWsell)
                    call BiLinInterp1(nb,GridB,nm,GridM,WpayRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWpay)


                    helpVals(1) =   helpWfore
                    helpVals(2) =   helpWsell
                    helpVals(3) =   helpWpay


                    polind=maxloc(helpVals(1:3),dim=1)


                    select case(polind)

                        case(1) !Foreclosure optimal

                            call BiLinInterp1(nb,GridB,nm,GridM,consFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),cj)
                            call BiLinInterp1(nb,GridB,nm,GridM,hconsFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),hj)
                            call BiLinInterp1(nb,GridB,nm,GridM,bFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bj)


                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                            hdemdisto(bc,lc,mc,hc,giW,gij)=GridH(hc)
                            foredisto(bc,lc,mc,hc,giW,gij)=1.0d0


                        case(2) !Selling optimal

                            call BiLinInterp1(nb,GridB,nm,GridM,consNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),cj)
                            call BiLinInterp1(nb,GridB,nm,GridM,hconsNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),hj)
                            call BiLinInterp1(nb,GridB,nm,GridM,bNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bj)


                            selldisto(bc,lc,mc,hc,giW,gij)=1.0d0
                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                            hdemdisto(bc,lc,mc,hc,giW,gij)=hj

                            !bj=sum(vals*bRpolRet(inds(:),giExo,giPh,ijret))


                        case(3) !Payment optimal

                            call BiLinInterp1(nb,GridB,nm,GridM,consPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),cj)
                            hj=GridH(hc)
                            call BiLinInterp1(nb,GridB,nm,GridM,bPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bj)

                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                            hdemdisto(bc,lc,mc,hc,giW,gij)=hj

                            !bj=bPpolRet(bc,lc,mc,hc,giExo,giPh,ijret)
                    end select
                enddo
            enddo
        enddo
    enddo
enddo
! !$OMP END DO

! !$OMP END PARALLEL


sshdemand=(SUM(hdemdisto*distsso)+SUM(hdemdistr*distssr))/(sum(distssr)+sum(distsso))
!print*,maxval(hconsdisto),maxval(hconsdistr)
!print*,sum(distssr)+sum(distsso)
if(SUM(distsso) .GT. 1.0d-5) print*,SUM(hdemdisto*distsso)/SUM(distsso),SUM(hdemdistr*distssr)/SUM(distssr)


Epen=0.0d0
gif=1
do gij=1,Jtot
    if(gij .le. Jwork) then
        Einc(gij)=sum(zydist(gij,:)*ypsgrid(gij,gif,:,giAY))
        Vloginc(gij)=(sum(zydist(gij,:)*(log(ypsgrid(gij,gif,:,giAY))**2.0d0)))-(sum(zydist(gij,:)*(log(ypsgrid(gij,gif,:,giAY)))))**2.0d0
    else
        Epen(gij)=sum(zydist(Jwork,:)*pgrid(gij,:))

    endif

    Econ(gij)=(SUM(consdisto(:,:,:,:,:,gij)*distsso(:,:,:,:,:,gij))+SUM(consdistr(:,:,gij)*distssr(:,:,gij)))
    Vlogcon(gij)=((SUM(log(consdisto(:,:,:,:,:,gij)+cmin)**2.0d0*distsso(:,:,:,:,:,gij))+SUM((log(consdistr(:,:,gij)+cmin)**2.0d0*distssr(:,:,gij)))))-((SUM(log(consdisto(:,:,:,:,:,gij)+cmin)*distsso(:,:,:,:,:,gij))+SUM((log(consdistr(:,:,gij)+cmin)*distssr(:,:,gij)))))**2.0d0
    Vlogcono(gij)=((SUM(log(consdisto(:,:,:,:,:,gij)+cmin)**2.0d0*distsso(:,:,:,:,:,gij))))-((SUM(log(consdisto(:,:,:,:,:,gij)+cmin)*distsso(:,:,:,:,:,gij)))**2.0d0)
    Vlogconr(gij)=(SUM((log(consdistr(:,:,gij)+cmin)**2.0d0*distssr(:,:,gij))))-(SUM((log(consdistr(:,:,gij)+cmin)*distssr(:,:,gij)))**2.0d0)

    Eown(gij)=sum(distsso(:,:,:,:,:,gij))
    Eass(gij)=(SUM(bmatj*distsso(:,:,:,:,:,gij))+SUM(bmatjr*distssr(:,:,gij)))
    Emort(gij)=(SUM(hmatj*mmatj*distsso(:,:,:,:,:,gij)))
!    Eloc(gij)=(Phgrid(giPh)*SUM(hmatj*lmatj*distsso(:,:,:,:,:,gij)))
    Ehouse(gij)=(Phgrid(giPh)*SUM(hmatj*distsso(:,:,:,:,:,gij)))
    Esell(gij)=(SUM(selldisto(:,:,:,:,:,gij)*distsso(:,:,:,:,:,gij)))
    if(Eown(gij) .gt. 1.0d-5) then
        Elev(gij)=(SUM((mmatj+lmatj)*distsso(:,:,:,:,:,gij))/Phgrid(giPh))/Eown(gij)
        Emortpos(gij)=sum(distsso(:,:,2:nmsim,:,:,gij))/Eown(gij)

        if(Emortpos(gij) .gt. 1.0d-5) then
            Efore(gij)=(SUM(foredisto(:,:,:,:,:,gij)*distsso(:,:,:,:,:,gij)))/Emortpos(gij)

        endif

    endif

    !print*,gij,Econ(gij),Eown(gij)

enddo
OPEN(3, FILE = trim(OutputDir) // 'Vlogcon.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Vlogcon)
OPEN(3, FILE = trim(OutputDir) // 'Vlogcono.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Vlogcono)
OPEN(3, FILE = trim(OutputDir) // 'Vlogconr.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Vlogconr)
OPEN(3, FILE = trim(OutputDir) // 'Vloginc.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jwork,1,Vloginc)
OPEN(3, FILE = trim(OutputDir) // 'Einc.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jwork,1,Einc)


OPEN(3, FILE = trim(OutputDir) // 'Ehouse.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Ehouse)
OPEN(3, FILE = trim(OutputDir) // 'Ehnw.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Ehouse-Emort)
OPEN(3, FILE = trim(OutputDir) // 'Eass.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Eass)
OPEN(3, FILE = trim(OutputDir) // 'Ehnwsh.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,(Ehouse-Emort)/(Ehouse-Emort+Eass+1.0d-6))
OPEN(3, FILE = trim(OutputDir) // 'Eown.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Eown)
OPEN(3, FILE = trim(OutputDir) // 'Econ.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Econ)
OPEN(3, FILE = trim(OutputDir) // 'Epen.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Epen)

print*,'House Price:         ', Ph,phGrid(giPh)
print*,'Foreclosure rate:    ', sum(Efore*Emortpos)/(sum(Emortpos)+1.0d-6)
print*,'Home ownership rate: ', sum(Eown)/dble(Jtot)
print*,'Assets:              ', sum(Eass)/dble(Jtot)
print*,'Leverage:            ', sum(Elev)/dble(Jtot)
print*,'Housing NW:          ', (sum(Ehouse)-sum(Emort))/dble(Jtot)
print*,'NW:          ', (sum(Ehouse)-sum(Emort)+sum(Eass))/dble(Jtot)
print*,'Assets at retirement:', Eass(Jwork)
print*,'Housing value:       ', sum(Ehouse)/dble(Jtot)
print*,'Housing demand:      ', sshdemand
do hc=1,nh
print*,'Frac own H:          ', hc,sum(distsso(:,:,:,hc,:,:))/sum(distsso)
enddo
end subroutine SimulateSteadyDist

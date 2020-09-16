subroutine DecisionsRet

!****************************************************************************
!
!  PROGRAM: DecisionsRet
!
!  PURPOSE:  Compute Decision Rules of Retired
!
!  VERSION:
!           0.1, 11-June-2012
!           1.0, 27-May-2013
!           1.1, 10-June-2014
!
!  LAST EDITED BY: Kurt, 13-June-2014
!****************************************************************************

use params
use globals
use funcs
use procedures

implicit none

double precision,dimension(nhr)   :: consR,helpWR
double precision,dimension(nhr)   :: consRJ,helpWRJ,bWRJ,hWRJ,bR,hconsR
double precision,dimension(nb)                 :: coef1,coef2,coef3

double precision,dimension(nh*nm*nl) :: consB,helpWB,bB
integer,dimension(nh*nm*nl)         :: mB,hB,lB
double precision                    :: bjp, bj, bjps,nunu


double precision,dimension(nm*nl)   :: consN,helpWN,bN
integer,dimension(nm*nl)            :: mN,lN

double precision,dimension(nl*nm)      :: consP,helpWP,bP,mPp
integer,dimension(nl*nm)               :: lP
double precision                    :: mp, pm

integer                             :: bc,mc,mcc,hc,hcc,polind,index,lc,lcc,tempgiExo,tempgiAggp

double precision                    :: bccmax,ftemp1,ftemp2,btemp,bccmid,bccmin


double precision,dimension(2)       :: vals
integer,dimension(2)                :: inds

double precision                    :: Ph,Pr
double precision                    :: y, fc
double precision                    :: HelpVals(4)

double precision                    :: maxfunds
double precision,dimension(nb,nl,nm,nh):: funds
double precision                    :: Wsellbuy,Wsellrent

double precision                    :: vftol,Vmin,Wmin,WminFore,cminfore


DOUBLE PRECISION, EXTERNAL          :: FnVFRRetJ, golden, FnVFFRetJ, FnVFPRetJ
DOUBLE PRECISION, EXTERNAL          :: FnVFRRet, FnVFBRet, FnVFFRet, FnVFNRet,FnVFPRet

integer                             :: iphR,aggindex(ngpAgg*ngpR*ngpPh),Rindex(ngpAgg*ngpR*ngpPh),iAgg,Phindex(ngpAgg*ngpR*ngpPh)

do giAgg = 1,ngpAgg
   do giR = 1,ngpR
    do giPh = 1,ngpPh
      iPhR = (giAgg-1)*ngpR*ngpPh+(giR-1)*ngpPh+giPh
      aggindex(iPhR) = giAgg
      Rindex(iPhR) = giR
      Phindex(iPhR) = giPh
    enddo
   enddo
enddo


Vmin=-1.0d10
Wmin=Vmin
Wminfore=-1.0d9
cminfore=1.0d-4
vftol=1.0d-5
qmret=0.0d0
qmret_e=0.d0
qlret=0.0d0




! First Solve for Last Period of Life
gij=Jret
if (Display==0) write(*,*) 'Solving for decision rules at age ', gij+Jwork


!$OMP PARALLEL DO PRIVATE( consR,helpWR,consRJ,helpWRJ,hconsR,bR,hWRJ,bWRJ,consB,helpWB,bB,mB,hB,lB,bjp, bj, bjps,consN,helpWN,bN,mN,lN,consP,helpWP,bP,lP,mp, pm,bc,mc,mcc, hc,hcc,polind,index,lc,lcc,bccmax,ftemp1,ftemp2,btemp, vals,inds,Ph,Pr,y,fc,HelpVals,maxfunds,funds,Wsellbuy,Wsellrent,iPhR,rf,rm,rl,bccmid,bccmin,nunu,mPp) COPYIN(gij)
do iPhR = 1, ngpAgg*ngpR*ngpPh

    giAgg=aggindex(iPhR)
    giR=Rindex(iPhR)

    giPh = Phindex(iPhR)
    giAY = AtoY(giAgg)
    giHD=AtoD(giAgg)
    giExo=(giAgg-1)*ngpR+giR
    Ph=PhGrid(giPh)
    Pr=Prgrid(giPh,giAgg)
    gPr=Pr
    gPh=Ph
    y=pgrid(gij+Jwork,giR)
    nunu=NuGrid(giAgg)
    giRf = AtoR(giAgg)
    !Changed 11/20/15 to make grid on Rf on Agg for comovement
    rf=RfGrid(giAgg)
    grf=rf
    rm=RmGrid(giAgg)
    grm=rm
    rl=RlGrid(giAgg)
    grl=rl


        do bc = 1,nb
            bj=GridB(bc)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !If Renter, will stay renter - just need to find house size
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            do hcc=1,nhr


                index=hcc
                gliq=bj+y-FnTax(y)-Pr*GridHR(hcc)+Trans(giPh)
                bccmax=(gliq-cmin)*(1.0d0+rf)
                gih = hcc
                bccmin=0.0d0
                bccmid=(bccmax+bccmin)/2.0d0
                btemp=bccmin
                if(bccmax .ge. bccmin) then
                    ftemp1=FnVFRRetJ(bccmin)
                    ftemp2=golden(bccmin,bccmid,bccmax,FnVFRRetJ,vftol,btemp)
                    if(ftemp2<ftemp1) then
                        ftemp1=ftemp2
                    else
                        btemp=bccmin
                    endif
                    helpWRJ(index)=-ftemp1
                else
                    helpWRJ(index)=Wmin
                    btemp=0.0d0
                endif


                bWRJ(index)=btemp
                consRJ(index)=gliq-FnQb(btemp,0.0d0)*btemp
                hWRJ(index)=GridHR(hcc)

            enddo
            polind=maxloc(helpWRJ(1:nhr),dim=1)
            WrentRet(bc,giExo,giPh,gij)          =   helpWRJ(polind)
            consRPolRet(bc,giExo,giPh,gij)=consRJ(polind)
            hconsRPolRet(bc,giExo,giPh,gij)=hWRJ(polind)
            bRPolRet(bc,giExo,giPh,gij)=bWRJ(polind)

            !update the value and policy functions

            if(consRPolRet(bc,giExo,giPh,gij) .lt. cmin) then
               WrentRet(bc,giExo,giPh,gij)      = Wmin
                consRPolRet(bc,giExo,giPh,gij)  = cmin
                hconsRPolRet(bc,giExo,giPh,gij)  = GridHR(1)
                bRPolRet(bc,giExo,giPh,gij)  = 0.0d0


            endif
            VrentRet(bc,giExo,giPh,gij)          =   WrentRet(bc,giExo,giPh,gij)


        enddo

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !If homeowner, will  sell, foreclose or pay
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            do hc=1,nh
                do mc=1,nm
                do lc=1,nl
                do bc=1,nb
                    bj=GridB(bc)
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Start off with the sellers problem
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Compute value of liquid assets after the sale
                    bjp=bj+GridH(hc)*(Ph-GridM(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))-FnMaint(GridH(hc))

                    do hcc=1,nhr


                        index=hcc
                        gliq=Trans(giPh)+bjp+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-Pr*GridHR(hcc)
                        bccmax=(gliq-cmin)*(1.0d0+rf)
                        bccmin=0.0d0
                        bccmid=(bccmax+bccmin)/2.0d0

                        gih = hcc
                        if(bccmax .ge. bccmin) then
                            ftemp1=FnVFRRetJ(bccmin)
                            ftemp2=golden(bccmin,bccmid,bccmax,FnVFRRetJ,vftol,btemp)
                            if(ftemp2<ftemp1) then
                                ftemp1=ftemp2
                            else
                                btemp=bccmin
                            endif
                            helpWRJ(index)=-ftemp1-disutil(gij)
                        else
                            helpWRJ(index)=Wmin-disutil(gij)
                            btemp=0.0d0
                        endif
                        bWRJ(index)=btemp
                        consRJ(index)=gliq-FnQb(btemp,0.0d0)*btemp


                        hWRJ(index)=GridHR(hcc)

                    enddo
                    polind=maxloc(helpWRJ(1:nhr),dim=1)



                    !Set the value of saving to what we've just calculated
                    WsellRet(bc,lc,mc,hc,giExo,giPh,gij)    =     helpWRJ(polind)
                    consNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=consRJ(polind)
                    hconsNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=hWRJ(polind)
                    bNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=bWRJ(polind)

                    if(consRJ(polind) .lt. cmin) then


                       WsellRet(bc,lc,mc,hc,giExo,giPh,gij)    =    Wmin !-1.0d10
                        consNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=cmin
                        hconsNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridHR(1)
                        bNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0

                    endif


                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Now solve pay problem
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    pm=FnPm(GridH(hc)*GridM(mc),gij+Jwork)
                    if(mc .eq. 1) pm=0.0d0
                    mp=(GridM(mc)*GridH(hc))*(1.0d0+rm)-pm

                    gliq=Trans(giPh)+bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-pm-GridH(hc)*GridL(lc)*(1.0d0+rl)-FnMaint(GridH(hc))
                    gih = hc
                    ghouse = GridH(gih)
                    gmort = mp
                    bccmax=(gliq-cmin)*(1.0d0+rf)
                    if(HELOCVal .eq. 1) then
                       bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                    else
                       bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                    endif

                    bccmid=(bccmax+bccmin)/2.0d0

                    if(bccmax .gt. bccmin) then
                        ftemp1=FnVFPRetJ(0.0d0)
                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFPRetJ,vftol,btemp)
                        if(ftemp2<ftemp1) then
                            ftemp1=ftemp2
                        else
                            btemp=0.0d0
                        endif
                        WpayRet(bc,lc,mc,hc,giExo,giPh,gij)    = -ftemp1

                    else
                        WpayRet(bc,lc,mc,hc,giExo,giPh,gij)    = Wmin
                        btemp=0.0d0
                    endif
                    consPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=gliq-FnQb(btemp,gPh*ghouse-mp)*btemp

                    bPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=btemp
                    hconsPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                    mPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=mp
                    if(consPPolRet(bc,lc,mc,hc,giExo,giPh,gij) .lt. cmin) then


                       WpayRet(bc,lc,mc,hc,giExo,giPh,gij)    =  Wmin
                        consPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=cmin
                        bPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0

                    endif

                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    !Now solve foreclosure problem
                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                    !No max - household just consumes resources and lives in house

                    !update the value and policy functions


                    gliq=Trans(giPh)+bj+y-FnTax(y)-Pr*GridHR(1)
                    bccmax=(gliq-cmin)*(1.0d0+rf)
                    gih = hc
                    ghouse = 0.0d0 !GridH(hc)
                    bccmin=0.0d0
                    bccmid=(bccmax+bccmin)/2.0d0

                    if(bccmax .gt. bccmin) then
                       ftemp1=FnVFFRetJ(bccmin)
                       ftemp2=golden(bccmin,bccmid,bccmax,FnVFFRetJ,vftol,btemp)
                       if(ftemp2<ftemp1) then
                          ftemp1=ftemp2
                       else
                          btemp=bccmin
                       endif
                    else
                       btemp=0.0d0
                       ftemp1=-Wmin
                    end if
                    WforeRet(bc,lc,mc,hc,giExo,giPh,gij)    =-ftemp1-disutil(gij)

                    consFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=gliq-FnQb(btemp,0.0d0)*btemp
                    hconsFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridHR(1) !GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
                    bFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=btemp
                    if(consFPolRet(bc,lc,mc,hc,giExo,giPh,gij) .lt. cmin) then

                        consFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=cminfore !gliq-FnQb(btemp,0.0d0)*btemp
                        hconsFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridHR(1)
                        bFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                        WforeRet(bc,lc,mc,hc,giExo,giPh,gij)    = Wminfore




                    endif



                    if(WforeRet(bc,lc,mc,hc,giExo,giPh,gij) .GT. WsellRet(bc,lc,mc,hc,giExo,giPh,gij) .AND. WforeRet(bc,lc,mc,hc,giExo,giPh,gij) .GT. WpayRet(bc,lc,mc,hc,giExo,giPh,gij)) then
                        VownRet(bc,lc,mc,hc,giExo,giPh,gij)         =   WforeRet(bc,lc,mc,hc,giExo,giPh,gij)
                        if(mc .gt. 1) then
                            bankgain_mort(bc,lc,mc,hc,giExo,giPh) = min(1.0d0,1.0d0+(gam*gPh/(GridM(mc)*(1.0d0+rm))-1.0d0))
                        else
                            bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                        endif
                        if(lc .gt. 1) then
                            bankgain_loc(bc,lc,mc,hc,giExo,giPh) = max(0.0d0,min(1.0d0,(gam*gPh-GridM(mc)*(1.0d0+rm))/(GridL(lc)*(1.0d0+rl))))
                        else
                            bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                        endif
                    elseif(WsellRet(bc,lc,mc,hc,giExo,giPh,gij) .GT. WpayRet(bc,lc,mc,hc,giExo,giPh,gij)) then
                        !Selling optimal
                        VownRet(bc,lc,mc,hc,giExo,giPh,gij)            =   WsellRet(bc,lc,mc,hc,giExo,giPh,gij)

                        bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                        bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0

                    else
                        !Paying optimal
                       pm=FnPm(GridH(hc)*GridM(mc),gij+Jwork)
                       mp=(GridM(mc)*GridH(hc))*(1.0d0+rm)-pm
                        VownRet(bc,lc,mc,hc,giExo,giPh,gij)            =   WpayRet(bc,lc,mc,hc,giExo,giPh,gij)
                        if( mc .GT. 1) then
                           bankgain_mort(bc,lc,mc,hc,giExo,giPh) = min(1.0d0,(pm+min(mp,gPh*GridH(hc)-FnMaint(GridH(hc))-FnAdj(gPh*GridH(hc)))/(1.0d0+rm))/(GridH(hc)*GridM(mc)*(1.0d0+rm)))
                        else
                           bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                        endif

                        bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0

                    endif

                  enddo
               enddo
            enddo
         enddo
      enddo

!Now solve rest of retirement

do gij=Jret-1,1,-1
	if (Display==0) write(*,*) 'Solving for decision rules at age ', gij+Jwork

    ! Loop over aggregate house price states
!$OMP PARALLEL DO PRIVATE( consR,helpWR,consRJ,helpWRJ,hconsR,bR,hWRJ,bWRJ,consB,helpWB,bB,mB,hB,lB,bjp, bj, bjps,consN,helpWN,bN,mN,lN,consP,helpWP,bP,lP,mp, pm,bc,mc,mcc, hc,hcc,polind,index,lc,lcc,bccmax,ftemp1,ftemp2,btemp, vals,inds,Ph,Pr,y,fc,HelpVals,maxfunds,funds,Wsellbuy,Wsellrent,mpp,coef1,coef2,coef3,rf,rm,rl,nunu,bccmin,bccmid,tempgiExo,tempgiAggp) COPYIN(gij)
    do iPhR = 1, ngpAgg*ngpR*ngpPh

        giAgg=aggindex(iPhR)
        giR=Rindex(iPhR)
        nunu=NuGrid(giAgg)
        giPh = Phindex(iPhR)
        giAY = AtoY(giAgg)
        giHD=AtoD(giAgg)
        Ph=PhGrid(giPh)
        gPh=Ph
        giC=AtoC(giAgg)
        giExo=(giAgg-1)*ngpR+giR
        giRf = AtoR(giAgg)
        !Changed 11/20/15 to make grid on Rf on Agg for comovement
        rf=RfGrid(giAgg)
        grf=rf
        rm=RmGrid(giAgg)
        grm=rm
        rl=RlGrid(giAgg)
        grl=rl

        Pr=Prgrid(giPh,giAgg)
        gPr=Pr
        y=pgrid(gij+Jwork,giR)
        if(giC .lt. 1) then
           print*,giC,giAgg
           print*,AtoC

        endif
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !Compute the expected continuation values to be used later
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            do hc=1,nh
                do mc=1,nm
                   do lc=1,nl
                    do bc=1,nb
                        EVownRet(bc,lc,mc,hc,gij)=0.0d0
                        qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)=0.0d0
                        qlret(bc,lc,mc,hc,giExo,giPh,gij+1)=0.0d0
                        do giAggp=1,ngpAgg

                            giExop=(giAggp-1)*ngpR+giR

                            EVownRet(bc,lc,mc,hc,gij)=EVownRet(bc,lc,mc,hc,gij)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*VownRet(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp),gij+1)+PtoV2(giPh,giAgg,giAggp)*VownRet(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp),gij+1))
                            if(NoBankBelief .eq. 0 .or. AtoD(giAggp) .eq. ngpHD) then
                               qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)=qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))
                            else
                               tempgiAggp=CHZYtoA(AtoC(giAggp),ngpHD,AtoZ(giAggp),AtoY(giAggp))
                               tempgiExo = (tempgiAggp - 1)*ngpR + giR
                               qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)=qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,tempgiExo,PtoP1(giPh,giAgg,tempgiAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,tempgiExo,PtoP2(giPh,giAgg,tempgiAggp)))
                            endif
                            qlret(bc,lc,mc,hc,giExo,giPh,gij+1)=qlret(bc,lc,mc,hc,giExo,giPh,gij+1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_loc(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_loc(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))
                        enddo
                        qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1) = min(1.0d0,qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1))


!Removed LTI constraint from Retired
                        if( ( (GridM(mc) .GT. LTVGrid(giAgg)*gPh) .AND. (DoMqm .eq. 0)) .OR. ( (qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)*GridM(mc) .GT. LTVGrid(giAgg)*gPh) .AND. (DoMqm .eq. 1))  .OR. ( (DoInterestCap(giAgg) .eq. 1) .and. (qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1) .lt. InterestCap)) ) then

                            qmret(bc,lc,mc,hc,giExo,giPh,gij+1)=0.0d0
                        else
                            if(OnlyBankBelief .eq. 0) then
                               qmret(bc,lc,mc,hc,giExo,giPh,gij+1)=qmret_e(bc,lc,mc,hc,giExo,giPh,gij+1)-Markupgrid(giAgg)
                            else
                               qmret(bc,lc,mc,hc,giExo,giPh,gij+1)=qmret_e_exo(bc,lc,mc,hc,giExo,giPh,gij+1)-Markupgrid(giAgg)
                            endif

                         endif

                    enddo
                    enddo
                enddo
            enddo

            do bc=1,nb
                EVrentRet(bc,gij)=0.0d0
                do giAggp=1,ngpAgg
                    giExop=(giAggp-1)*ngpR+giR
                    EVrentRet(bc,gij)=EVrentRet(bc,gij)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*VrentRet(bc,giExop,PtoP1(giPh,giAgg,giAggp),gij+1)+PtoV2(giPh,giAgg,giAggp)*VrentRet(bc,giExop,PtoP2(giPh,giAgg,giAggp),gij+1))
                enddo
            enddo
            do hc=1,nh
                do mc=1,nm
                   do lc=1,nl
                    do bc=1,nb
                        funds(bc,lc,mc,hc)=(qmret(bc,lc,mc,hc,giExo,giPh,gij+1)*GridM(mc)+qlret(bc,lc,mc,hc,giExo,giPh,gij+1)*GridL(lc))*GridH(hc)-FnQb(GridB(bc),(Ph-GridM(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))*GridH(hc))*GridB(bc)-Ph*GridH(hc)
                    enddo
                    enddo
                enddo
            enddo

            EEVownRet(:,:,:,:,gij,giExo,giPh)=EVownRet(:,:,:,:,gij)
            EEVrentRet(:,gij,giExo,giPh)=EVrentRet(:,gij)


            maxfunds=maxval(funds)

            do bc = 1,nb

                bj=GridB(bc)

                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !First solve for the renter's problem
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



                !For each grid point on b and h, compute non-durable consumption
                do hcc=1,nhr
                    index=hcc
                    gliq=Trans(giPh)+bj+y-FnTax(y)-Pr*GridHR(hcc)
                    bccmax=(gliq-cmin)*(1.0d0+rf)
                    gih = hcc

                    bccmin=0.0d0
                    bccmid=(bccmax+bccmin)/2.0d0


                    if(gliq .gt. (cmin+bccmin)) then

                        ftemp1=FnVFRRet(bccmin)
                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFRRet,vftol,btemp)
                        if(ftemp2<ftemp1) then
                            ftemp1=ftemp2
                        else
                            btemp=0.0d0
                        endif

                        helpWR(index)=-ftemp1

                    else

                        btemp=0.0d0
                        helpWR(index)=Wmin

                    endif
                    hconsR(index)=GridHR(hcc)
                    bR(index)=btemp
                    consR(index)=gliq-FnQb(btemp,0.0d0)*btemp

                enddo
                !maximize over the portfolio choice
                polind=maxloc(helpWR(1:nhr),dim=1)

                !update the value and policy functions
                WrentRet(bc,giExo,giPh,gij)=helpWR(polind)
                consRPolRet(bc,giExo,giPh,gij)=consR(polind)
                hconsRPolRet(bc,giExo,giPh,gij)=hconsR(polind)
                bRPolRet(bc,giExo,giPh,gij)=bR(polind)
                mRPolRet(bc,giExo,giPh,gij)=0.0d0
                hRPolRet(bc,giExo,giPh,gij)=0.0d0


                if(consRPolRet(bc,giExo,giPh,gij) .lt. cmin) then
                    WrentRet(bc,giExo,giPh,gij)=Vmin
                    consRPolRet(bc,giExo,giPh,gij)=cmin
                    hconsRPolRet(bc,giExo,giPh,gij)=GridHR(1)
                    bRPolRet(bc,giExo,giPh,gij)=0.0d0
                    mRPolRet(bc,giExo,giPh,gij)=0.0d0
                    hRPolRet(bc,giExo,giPh,gij)=0.0d0
                endif


                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !Next solve for the buyer's problem
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
                            gliq=Trans(giPh)+bj+y-FnTax(y)-Ph*GridH(hcc)-fc
                            bccmax=bj+y-FnTax(y)-fc-Ph*GridH(hcc)+gmort*ghouse
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

                            consB(index)=gliq+(vals(1)*(qmret(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gmort+qlret(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gloc)+(vals(2)*(qmret(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gmort+qlret(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gloc)))*ghouse-FnQb(btemp,(gPh-gmort)*ghouse)*btemp

                        enddo
                        fc=FCGrid(giAgg)
                    enddo
                enddo

                !maximize over the portfolio choice
                polind=maxloc(helpWB,dim=1)

                !update the value and policy functions
                WbuyRet(bc,giExo,giPh,gij)       =   helpWB(polind)
                consBPolRet(bc,giExo,giPh,gij)   =   consB(polind)
                hBPolRet(bc,giExo,giPh,gij)      =   GridH(hB(polind))
                hIBPolRet(bc,giExo,giPh,gij)     =   hB(polind)
                hconsBPolRet(bc,giExo,giPh,gij)  =   GridH(hB(polind))
                mBPolRet(bc,giExo,giPh,gij)      =   GridM(mB(polind))
                mIBPolRet(bc,giExo,giPh,gij)     =   mB(polind)
                bBPolRet(bc,giExo,giPh,gij)      =   bB(polind)
                lIBPolRet(bc,giExo,giPh,gij)     =   lB(polind)
                lBPolRet(bc,giExo,giPh,gij)      =   GridL(lB(polind))


                if(consBPolRet(bc,giExo,giPh,gij) .lt. cmin) then
                    if(Display==1) then
                        print*,'Negative consumption buyer'
                        print*,bc,giR,giAgg
                    endif
                    WbuyRet(bc,giExo,giPh,gij)       =   Vmin
                    consBPolRet(bc,giExo,giPh,gij)   =   cmin
                    hBPolRet(bc,giExo,giPh,gij)      =   GridH(1)
                    hIBPolRet(bc,giExo,giPh,gij)     =   1
                    hconsBPolRet(bc,giExo,giPh,gij)  =   GridH(1)
                    mBPolRet(bc,giExo,giPh,gij)      =   GridM(1)
                    bBPolRet(bc,giExo,giPh,gij)      =   0.0d0
                    lIBPolRet(bc,giExo,giPh,gij)     =   1
                    lBPolRet(bc,giExo,giPh,gij)      =   GridL(1)
                endif


                !Solve whether current renter prefers to rent or buy

                if( WrentRet(bc,giExo,giPh,gij) .gt. WbuyRet(bc,giExo,giPh,gij) ) then
                    VrentRet(bc,giExo,giPh,gij)          =   WrentRet(bc,giExo,giPh,gij)
                else
                    VrentRet(bc,giExo,giPh,gij)          =   WbuyRet(bc,giExo,giPh,gij)
                endif

            enddo

            do bc=1,nbs


                bj=GridBS(bc)



                if(bj+y-FnTax(y) .gt. cmin) then
                     do hcc=1,nhr
                            index=hcc
                            gliq=Trans(giPh)+bj+y-FnTax(y)-Pr*GridHR(hcc)
                            bccmax=(gliq-cmin)*(1.0d0+rf)
                            gih = hcc

                            bccmin=0.0d0
                            bccmid=(bccmax+bccmin)/2.0d0
                            ftemp1=FnVFRRet(0.0d0)
                            ftemp2=golden(bmin,GridB(2),bccmax,FnVFRRet,vftol,btemp)
                            if(ftemp2<ftemp1) then
                                ftemp1=ftemp2
                            else
                                btemp=0.0d0
                            endif
                            bR(index)=btemp
                            consR(index)=gliq-FnQb(btemp,0.0d0)*btemp
                            helpWR(index)=-ftemp1
                            hconsR(index)=GridH(hcc)

                        enddo
                        !maximize over the portfolio choice
                        polind=maxloc(helpWR(1:nhr),dim=1)

                        !update the value and policy functions
                        WrentRetS(bc,giExo,giPh,gij)=helpWR(polind)
                        consRPolRetS(bc,giExo,giPh,gij)=consR(polind)
                        hconsRPolRetS(bc,giExo,giPh,gij)=hconsR(polind)
                        bRPolRetS(bc,giExo,giPh,gij)=bR(polind)
                        mRPolRetS(bc,giExo,giPh,gij)=0.0d0
                        hRPolRetS(bc,giExo,giPh,gij)=0.0d0


                       if(consRPolRetS(bc,giExo,giPh,gij) .lt. cmin) then
                            WrentRetS(bc,giExo,giPh,gij)=Vmin
                            consRPolRetS(bc,giExo,giPh,gij)=cmin
                            hconsRPolRetS(bc,giExo,giPh,gij)=GridHR(1)
                            bRPolRetS(bc,giExo,giPh,gij)=0.0d0
                            mRPolRetS(bc,giExo,giPh,gij)=0.0d0
                            hRPolRetS(bc,giExo,giPh,gij)=GridH(1)
                        endif

                else
                        WrentRetS(bc,giExo,giPh,gij)=Vmin
                        consRPolRetS(bc,giExo,giPh,gij)=cmin
                        hconsRPolRetS(bc,giExo,giPh,gij)=GridHR(1)
                        bRPolRetS(bc,giExo,giPh,gij)=0.0d0
                        mRPolRetS(bc,giExo,giPh,gij)=0.0d0
                        hRPolRetS(bc,giExo,giPh,gij)=GridHR(1)





                endif




                if(bj+y+maxfunds .gt. cmin) then

                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !Next solve for the buyer's problem
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
                                gliq=Trans(giPh)+bj+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))
                                bccmax=bj+y-FnTax(y)-fc
                                if(giC .eq. ngpC) then
                                   bccmin=0.0d0
                                elseif(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif

                                bccmid=(bccmax+bccmin)/2.0d0
                                ftemp1=FnVFBRet(0.0d0)
                                ftemp2=golden(bccmin,bccmid,bccmax,FnVFBRet,vftol,btemp)

                                if(ftemp2<ftemp1) then
                                    ftemp1=ftemp2
                                else
                                    btemp=0.0d0
                                endif
                                bB(index)=btemp
                                call basefun(GridB,nb,btemp,vals,inds)

                                consB(index)=gliq+(vals(1)*(qmret(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gmort+qlret(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gloc)+(vals(2)*(qmret(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gmort+qlret(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gloc)))*ghouse-FnQb(btemp,(gph-gmort)*ghouse)*btemp
                                helpWB(index)=-ftemp1

                            enddo
                            fc=FCGrid(giAgg)
                        enddo
                    enddo

                    !maximize over the portfolio choice
                    polind=maxloc(helpWB,dim=1)

                    !update the value and policy functions
                    WbuyRetS(bc,giExo,giPh,gij)       =   helpWB(polind)
                    consBPolRetS(bc,giExo,giPh,gij)   =   consB(polind)
                    hBPolRetS(bc,giExo,giPh,gij)      =   GridH(hB(polind))
                    hIBPolRetS(bc,giExo,giPh,gij)     =   hB(polind)
                    hconsBPolRetS(bc,giExo,giPh,gij)  =   GridH(hB(polind))
                    mBPolRetS(bc,giExo,giPh,gij)      =   GridM(mB(polind))
                    mIBPolRetS(bc,giExo,giPh,gij)     =   mB(polind)
                    bBPolRetS(bc,giExo,giPh,gij)      =   bB(polind)
                    lIBPolRetS(bc,giExo,giPh,gij)     =   lB(polind)
                    lBPolRetS(bc,giExo,giPh,gij)      =   GridL(lB(polind))


                    if(consBPolRetS(bc,giExo,giPh,gij) .lt. cmin) then
                        if(Display==1) then
                            print*,'Negative consumption buyer'
                            print*,bc,giR,giAgg
                        endif
                        WbuyRetS(bc,giExo,giPh,gij)       =   Vmin
                        consBPolRetS(bc,giExo,giPh,gij)   =   cmin
                        hBPolRetS(bc,giExo,giPh,gij)      =   GridH(1)
                        hIBPolRetS(bc,giExo,giPh,gij)     =   1
                        hconsBPolRetS(bc,giExo,giPh,gij)  =   GridH(1)
                        mBPolRetS(bc,giExo,giPh,gij)      =   GridM(1)
                        bBPolRetS(bc,giExo,giPh,gij)      =   0.0d0
                        lIBPolRetS(bc,giExo,giPh,gij)     =   1
                        lBPolRetS(bc,giExo,giPh,gij)      =   GridL(1)
                    endif

                else
                    WbuyRetS(bc,giExo,giPh,gij)       =   Vmin
                    consBPolRetS(bc,giExo,giPh,gij)   =   cmin
                    hBPolRetS(bc,giExo,giPh,gij)      =   GridH(1)
                    hIBPolRetS(bc,giExo,giPh,gij)     =   1
                    hconsBPolRetS(bc,giExo,giPh,gij)  =   GridH(1)
                    mBPolRetS(bc,giExo,giPh,gij)      =   GridM(1)
                    bBPolRetS(bc,giExo,giPh,gij)      =   0.d0
                    mIBPolRetS(bc,giExo,giPh,gij)     =   1
                    lIBPolRetS(bc,giExo,giPh,gij)     =   1
                    lBPolRetS(bc,giExo,giPh,gij)      =   GridL(1)




                endif



             enddo




             !!!! Now Solve Homeowner's problem

            do hc=1,nh
                do mc=1,nm
                do lc=1,nl
                    do bc=1,nb

                        bj=GridB(bc)
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Start off with the sellers problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        !Compute value of liquid assets after the sale
                        bjp=bj+GridH(hc)*(Ph-GridM(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))-FnMaint(GridH(hc))
                        bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj)))
                        !Need to check it the budget set is empty conditional on selling

                        !Need to check it the budget set is empty conditional on selling
                        if(bjps .gt. GridBS(1)) then

                            if(bjps .lt. GridB(1)) then
                                call basefun(GridBS,nbs,bjps,vals,inds)
                                Wsellbuy=vals(1)*WbuyRetS(inds(1),giExo,giPh,gij)+vals(2)*WbuyRetS(inds(2),giExo,giPh,gij)
                                Wsellrent=vals(1)*WrentRetS(inds(1),giExo,giPh,gij)+vals(2)*WrentRetS(inds(2),giExo,giPh,gij)

                            else
                                call basefun(GridB,nb,bjps,vals,inds)
                                Wsellbuy=vals(1)*WbuyRet(inds(1),giExo,giPh,gij)+vals(2)*WbuyRet(inds(2),giExo,giPh,gij)
!                                Wsellbuy=ispline(bjps,GridB,WbuyRet(:,giExo,giPh,gij),coef1,coef2,coef3,nb)
                                Wsellrent=vals(1)*WrentRet(inds(1),giExo,giPh,gij)+vals(2)*WrentRet(inds(2),giExo,giPh,gij)

                            endif
                        else
                            Wsellbuy=Wmin
                            Wsellrent=Wmin

                        endif





                        !update the value and policy functions
                        WsellRet(bc,lc,mc,hc,giExo,giPh,gij)    =   max(Wsellbuy,Wsellrent)-disutil(gij)


                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve foreclosure problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        !Only have to maximize over c and b
                        gliq=Trans(giPh)+bj+y-FnTax(y)-Pr*GridHR(1)
                        bccmax=(gliq-cmin)*(1.0d0+rf)
                        bccmin=0.0d0
                        bccmid=(bccmax+bccmid)/2.0d0

                        ghouse = GridH(hc)

                        if(bccmax .gt. bccmin) then
                            ftemp1=FnVFFRet(0.0d0)
                            ftemp2=golden(bccmin,bccmid,bccmax,FnVFFRet,vftol,btemp)
                            if(ftemp2<ftemp1) then
                                ftemp1=ftemp2
                            else
                                btemp=0.0d0
                            endif
                        else

                            btemp=0.0d0
                            ftemp1=-Wminfore
                        endif


                        !maximize over the portfolio choice

                        !update the value and policy functions
                        WforeRet(bc,lc,mc,hc,giExo,giPh,gij)=-ftemp1-bet*disutil(gij)
                        consFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=gliq-FnQb(btemp,0.0d0)*btemp
                        hconsFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridHR(1)
                        bFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=btemp
                        mFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                        hFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0


                        if(consFPolRet(bc,lc,mc,hc,giExo,giPh,gij) .lt. cmin) then
                            WforeRet(bc,lc,mc,hc,giExo,giPh,gij)=Wminfore
                            consFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=cminfore
                            hconsFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridHR(1)
                            bFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            mFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            hFPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0

                        endif



                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve refinance problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        fc=FCGrid(giAgg)
                            do mcc=max(mc,2),nm

                               do lcc=1,nl
                                    index=(mcc-max(mc,2))*nl+lcc
                                    gih=hc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gmort=GridM(gim)
                                    gloc=GridL(gil)
                                    gliq=Trans(giPh)+bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-(GridM(mc)*(1.0d0+rm)+GridL(lc)*(1.0d0+rl))*GridH(hc)-fc-FnMaint(GridH(hc))
                                    bccmax=(gliq+gmort*ghouse)*(1.0d0+rf)
                                    if(HELOCVal .eq. 1) then
                                       bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                    else
                                       bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                    endif

                                    !bccmin=min(-HELOCGrid(giAgg)*(gPh-gmort)*ghouse,0.0d0)
                                    bccmid=(bccmax+bccmin)/2.0d0


                                    if(bccmax .gt. bccmin) then
                                        ftemp1=FnVFBRet(0.0d0)
                                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFBRet,vftol,btemp)

                                        if(ftemp2<ftemp1) then
                                            ftemp1=ftemp2
                                        else
                                            btemp=0.0d0
                                        endif
                                    else
                                        btemp=0.0d0
                                        ftemp1=-Wmin
                                    endif
                                    call basefun(GridB,nb,btemp,vals,inds)

                                    consN(index)=gliq+(vals(1)*(qmret(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gmort+qlret(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gloc)+(vals(2)*(qmret(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gmort+qlret(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gloc)))*ghouse-FnQb(btemp,(gph-gmort)*ghouse)*btemp
                                    helpWN(index)=-ftemp1
                                    bN(index)=btemp
                                    mN(index)=mcc
                                    lN(index)=lcc
                                enddo
                            enddo

                            !maximize over the portfolio choice
                            polind=maxloc(helpWN(1:(nm-max(mc,2)+1)*nl),dim=1)

                            !update the value and policy functions
                            WrefinRet(bc,lc,mc,hc,giExo,giPh,gij)=helpWN(polind)
                            consNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=consN(polind)
                            hconsNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                            bNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=bN(polind)
                            mNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridM(mN(polind))
                            lNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridL(lN(polind))
                            lINPolRet(bc,lc,mc,hc,giExo,giPh,gij)=lN(polind)
                            mINPolRet(bc,lc,mc,hc,giExo,giPh,gij)=mN(polind)
                            hNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)

                            if(consNPolRet(bc,lc,mc,hc,giExo,giPh,gij) .LT. cmin) then
                                WrefinRet(bc,lc,mc,hc,giExo,giPh,gij)=Wmin
                                consNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=cmin
                                hconsNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)
                                bNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                                mNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                                lNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                                lINPolRet(bc,lc,mc,hc,giExo,giPh,gij)=1
                                mINPolRet(bc,lc,mc,hc,giExo,giPh,gij)=1
                                hNPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)


                            endif


                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve mortgage payment problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



                        !Solve for the new mortgage balance
                        pm=FnPm(GridH(hc)*GridM(mc),gij+Jwork)
                        if(mc .eq. 1) pm=0.0d0
                        mp=(GridM(mc)*GridH(hc))*(1.0d0+rm)-pm

                        !Interpolate that onto the bond grid
                        call basefun(GridM,nm,mp/GridH(hc),gvals,ginds)

                        do lcc=1,nl
                                index=lcc
                                gih=hc
                                gil=lcc
                                ghouse=GridH(gih)
                                gmort=mp
                                gloc=GridL(gil)
                                gliq=Trans(giPh)+bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-pm-GridL(lc)*ghouse*(1.d0+rl)-FnMaint(GridH(hc))

                                bccmax=gliq*(1.0d0+rf)
                                if(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif

!                                bccmin=min(-HELOCGrid(giAgg)*(gPh*ghouse-gmort),0.0d0)
                                bccmid=(bccmax+bccmin)/2.0d0
                                if(bccmax .gt. bccmin) then
                                    ftemp1=FnVFNRet(0.0d0)
                                    ftemp2=golden(bccmin,bccmid,bccmax,FnVFNRet,vftol,btemp)

                                    if(ftemp2<ftemp1) then
                                        ftemp1=ftemp2
                                    else
                                        btemp=0.0d0
                                    endif

                                else
                                    ftemp1=-Wmin
                                    btemp=0.0d0
                                endif
                                call basefun(GridB,nb,btemp,vals,inds)


                                consP(index)=gliq+(vals(1)*sum(gvals*qlret(inds(1),gil,ginds,gih,giExo,giPh,gij+1))+vals(2)*sum(gvals*qlret(inds(2),gil,ginds,gih,giExo,giPh,gij+1)))*gloc*ghouse-FnQb(btemp,Ph*GridH(hc)-mp)*btemp
                                helpWP(index)=-ftemp1
                                bP(index)=btemp
                                lP(index)=lcc
                                mPp(index)=mp
                        enddo

                        mcc=1
                        do while(((GridM(mc)*GridH(hc))*(1.0d0+rm)-pm .GT. (GridM(mcc)*GridH(hc)) .AND. mcc.LT.mc)) ! .OR. (mcc .eq. 1))
                            do lcc=1,nl
                                    index=mcc*nl+lcc
                                    gih=hc
                                    gil=lcc
                                    gim=mcc
                                    ghouse=GridH(gih)
                                    gloc=GridL(gil)
                                    gmort=GridM(gim)
                                    gliq=Trans(giPh)+bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-GridL(lc)*ghouse*(1.d0+rl)+((GridM(mcc)*GridH(hc))-(GridM(mc)*GridH(hc))*(1.0d0+rm))-FnMaint(GridH(hc))

                                    bccmax=gliq*(1.0d0+rf)
                                    if(HELOCVal .eq. 1) then
                                       bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                    else
                                       bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                    endif

!                                    bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                    bccmid=(bccmax+bccmin)/2.0d0

                                    if(bccmax .gt. bccmin) then
                                        ftemp1=FnVFPRet(0.0d0)
                                        ftemp2=golden(bccmin,bccmid,bccmax,FnVFPRet,vftol,btemp)

                                        if(ftemp2<ftemp1) then
                                            ftemp1=ftemp2
                                        else
                                            btemp=0.0d0
                                        endif
                                    else
                                        ftemp1=-Wmin
                                        btemp=0.0d0
                                    endif


                                    call basefun(GridB,nb,btemp,vals,inds)


                                    consP(index)=gliq+(vals(1)*qlret(inds(1),gil,gim,gih,giExo,giPh,gij+1)+vals(2)*qlret(inds(2),gil,gim,gih,giExo,giPh,gij+1))*gloc*ghouse-FnQb(btemp,0.0d0)*btemp
                                    helpWP(index)=-ftemp1
                                    bP(index)=btemp
                                    lP(index)=lcc
                                    mPp(index)=GridM(mcc)*GridH(hc)

                            enddo
                            mcc=mcc+1
                        enddo

                        !maximize over the portfolio choice

                        polind=maxloc(helpWP(1:mcc*nl),dim=1)

                        !update the value and policy functions
                        WpayRet(bc,lc,mc,hc,giExo,giPh,gij)=helpWP(polind)
                        consPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=consP(polind)
                        hconsPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                        bPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=bP(polind)
                        mPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=mPp(polind)
                        hPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                        lPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridL(lP(polind))
                        lIPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=lP(polind)
                        if(consPPolRet(bc,lc,mc,hc,giExo,giPh,gij) .LT. cmin) then
                            WpayRet(bc,lc,mc,hc,giExo,giPh,gij)=Wmin
                            consPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=cmin
                            hconsPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)
                            bPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            mPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0 !GridM(mc)
                            hPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)
                            lPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            lIPPolRet(bc,lc,mc,hc,giExo,giPh,gij)=1


                        endif

                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !No moving shocks in retirement
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        helpVals(1) =   WforeRet(bc,lc,mc,hc,giExo,giPh,gij)
                        helpVals(2) =   WSellRet(bc,lc,mc,hc,giExo,giPh,gij)
                        helpVals(3) =   WrefinRet(bc,lc,mc,hc,giExo,giPh,gij)
                        helpVals(4) =   WpayRet(bc,lc,mc,hc,giExo,giPh,gij)


                        polind=maxloc(helpVals,dim=1)


                        select case(polind)

                            case(1) !Foreclosure optimal

                                VownRet(bc,lc,mc,hc,giExo,giPh,gij)         =   WforeRet(bc,lc,mc,hc,giExo,giPh,gij)



                                if(mc .gt. 1) then
                                    bankgain_mort(bc,lc,mc,hc,giExo,giPh) = min(1.0d0,1.0d0+(gam*gPh/(GridM(mc)*(1.0d0+rm))-1.0d0))

                                else
                                    bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                endif
                                if(lc .gt. 1) then
                                    bankgain_loc(bc,lc,mc,hc,giExo,giPh) = max(0.0d0,min(1.0d0,(gam*gPh-GridM(mc)*(1.0d0+rm))/(GridL(lc)*(1.0d0+rl))))
                                else
                                    bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                endif

                            case(2) !Selling optimal

                                VownRet(bc,lc,mc,hc,giExo,giPh,gij)         =   WsellRet(bc,lc,mc,hc,giExo,giPh,gij)

                                bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0

                            case(3) !Refinance optimal

                                VownRet(bc,lc,mc,hc,giExo,giPh,gij)         =   WrefinRet(bc,lc,mc,hc,giExo,giPh,gij)

                                bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0


                            case(4) !Payment optimal

                                VownRet(bc,lc,mc,hc,giExo,giPh,gij)         =   WpayRet(bc,lc,mc,hc,giExo,giPh,gij)
                                mp=mPPolRet(bc,lc,mc,hc,giExo,giPh,gij)
                                pm=(GridM(mc)*GridH(hc))*(1.0d0+rm)-mp
                                call basefun(GridM,nm,mp/GridH(hc),vals,inds)

                                if(mc .gt. 1) then
                                    bankgain_mort(bc,lc,mc,hc,giExo,giPh) = (pm+(vals(1)*qmret_e(bc,lc,inds(1),hc,giExo,giPh,gij+1)+vals(2)*qmret_e(bc,lc,inds(2),hc,giExo,giPh,gij+1))*mp)/(GridH(hc)*GridM(mc)*(1.0d0+rm))
                                else
                                    bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                endif
                                bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0


                        end select
                     enddo
                  enddo
               enddo
            enddo
         enddo
      enddo



end subroutine DecisionsRet

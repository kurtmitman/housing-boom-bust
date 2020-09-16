subroutine Decisions

!****************************************************************************
!
!  PROGRAM: Decisions
!
!  PURPOSE:  Compute Decision Rules of Working
!
!  VERSION:
!           0.1, 11-June-2012
!           1.0, 27-May-2013
!           1.1, 10-June-2014
!           2.0, 29-September-2014 (Defaultable HELOCs)
!           3.0, 30-August-15 (HELOCs in b)
!
!  LAST EDITED BY: Kurt, 29-September-2014
!****************************************************************************


use params
use globals
use funcs

implicit none

double precision,dimension(nb*nh)   :: consR,helpWR,bR,hconsR


double precision,dimension(nm*nh*nl)   :: consB,helpWB,bB
integer,dimension(nm*nh*nl)            :: mB,hB,lB
double precision                    :: bjp, bj, Wsellbuy,Wsellrent,bjps,nunu


double precision,dimension(nb)      :: consF,helpWF


double precision,dimension(nm*nl)   :: consN,helpWN,bN
integer,dimension(nm*nl)            :: mN,lN

double precision,dimension(nl*nm)      :: consP,helpWP,bP,mPp
integer,dimension(nl*nm)               :: lP
double precision                    :: mp, pm

integer                             :: bc,mc,mcc,hc,hcc,polind,index,lc,lcc,tempgiExo,tempgiAggp

double precision,dimension(2)       :: vals
integer,dimension(2)                :: inds

double precision                    :: Ph,Pr
double precision                    :: y
double precision                    :: HelpVals(4)
double precision                    :: fc
double precision                    :: maxfunds
double precision,dimension(nb,nl,nm,nh):: funds
double precision                    :: vftol
double precision                    :: ftemp1,ftemp2,ftemp3,btemp,bccmax,bccmin,bccmid,Wminfore,cminfore

DOUBLE PRECISION, EXTERNAL          :: FnVFR, golden, FnVFB, FnVFF, FnVFN, FnVFP
integer                             :: iphw,aggindex(ngpAgg*ngpW*ngpPh),windex(ngpAgg*ngpW*ngpPh),Phindex(ngpAgg*ngpW*ngpPh)
integer                             :: iAgg
double precision                    :: Vmin,Wmin

Vmin=-1.0d10
Wmin=Vmin
Wminfore=-1.0d8
cminfore=1.0d-4

vftol=1.0d-5
qm_e=0.0d0
qm=0.0d0
ql=0.0d0
do giAgg = 1,ngpAgg
   do giW = 1,ngpW
    do gipH = 1,ngpPh
      iphw = (giAgg-1)*ngpW*ngpPh+(giW-1)*ngpPh+giPh
      aggindex(iphw) = giAgg
      windex(iphw) = giW
      Phindex(iphw) = giPh
    enddo
   enddo
enddo





do gij = Jwork,1,-1

	if (Display==0) write(*,*) 'Solving for decision rules at age ', gij


    ! Loop over aggregate house price states

    !$OMP PARALLEL DO PRIVATE(consR,helpWR,bR,hconsR,consB,helpWB,bB,mB,hB,lB,bjp, bj, Wsellbuy,Wsellrent,bjps,consF,helpWF,consN,helpWN, bN,mN,lN,consP,helpWP,bP,lP,mp,pm,bc,mc,mcc,hc,hcc,polind,index,lc,lcc,vals,inds,Ph,Pr,y,HelpVals,fc,maxfunds, funds,ftemp1,ftemp2,btemp,bccmax,mpp,iphw,rf,rm,rl,bccmid,bccmin,ftemp3,nunu,tempgiExo,tempgiAggp) COPYIN(gij)
	do iphw = 1, ngpAgg*ngpW*ngpPh
            giAgg = aggindex(iphw)
            giW = windex(iphw)
            giPh = Phindex(iphw)
            giAY = AtoY(giAgg)
            giHD=AtoD(giAgg)
            giC = AtoC(giAgg)
            nunu=NuGrid(giAgg)
            giExo=(giAgg-1)*ngpW+giW

            giRf = AtoR(giAgg)


            !Changed 11/20/15 to make grid on Rf on Agg for comovement
            rf=RfGrid(giAgg)
            grf=rf
            rm=RmGrid(giAgg)
            grm=rm
            rl=RlGrid(giAgg)
            grl=rl

            Ph=PhGrid(giPh)
            gPh=Ph
            Pr=Prgrid(giPh,giAgg)
            gPr=Pr
            gif=fWind(1+mod(giW-1,ngpW/ngpPv))
            giz=zWind(1+mod(giW-1,ngpW/ngpPv))
            y=ypsgrid(gij,gif,giz,giAY)


            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !Compute the expected continuation values to be used later
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            !Last working period has slightly different continuation

            if(gij .LT. Jwork) then
                do hc=1,nh
                    do mc=1,nm
                        do lc=1,nl
                        do bc=1,nb
                            EVown(bc,lc,mc,hc,gij)=0.0d0
                            do giAggp = 1,ngpAgg
                                do giWp = 1, ngpW
                                    giExop = (giAggp-1)*ngpW+giWp
                                    EVown(bc,lc,mc,hc,gij)=EVown(bc,lc,mc,hc,gij)+zytrans(gij,giW,giWp)*transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*Vown(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp),gij+1)+PtoV2(giPh,giAgg,giAggp)*Vown(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp),gij+1))

                                    if(NoBankBelief .eq. 0 .or. AtoD(giAggp) .eq. ngpHD) then

                                      qm_e(bc,lc,mc,hc,giExo,giPh,gij+1)=qm_e(bc,lc,mc,hc,giExo,giPh,gij+1) + zytrans(gij,giW,giWp)*transMatAgg(giAgg,giAggp)*((1.0d0-thet(gij))*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))+thet(gij)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort_move(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort_move(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp))))

                                    else
                                      tempgiAggp=CHZYtoA(AtoC(giAggp),ngpHD,AtoZ(giAggp),AtoY(giAggp))
                                      tempgiExo = (tempgiAggp - 1)*ngpW + giW
                                      qm_e(bc,lc,mc,hc,giExo,giPh,gij+1)=qm_e(bc,lc,mc,hc,giExo,giPh,gij+1) + zytrans(gij,giW,giWp)*transMatAgg(giAgg,giAggp)*((1.0d0-thet(gij))*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,tempgiExo,PtoP1(giPh,giAgg,tempgiAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,tempgiExo,PtoP2(giPh,giAgg,tempgiAggp)))+thet(gij)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort_move(bc,lc,mc,hc,tempgiExo,PtoP1(giPh,giAgg,tempgiAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort_move(bc,lc,mc,hc,tempgiExo,PtoP2(giPh,giAgg,tempgiAggp))))

                                    endif

                                    ql(bc,lc,mc,hc,giExo,giPh,gij+1)=ql(bc,lc,mc,hc,giExo,giPh,gij+1) + zytrans(gij,giW,giWp)*transMatAgg(giAgg,giAggp)*( (1.0d0-thet(gij))*(PtoV1(giPh,giAgg,giAggp)*bankgain_loc(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_loc(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))+thet(gij)*(PtoV1(giPh,giAgg,giAggp)*bankgain_loc_move(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_loc_move(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp))))


                                 enddo
                              enddo

                              qm_e(bc,lc,mc,hc,giExo,giPh,gij+1)=min(1.0d0,qm_e(bc,lc,mc,hc,giExo,giPh,gij+1))
                              if( ((GridM(mc) .GT. LTVGrid(giAgg)*gPh) .AND. (DoMqm .eq. 0)) .OR. ((qm_e(bc,lc,mc,hc,giExo,giPh,gij+1)*GridM(mc) .GT. LTVGrid(giAgg)*gPh) .AND. (DoMqm .eq. 1))  .OR. (FnPm(GridM(mc)*GridH(hc),gij+1) .GT. LTIGrid(giAgg)*y) .OR. ( (DoInterestCap(giAgg) .eq. 1) .and. (qm_e(bc,lc,mc,hc,giExo,giPh,gij+1) .lt. InterestCap)) .OR. ( (RationInc(giAgg) .eq. 1) .and. (y .lt. RationIncLevel) )) then
                                 qm(bc,lc,mc,hc,giExo,giPh,gij+1)=0.0d0
                              else

                                if(OnlyBankBelief .eq. 0) then
                                   qm(bc,lc,mc,hc,giExo,giPh,gij+1)=qm_e(bc,lc,mc,hc,giExo,giPh,gij+1)-Markupgrid(giAgg)
                                else
                                   qm(bc,lc,mc,hc,giExo,giPh,gij+1)=qm_e_exo(bc,lc,mc,hc,giExo,giPh,gij+1)-Markupgrid(giAgg)
                                endif

                              endif
                              if( ((GridM(mc)+GridL(lc)) .GT. HELOCGrid(giAgg)*gPh) .OR. ((GridM(mc)+GridL(lc)) .GT. LTIGrid(giAgg)*y) ) ql(bc,lc,mc,hc,giExo,giPh,gij+1)=0.0d0

                              if((GSE .eq. 1) .AND. (giW .lt. ngpzy/2+1) .and. (giC .lt. ngpC) .and. (gij .LT. 6))  qm(bc,lc,mc,hc,giExo,giPh,gij+1)=min(1.0d0,0.02d0+qm(bc,lc,mc,hc,giExo,giPh,gij+1))
                           enddo
                        enddo


                     enddo
                  enddo

                do bc=1,nb
                    EVrent(bc,gij)=0.0d0
                    do giAggp = 1,ngpAgg
                        do giWp = 1, ngpW
                            giExop = (giAggp-1)*ngpW+giWp
                            EVrent(bc,gij)=EVrent(bc,gij)+zytrans(gij,zWind(giW),zWind(giWp))*transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*Vrent(bc,giExop,PtoP1(giPh,giAgg,giAggp),gij+1)+PtoV2(giPh,giAgg,giAggp)*Vrent(bc,giExop,PtoP2(giPh,giAgg,giAggp),gij+1))
                        enddo
                    enddo
                enddo
            else

                giR = giW
                do hc=1,nh
                    do mc=1,nm
                        do lc=1,nl
                        do bc=1,nb
                            qmret_e(bc,lc,mc,hc,giExo,giPh,1)=0.0d0
                            qlret(bc,lc,mc,hc,giExo,giPh,1)=0.0d0
                            EVown(bc,lc,mc,hc,gij)=0.0d0
                            do giAggp = 1,ngpAgg
                                giExop = (giAggp-1)*ngpR+giR

                                EVown(bc,lc,mc,hc,gij) = EVown(bc,lc,mc,hc,gij)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*VownRet(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp),1)+PtoV2(giPh,giAgg,giAggp)*VownRet(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp),1))
                                ! qmret_e(bc,lc,mc,hc,giExo,giPh,1)=qmret_e(bc,lc,mc,hc,giExo,giPh,1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))

                                if(NoBankBelief .eq. 0 .or. AtoD(giAggp) .eq. ngpHD) then

                                   qmret_e(bc,lc,mc,hc,giExo,giPh,1)=qmret_e(bc,lc,mc,hc,giExo,giPh,1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))

                                else
                                         tempgiAggp=CHZYtoA(AtoC(giAggp),ngpHD,AtoZ(giAggp),AtoY(giAggp))
                                         tempgiExo = (tempgiAggp - 1)*ngpW + giW

                                   qmret_e(bc,lc,mc,hc,giExo,giPh,1)=qmret_e(bc,lc,mc,hc,giExo,giPh,1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,tempgiExo,PtoP1(giPh,giAgg,tempgiAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_mort(bc,lc,mc,hc,tempgiExo,PtoP2(giPh,giAgg,tempgiAggp)))

                                endif


                                qlret(bc,lc,mc,hc,giExo,giPh,1)=qlret(bc,lc,mc,hc,giExo,giPh,1)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*bankgain_loc(bc,lc,mc,hc,giExop,PtoP1(giPh,giAgg,giAggp))+PtoV2(giPh,giAgg,giAggp)*bankgain_loc(bc,lc,mc,hc,giExop,PtoP2(giPh,giAgg,giAggp)))
                             enddo
                             qmret_e(bc,lc,mc,hc,giExo,giPh,1) = min(1.0d0,qmret_e(bc,lc,mc,hc,giExo,giPh,1))


                             if( ( (GridM(mc) .GT. LTVGrid(giAgg)*gPh) .AND. (DoMqm .eq. 0)) .OR. ( (qmret_e(bc,lc,mc,hc,giExo,giPh,1)*GridM(mc) .GT. LTVGrid(giAgg)*gPh) .AND. (DoMqm .eq. 1))  .OR. (FnPm(GridM(mc)*GridH(hc),gij+1) .GT. LTIGrid(giAgg)*y) .OR.( (DoInterestCap(giAgg) .eq. 1) .and. (qmret_e(bc,lc,mc,hc,giExo,giPh,1) .lt. InterestCap)) .OR. ( (RationInc(giAgg) .eq. 1) .and. (y .lt. RationIncLevel) ) ) then
                                qmret(bc,lc,mc,hc,giExo,giPh,1)=0.0d0
                            else
                                if(OnlyBankBelief .eq. 0) then
                                   qmret(bc,lc,mc,hc,giExo,giPh,1)=qmret_e(bc,lc,mc,hc,giExo,giPh,1)-Markupgrid(giAgg)
                                else
                                   qmret(bc,lc,mc,hc,giExo,giPh,1)=qmret_e_exo(bc,lc,mc,hc,giExo,giPh,1)-Markupgrid(giAgg)
                                endif

                              endif
                           if( ((GridM(mc)+GridL(lc)) .GT. HELOCGrid(giAgg)*gPh) .OR. ((GridM(mc)+GridL(lc)) .GT. LTIGrid(giAgg)*y) ) qlret(bc,lc,mc,hc,giExo,giPh,1) = 0.0d0



                            qm(bc,lc,mc,hc,giExo,giPh,gij+1)=qmret(bc,lc,mc,hc,giExo,giPh,1)
                            qm_e(bc,lc,mc,hc,giExo,giPh,gij+1)=qmret_e(bc,lc,mc,hc,giExo,giPh,1)
                            ql(bc,lc,mc,hc,giExo,giPh,gij+1)=qlret(bc,lc,mc,hc,giExo,giPh,1)
                         enddo
                      enddo
                   enddo
                enddo

                do bc=1,nb
                    EVrent(bc,gij)=0.0d0
                    do giAggp = 1,ngpAgg
                        giExop = (giAggp-1)*ngpR+giR
                        EVrent(bc,gij)=EVrent(bc,gij)+transMatAgg(giAgg,giAggp)*(PtoV1(giPh,giAgg,giAggp)*VrentRet(bc,giExop,PtoP1(giPh,giAgg,giAggp),1)+PtoV2(giPh,giAgg,giAggp)*VrentRet(bc,giExop,PtoP2(giPh,giAgg,giAggp),1))
                    enddo
                enddo

            endif
            do hc=1,nh
                do mc=1,nm
                    do lc=1,nl
                    do bc=1,nb
                        funds(bc,lc,mc,hc)=(qm(bc,lc,mc,hc,giExo,giPh,gij+1)*GridM(mc)+ql(bc,lc,mc,hc,giExo,giPh,gij+1)*GridL(lc))*GridH(hc)-FnQb(GridB(bc),(Ph-GridM(mc))*GridH(hc))*GridB(bc)-Ph*GridH(hc)
                    enddo
                    enddo
                enddo
            enddo
            maxfunds=maxval(funds)


            EEVown(:,:,:,:,gij,giExo,giPh)=EVown(:,:,:,:,gij)
            EEVrent(:,gij,giExo,giPh)=EVrent(:,gij)

            do bc = 1,nb

                bj=GridB(bc)

                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                !First solve for the renter's problem
                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



                do hcc=1,nhr
                    index=hcc
                    gliq=bj+y-FnTax(y)-Pr*GridHR(hcc)+Trans(giPh)
                    bccmax=(gliq-cmin)*(1.0d0+rf)
                    bccmin=0.0d0
                    bccmid=(bccmax+bccmin)/2.0d0


                    gih = hcc
                    ftemp1=FnVFR(bccmin)
                    ftemp2=FnVFR(GridB(2))
                    if(bccmax .gt. bccmin) then
                        if(ftemp2<ftemp1) then
                            ftemp1=golden(bmin,bccmid,bccmax,FnVFR,vftol,btemp)
                            bR(index)=btemp
                            consR(index)=gliq-FnQb(btemp,0.0d0)*btemp
                        else
                            bR(index)=0.0d0
                            consR(index)=gliq
                        endif
                    else
                        helpWR(index)=Wmin
                        consR(index)=cmin
                        bR(index)=0.0d0
                    endif

                    helpWR(index)=-ftemp1
                    hconsR(index)=GridHR(hcc)
                enddo


                !maximize over the portfolio choice
                polind=maxloc(helpWR(1:nhr),dim=1)

                !update the value and policy functions
                Wrent(bc,giExo,giPh,gij)=helpWR(polind)
                consRpol(bc,giExo,giPh,gij)=consR(polind)
                hconsRpol(bc,giExo,giPh,gij)=hconsR(polind)
                bRpol(bc,giExo,giPh,gij)=bR(polind)
                mRpol(bc,giExo,giPh,gij)=0.0d0
                hRpol(bc,giExo,giPh,gij)=0.0d0

                if(consRpol(bc,giExo,giPh,gij) .lt. cmin) then
                   consRpol(bc,giExo,giPh,gij)=cmin
                   hconsRpol(bc,giExo,giPh,gij)=GridHR(1)
                   bRpol(bc,giExo,giPh,gij)=0.0d0
                   mRpol(bc,giExo,giPh,gij)=0.0d0
                   hRpol(bc,giExo,giPh,gij)=0.0d0
                   Wrent(bc,giExo,giph,gij)=Wmin


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
                            gliq=bj+y-FnTax(y)-Ph*GridH(hcc)-fc+Trans(giPh)
                            ftemp1=FnVFB(bmin)
                            ftemp2=FnVFB(GridB(2))
                            bccmax=bj+y-FnTax(y)-fc+Trans(giPh)

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

                            call basefun(GridB,nb,btemp,vals,inds)

                            consB(index)=gliq+(vals(1)*(qm(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gmort+ql(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gloc)+(vals(2)*(qm(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gmort+ql(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gloc)))*ghouse-FnQb(btemp,(gPh-gmort)*ghouse)*btemp
                            helpWB(index)=-ftemp1

                        enddo
                        fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                    enddo
                enddo
                polind=maxloc(helpWB(1:nh*nm*nl),dim=1)

                !update the value and policy functions
                Wbuy(bc,giExo,giPh,gij)       =   helpWB(polind)
                consBpol(bc,giExo,giPh,gij)   =   consB(polind)
                mBpol(bc,giExo,giPh,gij)      =   GridM(mB(polind))
                bBpol(bc,giExo,giPh,gij)      =   bB(polind)
                hBpol(bc,giExo,giPh,gij)      =   GridH(hB(polind))
                hconsBpol(bc,giExo,giPh,gij)  =   GridH(hB(polind))
                hIBpol(bc,giExo,giPh,gij)     =   hB(polind)
                lBpol(bc,giExo,giPh,gij)      =   GridL(lB(polind))
                lIBpol(bc,giExo,giPh,gij)     =   lB(polind)
                mIBpol(bc,giExo,giPh,gij)     =   mB(polind)

                 if(consBpol(bc,giExo,giPh,gij) .lt. cmin) then
                    if(Display==1) then
                        print*,'Negative consumption buyer'
                        print*,bc
                        print*,giExo,giPh,gij
                    endif
                    Wbuy(bc,giExo,giPh,gij)       = Wmin
                    consBpol(bc,giExo,giPh,gij)   =   cmin
                    mBpol(bc,giExo,giPh,gij)      =   0.0d0 !GridM(1)
                    bBpol(bc,giExo,giPh,gij)      =   0.0d0 !GridB(1)
                    hBpol(bc,giExo,giPh,gij)      =   GridH(1)
                    hconsBpol(bc,giExo,giPh,gij)  =   GridH(1)
                    hIBpol(bc,giExo,giPh,gij)     =   1
                    lBpol(bc,giExo,giPh,gij)      =   GridL(1)
                    lIBpol(bc,giExo,giPh,gij)     =   1


                endif


                !Solve whether current renter prefers to rent or buy


                if(Wrent(bc,giExo,giPh,gij) .gt. Wbuy(bc,giExo,giPh,gij)) then
                    Vrent(bc,giExo,giPh,gij)          =   Wrent(bc,giExo,giPh,gij)
                    buyRentpol(bc,giExo,giPh,gij)     =   0
                else
                    Vrent(bc,giExo,giPh,gij)          =   Wbuy(bc,giExo,giPh,gij)
                    buyRentpol(bc,giExo,giPh,gij)     =   1
                endif


            end do

            do bc=1,nbs


                bj=GridBS(bc)



                if(bj+y-FnTax(y)-Pr*GridHR(1) .gt. cmin) then


                    do hcc=1,nhr
                        index=hcc
                        gliq=bj+y-FnTax(y)-Pr*GridHR(hcc)+Trans(giPh)
                        bccmax=(gliq-cmin)*(1.0d0+rf)
                        gih = hcc
                        ftemp1=FnVFR(0.0d0)
                        ftemp2=FnVFR(GridB(2))
                        if(ftemp2<ftemp1) then
                            ftemp1=golden(0.0d0,GridB(2),bccmax,FnVFR,vftol,btemp)
                            bR(index)=btemp
                            consR(index)=gliq-FnQb(btemp,0.0d0)*btemp
                        else
                            bR(index)=0.0d0
                            consR(index)=gliq
                        endif
                        helpWR(index)=-ftemp1
                        hconsR(index)=GridHR(hcc)
                    enddo


                    !maximize over the portfolio choice
                    polind=maxloc(helpWR(1:nhr),dim=1)

                    !update the value and policy functions
                    WrentS(bc,giExo,giPh,gij)=helpWR(polind)
                    consRpolS(bc,giExo,giPh,gij)=consR(polind)
                    hconsRpolS(bc,giExo,giPh,gij)=hconsR(polind)
                    bRpolS(bc,giExo,giPh,gij)=bR(polind)
                    mRpolS(bc,giExo,giPh,gij)=0.0d0
                    hRpolS(bc,giExo,giPh,gij)=0.0d0

                    !print*,bRpol(bc,giExo,giPh,gij),consRpol(bc,giExo,giPh,gij)
                    if(consRpolS(bc,giExo,giPh,gij) .lt. cmin) then
                       consRpolS(bc,giExo,giPh,gij)=cmin
                       hconsRpolS(bc,giExo,giPh,gij)=GridHR(1)
                       bRpolS(bc,giExo,giPh,gij)=0.0d0
                       mRpolS(bc,giExo,giPh,gij)=0.0d0
                       hRpolS(bc,giExo,giPh,gij)=0.0d0
                       WrentS(bc,giExo,giph,gij)=Wmin
                    endif
                else

                   consRpolS(bc,giExo,giPh,gij)=cmin
                   hconsRpolS(bc,giExo,giPh,gij)=GridHR(1)
                   bRpolS(bc,giExo,giPh,gij)=0.0d0
                   mRpolS(bc,giExo,giPh,gij)=0.0d0
                   hRpolS(bc,giExo,giPh,gij)=0.0d0
                   WrentS(bc,giExo,giph,gij)=Wmin


                endif

                if(bj+y+maxfunds .gt. cmin) then

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
                                gliq=bj+y-FnTax(y)-Ph*GridH(hcc)-fc-FnMaint(GridH(hcc))+Trans(giPh)
                                bccmax=max(bj+y-FnTax(y)-fc+Trans(giPh),1.0d-4)

                                !Changed 10/13/15 to prevent new home buyers from using HELOCs for purchase
                                if(giC .eq. ngpC) then
                                   bccmin=0.0d0
                                elseif(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif



                                bccmid=(bccmax+bccmin)/2.0d0



                                ftemp1=FnVFB(0.0d0)
                                ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)

                                if(ftemp2<ftemp1) then
                                    ftemp1=ftemp2
                                else
                                    btemp=0.0d0
                                endif
                                bB(index)=btemp
                                call basefun(GridB,nb,btemp,vals,inds)

                                consB(index)=gliq+(vals(1)*(qm(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gmort+ql(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gloc)+(vals(2)*(qm(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gmort+ql(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gloc)))*ghouse-FnQb(btemp,0.0d0)*btemp
                                helpWB(index)=-ftemp1

                            enddo
                            fc=FCGrid(giAgg) !Set fixed cost of mortgage for when mcc>1
                        enddo
                    enddo
                    polind=maxloc(helpWB,dim=1)

                    !update the value and policy functions
                    WbuyS(bc,giExo,giPh,gij)       =   helpWB(polind)
                    consBpolS(bc,giExo,giPh,gij)   =   consB(polind)
                    mBpolS(bc,giExo,giPh,gij)      =   GridM(mB(polind))
                    bBpolS(bc,giExo,giPh,gij)      =   bB(polind)
                    hBpolS(bc,giExo,giPh,gij)      =   GridH(hB(polind))
                    hconsBpolS(bc,giExo,giPh,gij)  =   GridH(hB(polind))
                    hIBpolS(bc,giExo,giPh,gij)     =   hB(polind)
                    lBpolS(bc,giExo,giPh,gij)      =   GridL(lB(polind))
                    lIBpolS(bc,giExo,giPh,gij)     =   lB(polind)
                    mIBpolS(bc,giExo,giPh,gij)     =   mB(polind)

                     if(consBpolS(bc,giExo,giPh,gij) .lt. cmin) then

                        WbuyS(bc,giExo,giPh,gij)       = Wmin
                        consBpolS(bc,giExo,giPh,gij)   =   cmin
                        mBpolS(bc,giExo,giPh,gij)      =   GridM(1)
                        bBpolS(bc,giExo,giPh,gij)      =   0.0d0
                        hBpolS(bc,giExo,giPh,gij)      =   GridH(1)
                        hconsBpolS(bc,giExo,giPh,gij)  =   GridH(1)
                        hIBpolS(bc,giExo,giPh,gij)     =   1
                        lBpolS(bc,giExo,giPh,gij)      =   GridL(1)
                        lIBpolS(bc,giExo,giPh,gij)     =   1


                    endif




                else

                    WbuyS(bc,giExo,giPh,gij)       = Wmin
                    consBpolS(bc,giExo,giPh,gij)   =   cmin
                    mBpolS(bc,giExo,giPh,gij)      =   GridM(1)
                    bBpolS(bc,giExo,giPh,gij)      =   0.0d0 !GridB(1)
                    hBpolS(bc,giExo,giPh,gij)      =   GridH(1)
                    hconsBpolS(bc,giExo,giPh,gij)  =   GridH(1)
                    hIBpolS(bc,giExo,giPh,gij)     =   1
                    lBpolS(bc,giExo,giPh,gij)      =   GridL(1)
                    lIBpolS(bc,giExo,giPh,gij)     =   1
                    mIBpolS(bc,giExo,giPh,gij)     =   1




                endif







            enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Next solve for homeowner's problems
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



            if(gij .gt. 1) then
            do hc=1,nh
                do mc=1,nm
                    do lc = 1,nl
                    do bc = 1,nb
                        bj=GridB(bc)
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Start off with the sellers problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        !Compute value of liquid assets after the sale
                        bjp=bj+GridH(hc)*(Ph-GridM(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))-FnMaint(GridH(hc))
                        bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj)))

                        !Need to check it the budget set is empty conditional on selling
                        if(bjps .gt. GridBS(1)) then

                            if(bjps .lt. GridB(1)) then
                                call basefun(GridBS,nbs,bjps,vals,inds)
                                Wsellbuy=vals(1)*WbuyS(inds(1),giExo,giPh,gij)+vals(2)*WbuyS(inds(2),giExo,giPh,gij)
                                Wsellrent=vals(1)*WrentS(inds(1),giExo,giPh,gij)+vals(2)*WrentS(inds(2),giExo,giPh,gij)

                            else
                                call basefun(GridB,nb,bjps,vals,inds)
                                Wsellbuy=vals(1)*Wbuy(inds(1),giExo,giPh,gij)+vals(2)*Wbuy(inds(2),giExo,giPh,gij)
                                Wsellrent=vals(1)*Wrent(inds(1),giExo,giPh,gij)+vals(2)*Wrent(inds(2),giExo,giPh,gij)

                            endif
                        else
                            Wsellbuy=Wmin
                            Wsellrent=Wmin
                        endif




                        !maximize over the portfolio choice

                        !update the value and policy functions
                        Wsell(bc,lc,mc,hc,giExo,giPh,gij)    =   max(Wsellbuy,Wsellrent)


                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve foreclosure problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        !Only have to maximize over c and b
                        gliq=bj+y-FnTax(y)-Pr*GridHR(1)+Trans(giPh)
                        bccmax=(gliq-cmin)*(1.0d0+rf)
                        bccmin=0.0d0
                        bccmid=(bccmax+bccmin)/2.0d0

                        ghouse = GridH(hc)
                        if(bccmax .gt. bccmin) then
                            ftemp1=FnVFF(bccmin)
                            ftemp2=golden(bccmin,bccmid,bccmax,FnVFF,vftol,btemp)
                            if(ftemp2<ftemp1) then
                                ftemp1=ftemp2
                            else
                                btemp=0.0d0
                            endif
                        else
                            ftemp1=-Wminfore
                            btemp=0.0d0
                        endif


                        !update the value and policy functions
                        Wfore(bc,lc,mc,hc,giExo,giPh,gij)=-ftemp1
                        consFpol(bc,lc,mc,hc,giExo,giPh,gij)=gliq-FnQb(btemp,0.0d0)*btemp
                        hconsFpol(bc,lc,mc,hc,giExo,giPh,gij)=GridHR(1)
                        bFpol(bc,lc,mc,hc,giExo,giPh,gij)=btemp
                        mFpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                        hFpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0

                        if(consFpol(bc,lc,mc,hc,giExo,giPh,gij) .lt. cmin) then
                            if(Display==1) then
                                print*,'Negative consumption foreclosure'
                                print*,bc,lc,mc,hc
                                print*,giExo,giPh,gij
                                print*,helpWF
                                print*,consF
                            endif

                            consFpol(bc,lc,mc,hc,giExo,giPh,gij)=cminfore
                            Wfore(bc,lc,mc,hc,giExo,giPh,gij)=Wminfore
                            bFpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            mFpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            hFpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0

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
                                gliq=bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-(GridM(mc)*(1.0d0+rm)+GridL(lc)*(1.0d0+rl))*GridH(hc)-fc-FnMaint(GridH(hc))+Trans(giPh)
                                bccmax=(gliq+Ph*ghouse)*(1.0d0+rf)
                                if(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif
                                bccmid=(bccmax+bccmin)/2.0d0
                                ftemp1=FnVFB(0.0d0)
                                ftemp2=golden(bccmin,bccmid,bccmax,FnVFB,vftol,btemp)

                                if(ftemp2<ftemp1) then
                                    ftemp1=ftemp2
                                else
                                    btemp=0.0d0
                                endif
                                call basefun(GridB,nb,btemp,vals,inds)

                                consN(index)=gliq+(vals(1)*(qm(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gmort+ql(inds(1),gil,gim,gih,giExo,giPh,gij+1)*gloc)+(vals(2)*(qm(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gmort+ql(inds(2),gil,gim,gih,giExo,giPh,gij+1)*gloc)))*ghouse-FnQb(btemp,0.0d0)*btemp
                                helpWN(index)=-ftemp1
                                bN(index)=btemp
                                mN(index)=mcc
                                lN(index)=lcc

                            enddo
                            enddo


                            !maximize over the portfolio choice

                            polind=maxloc(helpWN(1:(nm-max(mc,2)+1)*nl),dim=1)

                            !update the value and policy functions
                            Wrefin(bc,lc,mc,hc,giExo,giPh,gij)=helpWN(polind)
                            consNpol(bc,lc,mc,hc,giExo,giPh,gij)=consN(polind)
                            hconsNpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                            bNpol(bc,lc,mc,hc,giExo,giPh,gij)=bN(polind)
                            mNpol(bc,lc,mc,hc,giExo,giPh,gij)=GridM(mN(polind))
                            hNpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                            lNpol(bc,lc,mc,hc,giExo,giPh,gij)=GridL(lN(polind))
                            lINpol(bc,lc,mc,hc,giExo,giPh,gij)=lN(polind)
                            mINpol(bc,lc,mc,hc,giExo,giPh,gij)=mN(polind)

                            if(consNpol(bc,lc,mc,hc,giExo,giPh,gij) .lt. cmin) then
                                if(Display==1) then
                                    print*,'Negative consumption refi'
                                    print*,bc,lc,mc,hc
                                    print*,giExo,giPh,gij
                                endif
                                Wrefin(bc,lc,mc,hc,giExo,giPh,gij)=Wmin
                                consNpol(bc,lc,mc,hc,giExo,giPh,gij)=cmin
                                bNpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                                mNpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                                hNpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)
                                lNpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                                lINpol(bc,lc,mc,hc,giExo,giPh,gij)=1
                                mINpol(bc,lc,mc,hc,giExo,giPh,gij)=1

                                !pause
                            endif



                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve mortgage payment problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        !Solve for the new mortgage balance
                        pm=FnPm(GridM(mc)*GridH(hc),gij)
                        mp=(GridM(mc)*GridH(hc))*(1.0d0+rm)-pm

                        !Interpolate that onto the mortage grid
                        call basefun(GridM,nm,mp/GridH(hc),gvals,ginds)

                        do lcc=1,nl
                            index=lcc
                            gih=hc
                            gil=lcc
                            ghouse=GridH(gih)
                            gmort=mp
                            gloc=GridL(gil)
                            gliq=bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-pm-GridL(lc)*ghouse*(1.0d0+rl)-FnMaint(GridH(hc))+Trans(giPh)
                            bccmax=gliq*(1.0d0+rf)
                                if(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif
!                            bccmin=min(-HELOCGrid(giAgg)*(gPh*ghouse-gmort),0.0d0)
                            bccmid=(bccmax+bccmin)/2.0d0
                            ftemp1=FnVFN(0.0d0)
                            ftemp2=golden(bmin,GridB(2),bccmax,FnVFN,vftol,btemp)


                            if(ftemp2<ftemp1) then
                                ftemp1=ftemp2
                            else
                                btemp=0.0d0
                            endif
                            call basefun(GridB,nb,btemp,vals,inds)


                            consP(index)=gliq+(vals(1)*sum(gvals*ql(inds(1),gil,ginds,gih,giExo,giPh,gij+1))+vals(2)*sum(gvals*ql(inds(2),gil,ginds,gih,giExo,giPh,gij+1)))*gloc*ghouse-FnQb(btemp,Ph*GridH(hc)-mp)*btemp
                            helpWP(index)=-ftemp1
                            bP(index)=btemp
                            lP(index)=lcc
                            mPp(index)=mp

                        enddo


                        mcc=1
                        do while(((GridM(mc)*GridH(hc))*(1.0d0+rm)-pm .GT. (GridM(mcc)*GridH(hc)) .AND. mcc .LT. mc))! .OR. (mcc .EQ. 1))
                            do lcc=1,nl
                                index=mcc*nl+lcc
                                gih=hc
                                gil=lcc
                                gim=mcc
                                ghouse=GridH(gih)
                                gmort=GridM(mcc)
                                gloc=GridL(gil)
                                gliq=bj+y-FnTaxM(y,GridH(hc)*(GridM(mc)+GridL(lc)),min(0.0d0,bj))-GridL(lc)*ghouse*(1.d0+rl)+((GridM(mcc)*GridH(hc))-(GridM(mc)*GridH(hc))*(1.0d0+rm))-FnMaint(GridH(hc))+Trans(giPh)
                                bccmax=gliq*(1.0d0+rf)
                                if(HELOCVal .eq. 1) then
                                   bccmin=-HELOCGrid(giAgg)*gPh*ghouse
                                else
                                   bccmin=min(-HELOCGrid(giAgg)*((gPh-gmort)*ghouse),0.0d0)
                                endif
!                                bccmin=min(-HELOCGrid(giAgg)*(gPh-gmort)*ghouse,0.0d0)
                                bccmid=(bccmax+bccmin)/2.0d0


                                ftemp1=FnVFP(0.0d0)
                                ftemp2=golden(bccmin,bccmid,bccmax,FnVFP,vftol,btemp)


                                if(ftemp2<ftemp1) then
                                    ftemp1=ftemp2

                                else
                                    btemp=0.0d0
                                endif
                                call basefun(GridB,nb,btemp,vals,inds)

                                consP(index)=gliq+(vals(1)*ql(inds(1),gil,gim,gih,giExo,giPh,gij+1)+vals(2)*ql(inds(2),gil,gim,gih,giExo,giPh,gij+1))*gloc*ghouse-FnQb(btemp,0.0d0)*btemp
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
                        Wpay(bc,lc,mc,hc,giExo,giPh,gij)=helpWP(polind)
                        consPpol(bc,lc,mc,hc,giExo,giPh,gij)=consP(polind)
                        hconsPpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                        bPpol(bc,lc,mc,hc,giExo,giPh,gij)=bP(polind)
                        mPpol(bc,lc,mc,hc,giExo,giPh,gij)=mPp(polind)
                        hPpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(hc)
                        lPpol(bc,lc,mc,hc,giExo,giPh,gij)=GridL(lP(polind))
                        lIPpol(bc,lc,mc,hc,giExo,giPh,gij)=lP(polind)

                        if(consPpol(bc,lc,mc,hc,giExo,giPh,gij) .lt. cmin) then
                            if(Display==1) then
                                print*,'Negative consumption pay'
                                print*,bc,lc,mc,hc
                                print*,giExo,giPh,gij
                            endif
                            Wpay(bc,lc,mc,hc,giExo,giPh,gij)=Wmin
                            consPpol(bc,lc,mc,hc,giExo,giPh,gij)=cmin
                            hconsPpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)
                            bPpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            mPpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            hPpol(bc,lc,mc,hc,giExo,giPh,gij)=GridH(1)
                            lPpol(bc,lc,mc,hc,giExo,giPh,gij)=0.0d0
                            lIPpol(bc,lc,mc,hc,giExo,giPh,gij)=1

                            !pause
                        endif



                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve forced move problem
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        !See if better to foreclose or sell
                        if(Wfore(bc,lc,mc,hc,giExo,giPh,gij) .gt. Wsell(bc,lc,mc,hc,giExo,giPh,gij)) then
                            VownMove(bc,lc,mc,hc,giExo,giPh,gij)         =   Wfore(bc,lc,mc,hc,giExo,giPh,gij)
                            if(gij>1) then

                                if(mc .GT. 1) then
                                   if(gij .le. 10 .and. (giW .lt. (ngpzy/2))) then
                                      bankgain_mort_move(bc,lc,mc,hc,giExo,giPh)=min(1.0d0,1.0d0+(gam*gPh/(GridM(mc)*(1.0d0+rm))-1.0d0)*WedgeGrid(giAgg))
                                   else
                                      bankgain_mort_move(bc,lc,mc,hc,giExo,giPh)=min(1.0d0,1.0d0+(gam*gPh/(GridM(mc)*(1.0d0+rm))-1.0d0))
                                   endif
                                else
                                    bankgain_mort_move(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                endif
                                if(lc .GT. 1) then
                                    bankgain_loc_move(bc,lc,mc,hc,giExo,giPh) = max(0.0d0,min(1.0d0,(gam*gPh-GridM(mc)*(1.0d0+rm))/(GridL(lc)*(1.0d0+rl))))
                                else
                                    bankgain_loc_move(bc,lc,mc,hc,giExo,giPh) = 0.0d0
                                endif
                            endif

                        else
                            VownMove(bc,lc,mc,hc,giExo,giPh,gij)         =   Wsell(bc,lc,mc,hc,giExo,giPh,gij)
                            if(gij>1) then
                                bankgain_mort_move(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                bankgain_loc_move(bc,lc,mc,hc,giExo,giPh) = 1.0d0

                            endif
                        endif




                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        !Now solve problem where you can stay in your house
                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                        helpVals(1) =   Wfore(bc,lc,mc,hc,giExo,giPh,gij)
                        helpVals(2) =   Wsell(bc,lc,mc,hc,giExo,giPh,gij)
                        helpVals(3) =   Wrefin(bc,lc,mc,hc,giExo,giPh,gij)
                        helpVals(4) =   Wpay(bc,lc,mc,hc,giExo,giPh,gij)


                        polind=maxloc(helpVals,dim=1)


                        select case(polind)

                            case(1) !Foreclosure optimal

                                VownStay(bc,lc,mc,hc,giExo,giPh,gij)         =   Wfore(bc,lc,mc,hc,giExo,giPh,gij)
                                if(gij>1) then
                                    if(mc .GT. 1) then
                                       if(gij .le. 10 .and. (giW .lt. (ngpzy/2))) then
                                          bankgain_mort(bc,lc,mc,hc,giExo,giPh)=min(1.0d0,1.0d0+(gam*gPh/(GridM(mc)*(1.0d0+rm))-1.0d0)*WedgeGrid(giAgg))
                                       else
                                          bankgain_mort(bc,lc,mc,hc,giExo,giPh)=min(1.0d0,1.0d0+(gam*gPh/(GridM(mc)*(1.0d0+rm))-1.0d0))
                                       endif
                                    else
                                        bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                    endif
                                    if(lc .GT. 1) then
                                        bankgain_loc(bc,lc,mc,hc,giExo,giPh) = max(0.0d0,min(1.0d0,(gam*gPh-GridM(mc)*(1.0d0+rm))/(GridL(lc)*(1.0d0+rl))))
                                    else
                                        bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 0.0d0
                                    endif

                                endif


                            case(2) !Selling optimal

                                VownStay(bc,lc,mc,hc,giExo,giPh,gij)         =   Wsell(bc,lc,mc,hc,giExo,giPh,gij)

                                if(gij>1) then
                                    bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                    bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                endif

                            case(3) !Refinance optimal

                                VownStay(bc,lc,mc,hc,giExo,giPh,gij)         =   Wrefin(bc,lc,mc,hc,giExo,giPh,gij)


                                if(gij>1) then
                                    bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                    bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                endif
                            case(4) !Payment optimal

                                VownStay(bc,lc,mc,hc,giExo,giPh,gij)         =   Wpay(bc,lc,mc,hc,giExo,giPh,gij)

                                mp=mPpol(bc,lc,mc,hc,giExo,giPh,gij)
                                pm=(GridM(mc)*GridH(hc))*(1.0d0+rm)-mp

                                call basefun(GridM,nm,mp/GridH(hc),vals,inds)
                                if(gij>1) then
                                    bankgain_loc(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                    if(mc .GT. 1) then
                                        bankgain_mort(bc,lc,mc,hc,giExo,giPh)=(pm+mp*sum(vals(:)*qm_e(bc,lc,inds(:),hc,giExo,giPh,gij+1)))/(GridH(hc)*GridM(mc)*(1.0d0+rm))
                                    else
                                        bankgain_mort(bc,lc,mc,hc,giExo,giPh) = 1.0d0
                                    endif
                                endif
                        end select

                        !Take expectation over moving shock

                        Vown(bc,lc,mc,hc,giExo,giPh,gij)=(1.0d0-thet(gij))*VownStay(bc,lc,mc,hc,giExo,giPh,gij)+thet(gij)*VownMove(bc,lc,mc,hc,giExo,giPh,gij)

                     enddo
                  enddo
               enddo
            enddo
         endif
      enddo

!$OMP END PARALLEL DO
    qm(:,:,1,:,:,:,gij)=1.0d0
    qm_e(:,:,1,:,:,:,gij)=1.0d0
    ql(:,1,:,:,:,:,gij)=1.0d0


 enddo




end subroutine Decisions

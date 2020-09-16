!
!   SimulateStochGEDist.f90
!
!
!   Created by Kurt Mitman on 05/01/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

subroutine SimulateStochGEDist



use params
use globals
use funcs
use procedures
USE DFNLS, ONLY: BOBYQA_H

implicit none

EXTERNAL DFOVEC

integer :: nPhPr
DOUBLE PRECISION, EXTERNAL :: FnXSH
DOUBLE PRECISION, EXTERNAL :: FnXSdemand
double precision, allocatable,dimension(:,:,:,:) :: hconsdistrp
double precision, allocatable,dimension(:,:,:,:) :: howndistrp
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: hconsdistop
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: howndistop
double precision, allocatable,dimension(:,:,:,:) :: consdistrp
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: consdistop
double precision, allocatable,dimension(:,:) :: beqdistr
double precision, allocatable,dimension(:,:,:) :: taxsimr
double precision, allocatable,dimension(:,:,:,:,:) :: beqdisto
double precision, allocatable,dimension(:,:,:,:,:,:) :: taxsimo

double precision, allocatable,dimension(:,:,:,:) :: hconsRpolSim,consRpolSim,bRpolSim,hconsBpolSim,consBpolSim,hBpolSim,bBpolSim,mBpolSim,lBpolSim,WrentSim,WbuySim

double precision, allocatable,dimension(:,:,:,:,:,:,:) :: hconsPpolSim,consPpolSim,bPpolSim,mPpolSim,lPpolSim
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: hconsBpolOSim,consBpolOSim,bBpolOSim,mBpolOSim,lBpolOSim,hBpolOSim
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: hconsRpolOSim,consRpolOSim,bRpolOSim
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: hconsNpolSim,consNpolSim,bNpolSim,mNpolSim,lNpolSim
double precision, allocatable,dimension(:,:,:,:,:,:,:) :: hconsFpolSim,consFpolSim,bFpolSim

double precision, allocatable,dimension(:,:,:,:,:,:,:) :: WforeSim,WrefinSim,WpaySim,WsellbuySim,WsellrentSim

double precision, allocatable,dimension(:,:,:,:) :: pmsim
double precision, allocatable,dimension(:)       :: nw,nwdist,Si,Si1,hnw,hnwshare,hnwdist,hnwsharedist,liq,liqdist,ltv,nwy,nwydist,hy,hydist,ltvdist
integer, allocatable,dimension(:)                :: nworder,hnworder,hnwshareorder,liqorder,ltvorder,nwyorder,hyorder
double precision    :: gelas,tempsum1,tempsum2
double precision    :: Ph,Pr,cj2
double precision    :: phptemp(ngpAgg),ephtemp
double precision    :: pathEhconlow(Tsim),pathEhconhigh(Tsim)
integer     :: inds(2),indsm(2),indsS(2),Phinds(4),Phinds2(2),indsl(2),Phinds3(2),Phhinds(2),Phrinds(2)
double precision    ::  vals(2),valsm(2),valsS(2),Phvals(4),Phvals2(2),valsl(2),Phvals3(2),hyperc(5),Phhvals(2),Phrvals(2)

double precision    :: Ht,dummy,totmass

double precision,dimension(ngpPh)   ::Hdemand,Hsupply,Hownsupply
double precision     :: Wsellbuy,Wsellrent,helpWsell,helpWfore,helpWpay,helpWrefin,helpWbuy,helpWrent

!Variables that need to be private
INTEGER		:: ij,ijret,iW,iR,polind,hjI,index,mcc,hcc,lcc,ljI,ip,ify,izy,iPh,mc
integer :: bc,bcc,lc,hc,kcc,jcc,index1,index2
INTEGER :: it,iAgg,iindex,iAggp,posinitw,itstop,poslev
double precision    :: HelpVals(4)
double precision    :: btemp, y, pm, fc,temp,tempown,modifyval
double precision    :: bj,bjp,mj,mjp,hj,lj,bjps,phh,cj,nomortmeas
double precision    :: Demelas(Tsim)
double precision    :: PhPr(2)
double precision :: nunu,uChi_c,uChi_h
integer::iprint
double precision::ftol,scale
integer,parameter::nest=2
INTEGER,  PARAMETER  :: nmoments=2           ! Number of moments to be targeted
DOUBLE PRECISION :: itratio
INTEGER,  PARAMETER :: ninterppt = 2*nest+1
DOUBLE PRECISION, DIMENSION((ninterppt+5)*(ninterppt+nest)+3*nest*(nest+5)/2) :: wspace
INTEGER :: maxeval
DOUBLE PRECISION :: rhobeg,rhoend
INTEGER, PARAMETER :: nmax=nest+1 ! It should be bigger than the lengh of the vector function v_err(x).
INTEGER, PARAMETER :: mmax=nmoments+2 ! It should be bigger than the lengh of the vector function v_err(x).
INTEGER, PARAMETER ::nptmax=2*nmax+1
DOUBLE PRECISION  :: param_bound(nest,nest)
param_bound(1,1)=GridPh(1)
param_bound(1,2)=GridPh(ngpPhh)
param_bound(2,1)=GridPrPh(1)
param_bound(2,2)=GridPrPh(ngpPhr)


itratio = 0.0d0
rhobeg	= MINVAL(param_bound(:,2)-param_bound(:,1))/2.50d0
rhoend  = 1.0D-4
! WRITE(*,*) 'Rhobeg and Rhoend are', Rhobeg, Rhoend
maxeval = 40*(nest+1) ! max number of gradient evaluations
!CALLING THE MAIN MINIMIZATION ROUTINE (DERIVATIVE-FREE NON-LINEAR LEAST SQUARES)


allocate(pmsim(nmsim,nh,ngpW*ngpAgg,Jtot))

allocate(hconsRpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(consRpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(bRpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(hconsBpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(consBpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(bBpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(mBpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(lBpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(hBpolOSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))


allocate(WforeSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(WrefinSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(WpaySim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(WsellbuySim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(WsellrentSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))



allocate(hconsdistop(nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot,ngpPh))
allocate(hconsdistrp(nbsim,ngpW*ngpAgg,Jtot,ngpPh))
allocate(howndistop(nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot,ngpPh))
allocate(howndistrp(nbsim,ngpW*ngpAgg,Jtot,ngpPh))
allocate(consdistop(nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot,ngpPh))
allocate(consdistrp(nbsim,ngpW*ngpAgg,Jtot,ngpPh))
allocate(beqdisto(nbsim,nl,nmsim,nh,ngpW))
allocate(beqdistr(nbsim,ngpW))
allocate(taxsimo(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(taxsimr(nbsim,ngpW,Jtot))


allocate(hconsRpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(consRpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(bRpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(hconsBpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(consBpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(bBpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(mBpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(lBpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(hBpolSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))

allocate(WrentSim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))
allocate(WbuySim(ngpPh,nbsim,ngpW*ngpAgg,Jtot))


allocate(hconsPpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(consPpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(bPpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(mPpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(lPpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))


allocate(hconsNpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(consNpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(bNpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(mNpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(lNpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))

allocate(hconsFpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(consFpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))
allocate(bFpolSim(ngpPh,nbsim,nl,nmsim,nh,ngpW*ngpAgg,Jtot))


posinitw=1

gelas=1.04d0

iprint=0

scale=0.001d0
ftol=1.0d-4

if(exogenous .eq. 1) then
OPEN(3, FILE = trim(OutputDir) // 'distsso.txt')
DO ij=1,Jtot
DO giW=1,ngpW
DO hc=1,nh
DO mc=1,nmsim
DO lc=1,nl
DO bc=1,nbsim
   READ(3,*) dummy,dummy,dummy,dummy,dummy,dummy,dummy,Goldmine(:),distsso(bc,lc,mc,hc,giW,ij)
ENDDO
ENDDO
ENDDO
ENDDO
ENDDO
ENDDO
CLOSE(3)
OPEN(3, FILE = trim(OutputDir) // 'distssr.txt')
DO ij=1,Jtot
DO giW=1,ngpW
DO bc=1,nbsim
   READ(3,*) dummy,dummy,dummy,Goldmine(:),distssr(bc,giW,ij)
!   READ(3,*) GridBsim(bc),dble(giW),dble(ij),Goldmine(:),max(1.0d-16,distssr(bc,giW,ij))
ENDDO
ENDDO
ENDDO
CLOSE(3)

do ij=1,Jtot
   totmass = sum(distsso(:,:,:,:,:,ij))+sum(distssr(:,:,ij))

   distsso(:,:,:,:,:,ij)=distsso(:,:,:,:,:,ij)/totmass
   distssr(:,:,ij)=distssr(:,:,ij)/totmass

enddo


OPEN(3, FILE = trim(OutputDir) // 'pathHt.txt')

 do ij=1,98
    READ(3,*) Hsteady
 enddo

close(3)
endif








!distr=0.0d0
!disto=0.0d0
!distr(1,:,1)=zydist(1,:)
if(DoExu .eq. 0) then
   distr=distssr
   disto=distsso
   Ht=Hsteady !*.99d0 !*(1.0-deltah)
else
   distr=distssrexu
   disto=distssoexu
   Ht=Htexu !*.99d0 !*(1.0-deltah)
endif
ph=ssph(iAggsteady)
iAggpath=ngpAgg !iAggsteady
iindex=150
!if(ngpC .eq. 2 .and. ngpRf .eq. 2) then
!iAggpath=ngpAgg
!iAggpath(101:110)=1
!iAggpath(111:150)=4
!iAggpath(151:160)=1
!iAggpath(161:181)=2
!iAggpath(182:200)=4
!iAggpath(201:210)=1
!iAggpath(211:230)=3
!
!iindex=231
!endif
iAggpath=ngpAgg
iAggpath(101:105)=1
if(ngpAgg .eq. 36) then
   iAggpath=24
   iAggpath(101:105)=5
   iAggpath(106:107)=23 !35
   iAggpath(108:110)=23
   iAggpath(161:162)=10
   iAggpath(163:165)=5
   iAggpath(166:167)=23 !35
   iAggpath(168:170)=23
   iAggpath(221:225)=22
   iAggpath(241:250)=23
   iAggpath(291:295)=11
   iAggpath(296:297)=23 !35
   iAggpath(298:300)=23
   iAggpath(331:335)=20
   iAggpath(391:395)=12
!   iAggpath(396:397)=36
elseif(ngpAgg .eq. 12 .and. ngpRf .eq. 1) then
   iAggpath=ngpAgg
   iAggpath(31:60)=6
   iAggpath(81:90)=6


    if(modify .eq. 0) then
       !Benchmark
       iAggpath(101)=6
       iAggpath(102)=5
       iAggpath(103:105)=3
       if(DoRf .eq. 1)   iAggpath(106:110)=11

       iAggpath(161)=6
       iAggpath(162)=5
       iAggpath(163:165)=3
       iAggpath(166:176)=1
       if(DoRf .eq.1) iAggpath(166:170)=11

       iAggpath(201:202)=6
       iAggpath(203:205)=3
       if(DoRf .eq. 1) iAggpath(206:210)=11

    else
       iAggpath(101)=6
       iAggpath(102)=5
!        iAggpath(101:102)=5
        iAggpath(103:105)=3
        if(DoRf .eq. 1)   iAggpath(106:110)=11

!        iAggpath(161:162)=5
        iAggpath(163:165)=3
       iAggpath(161)=6
       iAggpath(162)=5
        if(DoRf .eq. 1)   iAggpath(166:170)=11

!        iAggpath(201:202)=5
       iAggpath(201)=6
       iAggpath(202)=5
        iAggpath(203:205)=3
        if(DoRf .eq. 1)   iAggpath(206:210)=11



    endif

    iAggpath(242:245)=11
    if(DoRf .eq. 1) iAggpath(246:250) = 11
    iAggpath(291:295)=6

!    iAggpath(361:362)=6
!    iAggpath(363:365)=4

!    iAggpath(421:422)=6
!    iAggpath(423:425)=5

!    iAggpath(421:422)=11
!    iAggpath(423:425)=9

   iAggpath(331:335)=5
   iAggpath(393:395)=10
   iAggpath(443:445)=8
elseif(ngpAgg .eq. 12 .and. ngpRf .eq. 2) then
   iAggpath=ngpAgg
   iAggpath(101:105)=3
   iAggpath(106:110)=11
   iAggpath(161:162)=5
   iAggpath(163:165)=3
   iAggpath(166:170)=11
   iAggpath(221:225)=11
   iAggpath(291:295)=6
   iAggpath(331:335)=5
   iAggpath(391:395)=10
elseif(ngpAgg .eq. 8) then
   iAggpath(131:135)=4
   iAggpath(161:165)=6
   iAggpath(191:195)=7
   iAggpath(241:245)=1
   iAggpath(246:250)=7
   iAggpath(331:335)=3
   iAggpath(336:340)=7
elseif(ngpAgg .eq. 6) then
   iAggpath=ngpAgg
   iAggpath(101:105)=2
!   iAggpath(106:107)=5
!   iAggpath(108:110)=3
   iAggpath(161:162)=3
   iAggpath(163:165)=2
   iAggpath(291:295)=3
   iAggpath(391:395)=5
elseif(ngpAgg .eq. 4) then
   iAggpath(101:105)=1
   iAggpath(106:110)=3
   iAggpath(161:162)=2
   iAggpath(163:165)=1
   iAggpath(166:170)=3
   iAggpath(291:295)=3
   iAggpath(221:225)=2
elseif(ngpAgg .eq. 3) then
   iAggpath=ngpAgg
   iAggpath(103:105)=2
else
   iAggpath(191:195)=1
endif
iindex=451

if(perfectcorr .eq. 1) then
iAggpath=1
iAggpath(101:103)=2
iAggpath(104:110)=3
iAggpath(111:150)=1
iAggpath(151:251)=2
iAggpath(252:260)=3
iAggpath(261:270)=1
iAggpath(271:280)=3
iAggpath(281:290)=1
iAggpath(291:300)=2
iindex=301
endif
it=Tsim-ngpAgg*ngpAgg*5
if(exogenous .eq. 0) then
  do iAgg=1,ngpAgg
     do iAggp=iAgg+1,ngpAgg
        iAggpath(it)=iAgg
        it=it+1
        iAggpath(it)=iAggp
        it=it+1
     enddo
     iAggpath(it)=iAgg
     it=it+1
     iAggpath(it)=iAgg
     it=it+1
     iAggpath(it)=iAgg
     it=it+1
  enddo
  itstop=it
else
  itstop=iindex
endif
hconsdistop=0.0d0
hconsdistrp=0.0d0
howndistop=0.0d0
howndistrp=0.0d0
consdistrp=cmin
consdistop=cmin

!First interpolate onto the simulation grid
do giPh=1,ngpPh
do giAgg=1,ngpAgg
do gij=1,Jtot

    giAY=AtoY(giAgg)
    giHD=AtoD(giAgg)
    giZh=AtoZ(giAgg)
    giC=AtoC(giAgg)
    giRf = AtoR(giAgg)

    rm=Rmgrid(giAgg)
    grm=rm
    Ph=Phgrid(giPh)
    gPh=Ph
    ij=gij
    ijret = ij-Jwork
    !Changed 11/20/15 to make grid on Rf on Agg for comovement
    rf=RfGrid(giAgg)
!    rf=GridRf(giRf)
    grf=rf
    rm=Rmgrid(giAgg)
    grm=rm
    rl=RlGrid(giAgg)
    grl=rl
    nunu = NuGrid(giAgg)
    Pr=Prgrid(giPh)
    uChi_c = 1.0d0-1.0d0/(1.0d0+(Pr**(alph/(1.0d0-alph)))*((nunu/(1.0d0-nunu))**(1.0d0/(1.0d0-alph))))
    !print*,'uChi_c',uChi_c
    !print*,'Pr',Pr
    !print*,'alph',alph
    !print*,'nunu',nunu
    uChi_h = ((1.0d0/(1.0d0+(Pr**(alph/(1.0d0-alph)))*((nunu/(1.0d0-nunu))**(1.0d0/(1.0d0-alph)))))/Pr) / uChi_c

    select case(gij)

        !Working people
        case(1:Jwork)


            do giW=1,ngpW
                do bc=1,nbsim
                    iW=giW
                    giExo=(giAgg-1)*ngpW+giW

                    !Check to see if stay renter or buy house
                    WrentSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*Wrent(bcinds(:,bc),giExo,giPh,gij))
                    WbuySim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*Wbuy(bcinds(:,bc),giExo,giPh,gij))

                    hconsRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hconsRpol(bcinds(:,bc),giExo,giPh,gij))
                    hconsBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hconsBpol(bcinds(:,bc),giExo,giPh,gij))

                    consRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*consRpol(bcinds(:,bc),giExo,giPh,gij))
                    consBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*consBpol(bcinds(:,bc),giExo,giPh,gij))

                    bRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*bRpol(bcinds(:,bc),giExo,giPh,gij))
                    bBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*bBpol(bcinds(:,bc),giExo,giPh,gij))

                    hBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hBpol(bcinds(:,bc),giExo,giPh,gij))
                    mBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*mBpol(bcinds(:,bc),giExo,giPh,gij))
                    lBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*lBpol(bcinds(:,bc),giExo,giPh,gij))

                    if(WrentSim(giPh,bc,giExo,gij) .gt. WbuySim(giPh,bc,giExo,gij)  ) then

                       if(GeRent .eq. 0) then

                          hconsdistrp(bc,giExo,gij,giPh)=0.0d0
                       else
                          hconsdistrp(bc,giExo,gij,giPh)=hconsRpolSim(giPh,bc,giExo,gij)
                       endif
                        consdistrp(bc,giExo,gij,giPh)=consRpolSim(giPh,bc,giExo,gij)
                        howndistrp(bc,giExo,gij,giPh)=0.0d0

                    else
                        hconsdistrp(bc,giExo,gij,giPh)=hconsBpolSim(giPh,bc,giExo,gij)
                        consdistrp(bc,giExo,gij,giPh)=consBpolSim(giPh,bc,giExo,gij)
                        howndistrp(bc,giExo,gij,giPh)=hBpolSim(giPh,bc,giExo,gij)

                    endif

!                    if(hconsdistrp(bc,giExo,gij,giPh) .LE. 0.0d0) then
!                        print*,bc,giExo,gij,giPh
!                        print*,hconsdistrp(bc,giExo,gij,giPh)
!                        pause
!
!                    endif
                     consdistrp(bc,giExo,gij,giPh)=max(consdistrp(bc,giExo,gij,giPh),cmin)

                enddo

                do hc=1,nh
                    do mc=1,nmsim
                        do lc = 1,nl
                            do bc = 1,nbsim
                                bj=GridBsim(bc)

                                iW=giW
                                gif=fWind(giW)
                                giz=zWind(giW)
                                giExo=(giAgg-1)*ngpW+giW
                                y=ypsgrid(gij,gif,giz,giAY)


                                bjp=GridBsim(bc)+GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
                                bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj)))

                                if(bjps .lt. GridB(1)) then
                                    !Negative Liquid Assets
                                    call basefun(GridBS,nbs,bjps,valsS,indsS)
                                    Wsellbuy=valsS(1)*WbuyS(indsS(1),giExo,giPh,gij)+valsS(2)*WbuyS(indsS(2),giExo,giPh,gij)
                                    Wsellrent=valsS(1)*WrentS(indsS(1),giExo,giPh,gij)+valsS(2)*WrentS(indsS(2),giExo,giPh,gij)

                                    consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consBpolS(indsS,giExo,giPh,gij))
                                    hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsBpolS(indsS,giExo,giPh,gij))
                                    mBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*mBpolS(indsS,giExo,giPh,gij))
                                    hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hBpolS(indsS,giExo,giPh,gij))
                                    bBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*bBpolS(indsS,giExo,giPh,gij))
                                    lBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*lBpolS(indsS,giExo,giPh,gij))

                                    hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsRpolS(indsS(:),giExo,giPh,gij))
                                    consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consRpolS(indsS(:),giExo,giPh,gij))
!                                    if(hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij) .le. 0.0d0 .or. hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij) .le. 0.0d0) then
!                                        print*, giPh,bc,lc,mc,hc,giExo,gij
!                                        print*, hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij),hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)
!                                        print*, bjp,bjps
!                                        print*, valsS,indsS
!                                        pause
!
!                                    endif



                                else
                                    !Positive Liquid Assets
                                    call basefun(GridB,nb,bjps,valsS,indsS)
                                    Wsellbuy=valsS(1)*Wbuy(indsS(1),giExo,giPh,gij)+valsS(2)*Wbuy(indsS(2),giExo,giPh,gij)
                                    Wsellrent=valsS(1)*Wrent(indsS(1),giExo,giPh,gij)+valsS(2)*Wrent(indsS(2),giExo,giPh,gij)

                                    consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consBpol(indsS,giExo,giPh,gij))
                                    hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsBpol(indsS,giExo,giPh,gij))
                                    mBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*mBpol(indsS,giExo,giPh,gij))
                                    hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hBpol(indsS,giExo,giPh,gij))
                                    bBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*bBpol(indsS,giExo,giPh,gij))
                                    lBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*lBpol(indsS,giExo,giPh,gij))

                                    hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsRpol(indsS(:),giExo,giPh,gij))
                                    consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consRpol(indsS(:),giExo,giPh,gij))
!                                    if(hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij) .le. 0.0d0 .or. hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij) .le. 0.0d0) then
!                                        print*, giPh,bc,lc,mc,hc,giExo,gij
!                                        print*, hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij),hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)
!                                        print*, bjp,bjps
!                                        print*, valsS,indsS
!                                        pause
!
!                                    endif
                                endif

                                helpWsell=max(Wsellbuy,Wsellrent)


                                WsellbuySim(giPh,bc,lc,mc,hc,giExo,gij)=Wsellbuy
                                WsellrentSim(giPh,bc,lc,mc,hc,giExo,gij)=Wsellrent


                                !See if better to foreclose or sell

                                call BiLinInterp1(nb,GridB,nm,GridM,Wfore(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),helpWfore)

                                WforeSim(giPh,bc,lc,mc,hc,giExo,gij)=helpWfore

                                call BiLinInterp1(nb,GridB,nm,GridM,consFpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),consFpolSim(giPh,bc,lc,mc,hc,giExo,gij))


                                if(helpWfore .gt. helpWsell) then

!                                    hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=(GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1))*thet(gij)
                                   if(GeRent .eq. 0) then
                                      hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                   else
                                      hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)*uChi_h
                                   endif

                                    howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0


                                    consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)
!(GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1))*thet(gij)

                                else

                                    if(Wsellbuy .gt. Wsellrent) then
                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)
                                    else

                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*thet(gij)
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                    endif

                                endif


                                call BiLinInterp1(nb,GridB,nm,GridM,Wrefin(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),helpWrefin)
                                call BiLinInterp1(nb,GridB,nm,GridM,Wpay(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),helpWpay)

                                WrefinSim(giPh,bc,lc,mc,hc,giExo,gij)=helpWrefin
                                WpaySim(giPh,bc,lc,mc,hc,giExo,gij)=helpWpay




                                call BiLinInterp1(nb,GridB,nm,GridM,consNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),consNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,mNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),mNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,bNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),bNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,lNpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),lNpolSim(giPh,bc,lc,mc,hc,giExo,gij))

!                                pmsim(mc,hc,giExo,gij)=FnPm(GridMsim(mc)*GridH(hc),gij)
!                                mPpolSim(giPh,bc,lc,mc,hc,giExo,gij)=((GridMsim(mc)*GridH(hc))*(1.0d0+rm)-pmsim(mc,hc,giExo,gij))/GridH(hc)
                                call BiLinInterp1(nb,GridB,nm,GridM,mPpol(:,lc,:,hc,giExo,giPh,gij)/GridH(hc),GridBsim(bc),GridMsim(mc),mPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,consPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),consPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,bPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),bPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,lPpol(:,lc,:,hc,giExo,giPh,gij),GridBsim(bc),GridMsim(mc),lPpolSim(giPh,bc,lc,mc,hc,giExo,gij))


                                helpVals(1) =   helpWfore
                                helpVals(2) =   helpWsell
                                helpVals(3) =   helpWrefin
                                helpVals(4) =   helpWpay

                                polind=maxloc(helpVals(1:4),dim=1)

                                select case(polind)

                                    case(1) !Foreclosure optimal
                                       if(GeRent .eq. 0) then
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                       else
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+(1.0d0-thet(gij))*consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)*uChi_h
                                       endif
                                      howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0

!                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+(GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1))*(1.0-thet(gij))
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consdistop(bc,lc,mc,hc,giExo,gij,giPh)+(1.0d0-thet(gij))*consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)


                                    case(2) !Selling optimal


                                        if(Wsellbuy .gt. Wsellrent) then

                                            hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                            consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consdistop(bc,lc,mc,hc,giExo,gij,giPh)+consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                            howndistop(bc,lc,mc,hc,giExo,gij,giPh)=howndistop(bc,lc,mc,hc,giExo,gij,giPh)+hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                        else
                                            consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consdistop(bc,lc,mc,hc,giExo,gij,giPh)+consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))

                                            if(GeRent .eq. 0) then
                                               hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                            else
                                               hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                            endif
                                            howndistop(bc,lc,mc,hc,giExo,gij,giPh)=howndistop(bc,lc,mc,hc,giExo,gij,giPh)


!                                            hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))

                                        endif

                                    case(3) !Refinance optimal

                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+uChi_h*consNpolSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consdistop(bc,lc,mc,hc,giExo,gij,giPh)+consNpolSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=howndistop(bc,lc,mc,hc,giExo,gij,giPh)+GridH(hc)*(1.0d0-thet(gij))
                                    case(4) !Payment optimal

                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)+uChi_h*consPpolSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consdistop(bc,lc,mc,hc,giExo,gij,giPh)+consPpolSim(giPh,bc,lc,mc,hc,giExo,gij)*(1.0d0-thet(gij))
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=howndistop(bc,lc,mc,hc,giExo,gij,giPh)+GridH(hc)*(1.0d0-thet(gij))
                                end select

!                                if(hconsdistop(bc,lc,mc,hc,giExo,gij,giPh) .LE. 0.0d0) then
!                                    print*,bc,lc,mc,hc,giExo,gij,giPh,polind
!                                    print*,hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)
!                                    pause
!
!                                endif



                            enddo
                        enddo
                    enddo
                enddo
            enddo


        !Retired people
        case(Jwork+1:Jtot-1)
            do giW=1,ngpW
                do bc=1,nbsim
                    bj=GridBsim(bc)

                    iW=giW
                    giWp=giW
                    giExo=(giAgg-1)*ngpW+giW

                    !Check to see if stay renter or buy house
                    WrentSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*WrentRet(bcinds(:,bc),giExo,giPh,ijret))
                    WbuySim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*WbuyRet(bcinds(:,bc),giExo,giPh,ijret))


                    hconsRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hconsRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    hconsBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hconsBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    consRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*consRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    consBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*consBpolRet(bcinds(:,bc),giExo,giPh,ijret))

                    bRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*bRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    bBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*bBpolRet(bcinds(:,bc),giExo,giPh,ijret))

                    hBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    mBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*mBpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    lBpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*lBpolRet(bcinds(:,bc),giExo,giPh,ijret))



                    !Check to see if stay renter or buy house
                    if(WrentSim(giPh,bc,giExo,gij) .gt. WbuySim(giPh,bc,giExo,gij)) then

                       if(GeRent .eq. 0) then
                          hconsdistrp(bc,giExo,gij,giPh)=0.0d0
                       else
                          hconsdistrp(bc,giExo,gij,giPh)=hconsRpolSim(giPh,bc,giExo,gij)

                       endif
                       howndistrp(bc,giExo,gij,giPh)=0.0d0


!                       hconsdistrp(bc,giExo,gij,giPh)=hconsRpolSim(giPh,bc,giExo,gij)

                       consdistrp(bc,giExo,gij,giPh)=consRpolSim(giPh,bc,giExo,gij)

                    else

                        hconsdistrp(bc,giExo,gij,giPh)=hconsBpolSim(giPh,bc,giExo,gij)
                        consdistrp(bc,giExo,gij,giPh)=consBpolSim(giPh,bc,giExo,gij)
                        howndistrp(bc,giExo,gij,giPh)=hBpolSim(giPh,bc,giExo,gij)
                    endif

!                    if(hconsdistrp(bc,giExo,gij,giPh) .LE. 0.0d0) then
!                        print*,bc,giExo,gij,giPh
!                        print*,hconsdistrp(bc,giExo,gij,giPh)
!                        pause

!                    endif



                enddo
                do hc=1,nh
                    do mc=1,nmsim
                        do lc = 1,nl
                            do bc = 1,nbsim
                                bj=GridBsim(bc)

                                iW=giW
                                giWp=giW
                                giExo=(giAgg-1)*ngpW+giW
                                y=pgrid(gij,giW)



                                !Compute value of liquid assets after the sale
                                bjp=GridBsim(bc)+GridH(hc)*(Ph-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(Ph*GridH(hc))
                                bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj)))
                                if(bjps .lt. GridB(1)) then
                                    !Negative Liquid Assets
                                    call basefun(GridBS,nbs,bjps,valsS,indsS)
                                    Wsellbuy=valsS(1)*WbuyS(indsS(1),giExo,giPh,ijret)+valsS(2)*WbuyRetS(indsS(2),giExo,giPh,ijret)
                                    Wsellrent=valsS(1)*WrentS(indsS(1),giExo,giPh,ijret)+valsS(2)*WrentRetS(indsS(2),giExo,giPh,ijret)

                                    consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consBpolRetS(indsS,giExo,giPh,ijret))
                                    hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsBpolRetS(indsS,giExo,giPh,ijret))
                                    mBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*mBpolRetS(indsS,giExo,giPh,ijret))
                                    hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hBpolRetS(indsS,giExo,giPh,ijret))
                                    bBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*bBpolRetS(indsS,giExo,giPh,ijret))
                                    lBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*lBpolRetS(indsS,giExo,giPh,ijret))

                                    hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsRpolRetS(indsS(:),giExo,giPh,ijret))
                                    consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consRpolRetS(indsS(:),giExo,giPh,ijret))

                                else
                                    !Positive Liquid Assets
                                    call basefun(GridB,nb,bjps,valsS,indsS)
                                    Wsellbuy=valsS(1)*WbuyRet(indsS(1),giExo,giPh,ijret)+valsS(2)*WbuyRet(indsS(2),giExo,giPh,ijret)
                                    Wsellrent=valsS(1)*WrentRet(indsS(1),giExo,giPh,ijret)+valsS(2)*WrentRet(indsS(2),giExo,giPh,ijret)

                                    consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consBpolRet(indsS,giExo,giPh,ijret))
                                    hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsBpolRet(indsS,giExo,giPh,ijret))
                                    mBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*mBpolRet(indsS,giExo,giPh,ijret))
                                    hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hBpolRet(indsS,giExo,giPh,ijret))
                                    bBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*bBpolRet(indsS,giExo,giPh,ijret))
                                    lBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*lBpolRet(indsS,giExo,giPh,ijret))

                                    hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*hconsRpolRet(indsS(:),giExo,giPh,ijret))
                                    consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)=sum(valsS*consRpolRet(indsS(:),giExo,giPh,ijret))

                                endif

                                helpWsell=max(Wsellbuy,Wsellrent)

                                WsellbuySim(giPh,bc,lc,mc,hc,giExo,gij)=Wsellbuy
                                WsellrentSim(giPh,bc,lc,mc,hc,giExo,gij)=Wsellrent


                                call BiLinInterp1(nb,GridB,nm,GridM,WforeRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWfore)
                                call BiLinInterp1(nb,GridB,nm,GridM,WrefinRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWrefin)
                                call BiLinInterp1(nb,GridB,nm,GridM,WpayRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWpay)

                                WrefinSim(giPh,bc,lc,mc,hc,giExo,gij)=helpWrefin
                                WpaySim(giPh,bc,lc,mc,hc,giExo,gij)=helpWpay
                                WforeSim(giPh,bc,lc,mc,hc,giExo,gij)=helpWfore

                                call BiLinInterp1(nb,GridB,nm,GridM,consFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),consFpolSim(giPh,bc,lc,mc,hc,giExo,gij))


                                call BiLinInterp1(nb,GridB,nm,GridM,consNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),consNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,mNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),mNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,bNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,lNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),lNpolSim(giPh,bc,lc,mc,hc,giExo,gij))

!                                pmsim(mc,hc,giExo,gij)=FnPm(GridMsim(mc)*GridH(hc),gij)
!                                mPpolSim(giPh,bc,lc,mc,hc,giExo,gij)=((GridMsim(mc)*GridH(hc))*(1.0d0+rm)-pmsim(mc,hc,giExo,gij))/GridH(hc)
                                call BiLinInterp1(nb,GridB,nm,GridM,mPpolRet(:,lc,:,hc,giExo,giPh,ijret)/GridH(hc),GridBsim(bc),GridMsim(mc),mPpolSim(giPh,bc,lc,mc,hc,giExo,gij))

                                call BiLinInterp1(nb,GridB,nm,GridM,consPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),consPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,bPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,lPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),lPpolSim(giPh,bc,lc,mc,hc,giExo,gij))


                                helpVals(1) =   helpWfore
                                helpVals(2) =   helpWsell
                                helpVals(3) =   helpWrefin
                                helpVals(4) =   helpWpay

                                polind=maxloc(helpVals(1:4),dim=1)


                                select case(polind)

                                    case(1) !Foreclosure optimal

!                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)

                                       if(GeRent .eq. 0) then
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                       else
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)*uChi_h

                                       endif

                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)
!                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0

                                    case(2) !Selling optimal

                                        if(Wsellbuy .gt. Wsellrent) then

                                            hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)
                                            consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)
                                            howndistop(bc,lc,mc,hc,giExo,gij,giPh)=hBpolOSim(giPh,bc,lc,mc,hc,giExo,gij)

                                        else
!                                            hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)
                                            consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)
                                            howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0

                                            if(GeRent .eq. 0) then
                                               hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                            else
                                               hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsRpolOSim(giPh,bc,lc,mc,hc,giExo,gij)

                                            endif



                                        endif

                                    case(3) !Refinance optimal

                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=consNpolSim(giPh,bc,lc,mc,hc,giExo,gij)*uChi_h
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consNpolSim(giPh,bc,lc,mc,hc,giExo,gij)
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)
                                    case(4) !Payment optimal
                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=consPpolSim(giPh,bc,lc,mc,hc,giExo,gij)*uChi_h
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consPpolSim(giPh,bc,lc,mc,hc,giExo,gij)
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)
                                end select

!                                if(hconsdistop(bc,lc,mc,hc,giExo,gij,giPh) .LE. 0.0d0) then
!                                    print*,bc,lc,mc,hc,giExo,gij,giPh,polind
!                                    print*,hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)
!                                    pause
!
!                                endif


                            enddo
                        enddo
                    enddo
                enddo
            enddo

        !Dying people
        case(Jtot)

            do giW=1,ngpW
                do bc=1,nbsim
                    iW=giW
                    giWp=giW
                    giExo=(giAgg-1)*ngpW+giW
                    hconsRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*hconsRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    consRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*consRpolRet(bcinds(:,bc),giExo,giPh,ijret))
                    bRpolSim(giPh,bc,giExo,gij)=sum(bcvals(:,bc)*bRpolRet(bcinds(:,bc),giExo,giPh,ijret))

                    if(GeRent .eq. 0) then
                       hconsdistrp(bc,giExo,gij,giPh)=0.0d0
                    else
                       hconsdistrp(bc,giExo,gij,giPh)=hconsRpolSim(giPh,bc,giExo,gij)

                    endif
                    howndistrp(bc,giExo,gij,giPh)=0.0d0

                    consdistrp(bc,giExo,gij,giPh)=consRpolSim(giPh,bc,giExo,gij)
 !                   hconsdistrp(bc,giExo,gij,giPh)=hconsRpolSim(giPh,bc,giExo,gij)

!                    if(hconsdistrp(bc,giExo,gij,giPh) .LE. 0.0d0) then
!                        print*,bc,giExo,gij,giPh
!                        print*,hconsdistrp(bc,giExo,gij,giPh)
!                        pause
!
!                    endif



                enddo

                do hc=1,nh
                    do mc=1,nmsim
                        do lc = 1,nl
                            do bc = 1,nbsim
                                iW=giW
                                giWp=giW
                                giExo=(giAgg-1)*ngpW+giW
                                y=pgrid(gij,giW)

                                call BiLinInterp1(nb,GridB,nm,GridM,WforeRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWfore)
                                call BiLinInterp1(nb,GridB,nm,GridM,WsellRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWsell)
                                call BiLinInterp1(nb,GridB,nm,GridM,WpayRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),helpWpay)

                                WsellrentSim(giPh,bc,lc,mc,hc,giExo,gij)=helpWsell
                                WpaySim(giPh,bc,lc,mc,hc,giExo,gij)=helpWpay
                                WforeSim(giPh,bc,lc,mc,hc,giExo,gij)=helpWfore

                                call BiLinInterp1(nb,GridB,nm,GridM,consFpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),consFpolSim(giPh,bc,lc,mc,hc,giExo,gij))

                                call BiLinInterp1(nb,GridB,nm,GridM,hconsNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),hconsNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,consNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),consNpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,bNpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bNpolSim(giPh,bc,lc,mc,hc,giExo,gij))

                                call BiLinInterp1(nb,GridB,nm,GridM,consPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),consPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,bPpolRet(:,lc,:,hc,giExo,giPh,ijret),GridBsim(bc),GridMsim(mc),bPpolSim(giPh,bc,lc,mc,hc,giExo,gij))
                                call BiLinInterp1(nb,GridB,nm,GridM,mPpolRet(:,lc,:,hc,giExo,giPh,ijret)/GridH(hc),GridBsim(bc),GridMsim(mc),mPpolSim(giPh,bc,lc,mc,hc,giExo,gij))


                                helpVals(1) =   helpWfore
                                helpVals(2) =   helpWsell
                                helpVals(3) =   helpWpay

                                polind=maxloc(helpVals(1:3),dim=1)


                                select case(polind)

                                    case(1) !Foreclosure optimal

!                                       hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
!                                       hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)

                                       if(GeRent .eq. 0) then
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                       else
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=consFpolSim(giPh,bc,lc,mc,hc,giExo,gij)*uChi_h

                                       endif
                                       howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0

                                    case(2) !Selling optimal

                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consNpolSim(giPh,bc,lc,mc,hc,giExo,gij)
                                       if(GeRent .eq. 0) then
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0
                                       else
                                          hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsNpolSim(giPh,bc,lc,mc,hc,giExo,gij)

                                       endif
                                       howndistop(bc,lc,mc,hc,giExo,gij,giPh)=0.0d0


!                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=hconsNpolSim(giPh,bc,lc,mc,hc,giExo,gij)

                                    case(3) !Payment optimal
                                        hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)=consPpolSim(giPh,bc,lc,mc,hc,giExo,gij)*uChi_h
                                        consdistop(bc,lc,mc,hc,giExo,gij,giPh)=consPpolSim(giPh,bc,lc,mc,hc,giExo,gij)
                                        howndistop(bc,lc,mc,hc,giExo,gij,giPh)=GridH(hc)

                                end select

!                                if(hconsdistop(bc,lc,mc,hc,giExo,gij,giPh) .LE. 0.0d0) then
!                                    print*,bc,lc,mc,hc,giExo,gij,giPh,polind
!                                    print*,hconsdistop(bc,lc,mc,hc,giExo,gij,giPh)
!                                    pause
!
!                                endif

                                 consdistop(bc,lc,mc,hc,giExo,gij,giPh)=max( consdistop(bc,lc,mc,hc,giExo,gij,giPh),cmin)

                            enddo
                        enddo
                    enddo
                enddo
            enddo
    end select


enddo
enddo
enddo



Ph=0.50d0
Pr=0.05d0

!do giAgg=1,ngpAgg
!do giPh=1,ngpPh
!print*,(sum(hconsdistrp(:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*distr)+sum(hconsdistop(:,:,:,:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*disto))/(dble(Jtot))
!enddo
!enddo
!pause

do it=tstart,tend

    if(it>=iindex .and. it.lt.(Tsim-5*ngpAgg*ngpAgg))  CALL DiscreteDist1(iAggpath(it), ngpAgg, transMatAgg(iAggpath(it-1),:),phrand(it))

    if(it>=itstop) then
       CALL DiscreteDist1(iAggpath(it), ngpAgg, transMatAgg(iAggpath(it-1),:),phrand(it))
       if(iAggpath(it) .le. (ngpAgg/2)) then
          iAggpath(it)=ngpAgg/2
       else
          iAggpath(it)=ngpAgg
       endif
       if (it>=Tsim-6) then
          iAggpath(it)=ngpAgg
       endif
    endif

    ! if((DoIRF .eq. 1) .and. (it .eq. 98)) then
    !     distssr=distr
    !     distsso=disto
    !     Hsteady=Ht
    ! endif
    if((DoIRF .eq. 1) .and. (it .eq. 98) .and. (exogenous .eq. 0)) then
        distssr=distr
        distsso=disto
        Hsteady=Ht
    endif


   if( ((it .eq. 99) .OR. (it .eq. 159) .or. (it .eq. 199) .or. (it .eq. 239) .or. (it .eq. 289) .or. (it .eq. 329) .or. (it .eq. 389) .or. (it .eq. 439) .or. (it .eq. 359) .or. (it .eq. 419)) .AND. (DoIRF .eq. 1)) then

      distr=distssr
      disto=distsso
      Ht=Hsteady

   endif

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


    if(((it .eq. 166) .or. (it .eq. 206)) .and. (modify .eq. 1) .and. (DoIRF .eq. 1) ) then
!    if(((it .eq. 167) .or. (it .eq. 207)) .and. (modify .eq. 1) .and. (DoIRF .eq. 1) ) then


        !Mortgage modification program

!        Hsupply=Ht*(1.0d0-deltah)+((PhGrid)**(alpha_h/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
!        do giPh=1,ngpPh
!            Hdemand(giPh)=(sum(hconsdistrp(:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*distr)+sum(hconsdistop(:,:,:,:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*disto))/(dble(Jtot))
!            if(Display==1) print*,phgrid(giph),Hdemand(giph),hsupply(giph)
!
!        enddo
!        CALL rtbis(FnXSH,PhGrid(1),PhGrid(ngpPh),1.0d-6,1.0d-6,phh)
!        if(Hdemand(ngpPh) .gt. Hsupply(ngpPh)) then
!           phh=Phgrid(ngpPh)
!        elseif(Hdemand(1) .lt. Hsupply(1)) then
!           phh=Phgrid(1)
!        endif


        distop=0.0d0


        !Renters unaffected
        !For every household with leverage > 1, reset leverage<=1
        mc=1
        if(it .lt. 200) then
            modifyval=0.96d0
        else
            modifyval=0.91d0
        endif
        do while(GridMsim(mc) .lt. modifyval*Phpath(106))
            mc=mc+1
        enddo
        poslev=mc-1

        if( 1 .eq. 0) then
            distop(:,:,1:poslev,:,:,:)=disto(:,:,1:poslev,:,:,:)
            do mc=poslev+1,nmsim
                distop(:,:,poslev,:,:,:)=distop(:,:,poslev,:,:,:)+disto(:,:,mc,:,:,:)
            enddo
            print*,'Fraction modified',sum(disto(:,:,poslev+1:nmsim,:,:,:))
            if(abs(sum(distop-disto)) .gt. 1.d-10) then
                print*,'Something Fucked'
                exit
            else
                disto=distop
                distop=0.0d0
            endif
        else
            distop(bzerosim:nbsim,:,1:poslev,:,:,:)=disto(bzerosim:nbsim,:,1:poslev,:,:,:)
            do hc=1,nh
                do mc=1,poslev
                    do bc=1,bzerosim-1
                        if((-GridBsim(bc)/GridH(hc)+GridMsim(mc)) .gt. modifyval*Phpath(106)) then
                            distop(bzerosim,:,mc,hc,:,:)=distop(bzerosim,:,mc,hc,:,:)+disto(bc,:,mc,hc,:,:)
                        else
                            distop(bc,:,mc,hc,:,:)=distop(bc,:,mc,hc,:,:)+disto(bc,:,mc,hc,:,:)
                        endif
                    enddo
                enddo
            enddo
            do mc=poslev+1,nmsim
                do bc=1,bzerosim-1
                    distop(bzerosim,:,poslev,:,:,:)=distop(bzerosim,:,poslev,:,:,:)+disto(bc,:,mc,:,:,:)
                enddo
                do bc=bzerosim,nbsim
                    distop(bc,:,poslev,:,:,:)=distop(bc,:,poslev,:,:,:)+disto(bc,:,mc,:,:,:)
                enddo
            enddo


            print*,'Fraction modified',sum(disto(:,:,poslev+1:nmsim,:,:,:))
            if(abs(sum(distop-disto)) .gt. 1.d-10) then
                print*,'Something Fucked'
                exit
            else
                disto=distop
                distop=0.0d0
            endif
         endif

    endif
    if( (Modify .eq. 1) .and. (it .eq. 1000) ) exit

!    print*,giAgg
    if(it .gt. -2*Jtot) then

       if(AdjustCostModel .eq. 0) then
!          Hsupply=Ht*(1.0d0-deltah)+((PhGrid)**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))


! Old April 2016
!          Hsupply=Ht*(1.0d0-deltah)+((PhGrid)**(alpha_h/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
!New April 2016
          Hsupply=Ht*(1.0d0-deltah)+((PhGrid)**(alpha_h/(1.0d0-alpha_h)))*((ZhGrid(giAgg))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))

       else
          Hsupply=Ht*(1.0d0-deltah)+(1.0d0/kappa)*(PhGrid**alpha_h)*(Ht**adjust)

       endif


!        Hsupply=Ht*(1.0d0-deltah)+((PhGrid)**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))*Ht**0.5d0








    !    Hsupply=Ht*(1.0d0-deltah)+((PhGrid)**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h/GridAY(giAy))**(alpha_h/(1.0d0-alpha_h)))
    !    print*,sum(distr),sum(disto),sum(distr)+sum(disto)
        if(abs(sum(distr)+sum(disto)-dble(Jtot)) .gt. 0.001d0) print*,'Dist fucked'
        do giPh=1,ngpPh
            Hdemand(giPh)=(sum(hconsdistrp(:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*distr)+sum(hconsdistop(:,:,:,:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*disto))/(dble(Jtot))
            if(Display==1) print*,phgrid(giph),Hdemand(giph),hsupply(giph)
            Hownsupply(giPh)=(sum(howndistrp(:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*distr)+sum(howndistop(:,:,:,:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,giPh)*disto))/(dble(Jtot))
        enddo

        XSHdem=Hdemand-Hsupply

        do giPhh = 1, ngpPhh
          do giPhr = 1, ngpPhr
            giPh = (giPhh-1)*ngpPhr+giPhr
            xsdemand(giPhh,giPhr)=(Hdemand(giPh)-Hownsupply(giPh))**2 + (Hsupply(giPh)-Hownsupply(giPh))**2
            xsdemand1(giPhh,giPhr)=Hdemand(giPh)-Hownsupply(giPh)
            xsdemand2(giPhh,giPhr)=Hsupply(giPh)-Hownsupply(giPh)
!            print*,GridPh(giPhh),GridPrPh(giPhr),xsdemand(giPhh,giPhr)
!            print*,Hdemand(giPh),Hownsupply(giPh),Hsupply(giPh)
          ENDDO
        ENDDO
        PhPr(1)=Ph
        PhPr(2)=Pr/Ph

        if(Display==1) print*,Hdemand
        if(Display==1) print*,Hsupply
        if(Display==1) print*,XSHdem
        !  call spline(Phgrid,XSHdem,scoef1,scoef2,scoef3,ngpPh)
        ! CALL rtbis(FnXSH,PhGrid(1),PhGrid(ngpPh),1.0d-6,1.0d-6,phh)
        nPhPr=2

        CALL bobyqa_h(nest,ninterppt,PhPr,param_bound(:,1),param_bound(:,2), &
             rhobeg,rhoend,iprint,maxeval,wspace,nmoments+1, &
             NMAX,MMAX,nptmax,DFOVEC)

        ! call simplex(PhPr,nPhPr,ftol,scale,iprint,FnXSdemand)
!        print*,PhPr(1),PhPr(2),FnXSdemand(PhPr)

        ! if(Hdemand(ngpPh) .gt. Hsupply(ngpPh)) then
        !    phh=Phgrid(ngpPh)
        ! elseif(Hdemand(1) .lt. Hsupply(1)) then
        !    phh=Phgrid(1)
        ! endif
        Demelas(it)=(log(Hdemand(ngpPh))-log(Hdemand(1)))/(log(PhGrid(ngpPh))-log(PhGrid(1)))
!        Demelas(it)=(log(Hdemand(ngpPh/2+1))-log(Hdemand(ngpPh/2-1)))/(log(PhGrid(ngpPh/2+1))-log(PhGrid(ngpPh/2-1)))
        phh = PhPr(1)
        CALL basefun(GridPh,ngpPhh,exp(log(phh)+log(gelas)),Phvals2,Phinds2)

        CALL basefun(GridPrPh,ngpPhr,PhPr(2),Phrvals,Phrinds)
        CALL basefun(GridPh,ngpPhh,PhPr(1),Phhvals,Phhinds)

        Phinds(1)=Phrinds(1)+(Phhinds(1)-1)*ngpPhr
        Phinds(2)=Phrinds(1)+(Phhinds(2)-1)*ngpPhr
        Phinds(3)=Phrinds(2)+(Phhinds(1)-1)*ngpPhr
        Phinds(4)=Phrinds(2)+(Phhinds(2)-1)*ngpPhr
        Phvals(1)=Phrvals(1)*Phhvals(1)
        Phvals(2)=Phrvals(1)*Phhvals(2)
        Phvals(3)=Phrvals(2)*Phhvals(1)
        Phvals(4)=Phrvals(2)*Phhvals(2)


        Ph=phh
        gPh=phh
        Phpath(it)=gPh

       if(AdjustCostModel .eq. 0) then

          Ht=Ht*(1.0d0-deltah)+((phh)**(alpha_h/(1.0d0-alpha_h)))*((ZhGrid(giAgg))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
!          Ht = sum(Phvals*Hownsupply(Phinds))
       else
          Ht=Ht*(1.0d0-deltah)+(1.0d0/kappa)*(phh**alpha_h)*(Ht**adjust)

       endif

       Pr = PhPr(2)*Ph
       gPr=Pr
       Prpath(it)=Pr


    else

        !   phh=1.08d0
        !CALL basefun(PhGrid,ngpPh,phh,Phvals,Phinds)
        phh=phgrid(6)
        !Ht=sum(Phvals*Hdemand(phinds))
        Ht=Ht*(1.0d0-deltah)+((phh)**(1.0d0/(1.0d0-alpha_h)))*((ZhGrid(giAgg))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h)))
        Ph=phh
        gPh=phh
        Phpath(it)=gPh
        Phvals(1)=1.0d0
        Phvals(2)=0.0d0
        Phinds(1)=6
        Phinds(2)=7
    endif

    if(Display==0) print*,it,gPh,giAgg,gPr !,sum(disto)+sum(distr)
    if(Display==0) print*,Ht,sum(Phvals*(Hdemand(Phinds)-Hownsupply(Phinds))),sum(Phvals*(Hsupply(Phinds)-Hownsupply(Phinds)))
    giRf = AtoR(giAgg)
    !Changed 11/20/15 to make grid on Rf on Agg for comovement
    rf=RfGrid(giAgg)
!    rf=GridRf(giRf)
    grf=rf
    rm=Rmgrid(giAgg)
    grm=rm
    rl=RlGrid(giAgg)
    grl=rl
    if(exurent .eq. 0) then
       phptemp=exp(aa0(giAgg,:)+aa1(giAgg,:)*log(phh))
    else
       phptemp=exp(aa0p(giAgg,:)+aa1(giAgg,:)*log(phh))
    endif

    Htpath(it)=Ht
    nunu = NuGrid(giAgg)
    uChi_c = 1.0d0-1.0d0/(1.0d0+(Pr**(alph/(1.0d0-alph)))*((nunu/(1.0d0-nunu))**(1.0d0/(1.0d0-alph))))
    uChi_h = (1.0d0/(1.0d0+(Pr**(alph/(1.0d0-alph)))*((nunu/(1.0d0-nunu))**(1.0d0/(1.0d0-alph)))))/Pr / uChi_c

!    do ij=1,Jtot
!       print*,ij,sum(disto(:,:,:,:,:,ij))+sum(distr(:,:,ij)),sum(distr(:,:,ij)),sum(disto(:,:,:,:,:,ij))

!    enddo
!    pause
    distop=0.0d0
    distrp=0.0d0

    hconsdistr=0.0d0
    hconsdisto=0.0d0
    selldisto=0.0d0
    consdisto=0.0d0
    foredisto=0.0d0
    buydisto=0.0d0
    refidisto=0.0d0

    beqdisto=0.0d0
    beqdistr=0.0d0
    taxsimo=0.0d0
    taxsimr=0.0d0

!    if((it .eq. 245) .and. (DoIRF .eq. 1)) then
    if((it .eq. 165)) then
    !Do Goldmine
!       WHERE(ISNAN(consdistrp)) consdistrp=cmin
!       WHERE(ISNAN(consdistop)) consdistop=cmin
!       WHERE(consdistrp .LT. cmin) consdistrp=cmin
!       WHERE(consdistop .LT. cmin) consdistop=cmin


!        Goldmine(1)=Phvals(1)*(sum(consdistrp(:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,Phinds(1))*distr)+sum(hconsdistop(:,:,:,:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,Phinds(1))*disto))/(dble(Jtot))+Phvals(2)*(sum(consdistrp(:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,Phinds(2))*distr)+sum(hconsdistop(:,:,:,:,(giAgg-1)*ngpW+1:giAgg*ngpW,:,Phinds(2))*disto))/(dble(Jtot))
if(exogenous .eq. 0) then
OPEN(3, FILE = trim(OutputDir) // 'distsso.txt', STATUS = 'replace');
DO ij=1,Jtot
DO giW=1,ngpW
DO hc=1,nh
DO mc=1,nmsim
DO lc=1,nl
DO bc=1,nbsim
   do gij=1,7
      CALL basefun(GridPh,ngpPhh,exp(log(phh)+Goldminevals(gij)),Phvals3,Phinds3)
      Goldmine(gij)=sum(Phvals3*consdistop(bc,lc,mc,hc,(giAgg-1)*ngpW+giW,ij,Phinds3))
   enddo
   WRITE(3,'(15e25.15)') GridBsim(bc),GridL(lc),GridMsim(mc),GridH(hc),dble(giW),dble(ij),Ph,Goldmine(:),max(1.0d-16,distsso(bc,lc,mc,hc,giW,ij))
ENDDO
ENDDO
ENDDO
ENDDO
ENDDO
ENDDO
CLOSE(3)
OPEN(3, FILE = trim(OutputDir) // 'distssr.txt', STATUS = 'replace');
DO ij=1,Jtot
DO giW=1,ngpW
DO bc=1,nbsim
   do gij=1,7
      CALL basefun(GridPh,ngpPhh,exp(log(phh)+Goldminevals(gij)),Phvals3,Phinds3)
      Goldmine(gij)=sum(Phvals3*consdistrp(bc,(giAgg-1)*ngpW+giW,ij,Phinds3))
   enddo
   WRITE(3,'(11e25.15)') GridBsim(bc),dble(giW),dble(ij),Goldmine(:),max(1.0d-16,distssr(bc,giW,ij))
ENDDO
ENDDO
ENDDO
CLOSE(3)
endif



    endif





    !Now update thedistribution based on the market clearing price

    do gij=1,Jtot

        ij=gij
        ijret = ij-Jwork
        rf=grf
        rm=grm
        rl=grl
        select case(gij)

            !Working people
            case(1:Jwork)
                do giW=1,ngpW
                    gif=fWind(1+mod(giW-1,ngpW/ngpPv))
                    giz=zWind(1+mod(giW-1,ngpW/ngpPv))
                    do bc=1,nbsim
                       if(distr(bc,giW,gij) .eq. 0.0d0) cycle
                        iW=giW
                        giExo=(giAgg-1)*ngpW+giW
                        taxsimr(bc,giW,gij)=FnTax(ypsgrid(gij,gif,giz,giAY))/ypsgrid(gij,gif,giz,giAY)

                        !Check to see if stay renter or buy house

                        if(sum(Phvals*WbuySim(Phinds,bc,giExo,gij)) .lt. sum(Phvals*WrentSim(Phinds,bc,giExo,gij)) ) then

                            hj=sum(Phvals*hconsRpolSim(Phinds,bc,giExo,gij))
                            bj=sum(Phvals*bRpolSim(Phinds,bc,giExo,gij))
                            cj=sum(Phvals*consRpolSim(Phinds,bc,giExo,gij))
                            cj2=sum(Phvals2*consRpolSim(Phinds2,bc,giExo,gij))


                            hconsdistr(bc,giW,gij)=hj
                            consdistr(bc,giW,gij)=cj
                            dpconsdistr(bc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                            dhconsdistr(bc,giW,gij)=0.0d0
                            bdistr(bc,giW,gij)=bj
                            call basefun(GridBsim,nbsim,bj,vals,inds)

                            do giWp=1,ngpW
                                distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*distr(bc,giW,gij)
                                distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*distr(bc,giW,gij)
                            enddo

                        else

                            hj=sum(Phvals*hconsBpolSim(Phinds,bc,giExo,gij))
                            cj=sum(Phvals*consBpolSim(Phinds,bc,giExo,gij))
                            cj2=sum(Phvals2*consBpolSim(Phinds2,bc,giExo,gij))
                            bj=sum(Phvals*bBpolSim(Phinds,bc,giExo,gij))

                            hconsdistr(bc,giW,gij)=hj

                            consdistr(bc,giW,gij)=cj
                            bdistr(bc,giW,gij)=bj
                            buydistr(bc,giW,gij)=1.0d0
                            dpconsdistr(bc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                            dhconsdistr(bc,giW,gij)=0.0d0

                            call basefun(GridBsim,nbsim,bj,vals,inds)
                            temp=0.0d0
                            do giWp=1,ngpW
                                do bcc=1,2
                                    do mcc=1,4
                                    do jcc=1,2
                                        hcc=hIBpol(bcinds(jcc,bc),giExo,Phinds(mcc),gij)
                                        lcc=lIBpol(bcinds(jcc,bc),giExo,Phinds(mcc),gij)
                                        kcc=MtoMsim(mIBpol(bcinds(jcc,bc),giExo,Phinds(mcc),gij))
                                        distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+bcvals(jcc,bc)*vals(bcc)*Phvals(mcc)*zytrans(gij,giW,giWp)*distr(bc,giW,gij)
                                        !temp=temp+vals(bcc)*Phvals(mcc)*zytrans(gij,giW,giWp)
                                    enddo
                                    enddo
                                enddo
                            enddo
                            !if(abs(temp-1.0d0) .gt. 0.00000001d0) print*,'1 ',gij,temp
                        endif
                    enddo

                    do hc=1,nh
                        do mc=1,nmsim
                            do lc = 1,nl
                                do bc = 1,nbsim
                                   if(disto(bc,lc,mc,hc,giW,gij) .eq. 0.0d0) cycle
                                    iW=giW
                                    gif=fWind(giW)
                                    giz=zWind(giW)
                                    y=ypsgrid(gij,gif,giz,giAY)


                                    Wsellbuy=sum(Phvals*WsellbuySim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    Wsellrent=sum(Phvals*WsellrentSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                    helpWsell=max(Wsellrent,Wsellbuy)

                                    helpWfore=sum(Phvals*WforeSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                    !See if better to foreclose or sell
                                    if(helpWfore  .gt. helpWsell) then


                                        taxsimo(bc,lc,mc,hc,giW,gij)=thet(gij)*FnTax(y)/y
                                        cj=sum(Phvals*consFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                        cj2=sum(Phvals2*consFpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                        bj=sum(Phvals*bFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
!                                        hj=GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
                                        hj=cj*uChi_h

                                        consdisto(bc,lc,mc,hc,giW,gij)=cj*thet(gij)
                                        hconsdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                                        foredisto(bc,lc,mc,hc,giW,gij)=thet(gij)
                                        dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*thet(gij)
!                                        dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*thet(gij)


                                        bdisto(bc,lc,mc,hc,giW,gij)=bj*thet(gij)

                                        call basefun(GridBsim,nbsim,bj,vals,inds)
                                        temp=0.0d0
                                        do giWp=1,ngpW
                                            distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*thet(gij)
                                            distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*thet(gij)
                                            !temp=temp+(vals(1)+vals(2))*zytrans(gij,giW,giWp)
                                        enddo
                                        !if(abs(temp-1.0d0) .gt. 0.00000001d0) print*,'2 ', gij,temp
                                    else

                                        selldisto(bc,lc,mc,hc,giW,gij)=selldisto(bc,lc,mc,hc,giW,gij)+(thet(gij))
                                        taxsimo(bc,lc,mc,hc,giW,gij)=thet(gij)*FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y


                                        if(Wsellbuy .gt. Wsellrent) then
                                            !Compute value of liquid assets after the sale
                                            bjp=GridBsim(bc)+GridH(hc)*(gPh-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(gPh*GridH(hc))
                                            bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj)))


                                            hj=sum(Phvals*hconsBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj=sum(Phvals*consBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consBpolOSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                            consdisto(bc,lc,mc,hc,giW,gij)=cj*thet(gij)
                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*thet(gij)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*thet(gij)

                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)

                                            buydisto(bc,lc,mc,hc,giW,gij)=thet(gij)

                                            call basefun(GridBsim,nbsim,bj,vals,inds)

                                            bdisto(bc,lc,mc,hc,giW,gij)=bj*thet(gij)

                                            if(bjps .lt. 0.0d0) then
                                                call basefun(GridBS,nbs,bjps,valsS,indsS)

                                                do giWp=1,ngpW
                                                    do jcc=1,4
                                                    do mcc=1,2
                                                        do bcc=1,2
                                                            hcc=hIBpolS(indsS(mcc),giExo,Phinds(jcc),gij)
                                                            kcc=MtoMsim(mIBpolS(indsS(mcc),giExo,Phinds(jcc),gij))
                                                            lcc=lIBpolS(indsS(mcc),giExo,Phinds(jcc),gij)
                                                            distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*Phvals(jcc)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*thet(gij)
                                                        enddo

                                                    enddo
                                                    enddo
                                                enddo
                                            else
                                                call basefun(GridB,nb,bjps,valsS,indsS)
                                                do giWp=1,ngpW
                                                    do jcc=1,4
                                                    do mcc=1,2
                                                        do bcc=1,2
                                                            hcc=hIBpol(indsS(mcc),giExo,Phinds(jcc),gij)
                                                            kcc=MtoMsim(mIBpol(indsS(mcc),giExo,Phinds(jcc),gij))
                                                            lcc=lIBpol(indsS(mcc),giExo,Phinds(jcc),gij)
                                                            distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*Phvals(jcc)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*thet(gij)
                                                        enddo

                                                    enddo
                                                    enddo
                                                enddo




                                            endif


                                        else

                                            hj=sum(Phvals*hconsRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj=sum(Phvals*consRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consRpolOSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                            consdisto(bc,lc,mc,hc,giW,gij)=cj*thet(gij)

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*thet(gij)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*thet(gij)

                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj*thet(gij)
                                            bdisto(bc,lc,mc,hc,giW,gij)=bj*thet(gij)

                                            call basefun(GridBsim,nbsim,bj,vals,inds)

                                            do giWp=1,ngpW
                                                distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*thet(gij)
                                                distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*thet(gij)
                                            enddo

                                        endif


                                    endif

                                    helpWrefin=sum(Phvals*WrefinSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    helpWpay=sum(Phvals*WpaySim(Phinds,bc,lc,mc,hc,giExo,gij))


                                    helpVals(1) =   helpWfore
                                    helpVals(2) =   helpWsell
                                    helpVals(3) =   helpWrefin
                                    helpVals(4) =   helpWpay


                                    polind=maxloc(helpVals(1:4),dim=1)


                                    select case(polind)

                                        case(1) !Foreclosure optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=taxsimo(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))*FnTax(y)

                                            cj=sum(Phvals*consFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consFpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
!                                            hj=GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
                                            hj=cj*uChi_h


                                            consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0-thet(gij))
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0-thet(gij))
                                            foredisto(bc,lc,mc,hc,giW,gij)=foredisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))
                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*(1.0d0-thet(gij))+dpconsdisto(bc,lc,mc,hc,giW,gij)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*(1.0d0-thet(gij))+dhconsdisto(bc,lc,mc,hc,giW,gij)


                                            call basefun(GridBsim,nbsim,bj,vals,inds)

                                            do giWp=1,ngpW
                                                distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                            enddo





                                        case(2) !Selling optimal


                                            taxsimo(bc,lc,mc,hc,giW,gij)=taxsimo(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))*FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y

                                            selldisto(bc,lc,mc,hc,giW,gij)=selldisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))

                                            if(Wsellbuy .gt. Wsellrent) then


                                                hj=sum(Phvals*hconsBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj=sum(Phvals*consBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj2=sum(Phvals2*consBpolOSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                                bj=sum(Phvals*bBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))



                                                dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*(1.0d0-thet(gij))+dpconsdisto(bc,lc,mc,hc,giW,gij)
!                                                dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*(1.0d0-thet(gij))+dhconsdisto(bc,lc,mc,hc,giW,gij)
                                                consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0d0-thet(gij))
                                                hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                                buydisto(bc,lc,mc,hc,giW,gij)=buydisto(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))
                                                bdisto(bc,lc,mc,hc,giW,gij)=bdisto(bc,lc,mc,hc,giW,gij)+bj*(1.0d0-thet(gij))

                                                call basefun(GridBsim,nbsim,bj,vals,inds)


                                                if(bjps .lt. 0.0d0) then

                                                    do giWp=1,ngpW
                                                        do jcc=1,4
                                                        do mcc=1,2
                                                            do bcc=1,2
                                                                hcc=hIBpolS(indsS(mcc),giExo,Phinds(jcc),gij)
                                                                kcc=MtoMsim(mIBpolS(indsS(mcc),giExo,Phinds(jcc),gij))
                                                                lcc=lIBpolS(indsS(mcc),giExo,Phinds(jcc),gij)
                                                                distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*Phvals(jcc)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                            enddo

                                                        enddo
                                                        enddo
                                                    enddo


                                                else

                                                    do giWp=1,ngpW
                                                        do jcc=1,4
                                                        do mcc=1,2
                                                            do bcc=1,2
                                                                hcc=hIBpol(indsS(mcc),giExo,Phinds(jcc),gij)
                                                                kcc=MtoMsim(mIBpol(indsS(mcc),giExo,Phinds(jcc),gij))
                                                                lcc=lIBpol(indsS(mcc),giExo,Phinds(jcc),gij)
                                                                distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*Phvals(jcc)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                            enddo

                                                        enddo
                                                        enddo
                                                    enddo




                                                endif


                                            else

                                                hj=sum(Phvals*hconsRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj=sum(Phvals*consRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj2=sum(Phvals2*consRpolOSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                                bj=sum(Phvals*bRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                                consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0d0-thet(gij))

                                                dpconsdisto(bc,lc,mc,hc,giW,gij)=dpconsdisto(bc,lc,mc,hc,giW,gij)+(log(cj2)-log(cj))/log(gelas)*(1.0d0-thet(gij))
!                                                dhconsdisto(bc,lc,mc,hc,giW,gij)=dhconsdisto(bc,lc,mc,hc,giW,gij)+(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*(1.0d0-thet(gij))

                                                hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+hj*(1.0d0-thet(gij))
                                                bdisto(bc,lc,mc,hc,giW,gij)=bdisto(bc,lc,mc,hc,giW,gij)+bj*(1.0d0-thet(gij))

                                                call basefun(GridBsim,nbsim,bj,vals,inds)

                                                do giWp=1,ngpW
                                                    distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                    distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                enddo

                                            endif









                                        case(3) !Refinance optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=taxsimo(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))*FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y

                                            hj=GridH(hc)
                                            cj=sum(Phvals*consNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consNpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            mj=sum(Phvals*mNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            lj=sum(Phvals*lNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*(1.0d0-thet(gij))+dpconsdisto(bc,lc,mc,hc,giW,gij)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*(1.0d0-thet(gij))+dhconsdisto(bc,lc,mc,hc,giW,gij)
                                            consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0d0-thet(gij))
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+uChi_h*cj*(1.0d0-thet(gij))


                                            refidisto(bc,lc,mc,hc,giW,gij)=(1.0d0-thet(gij))


                                            hcc=hc
                                            !mj=min(mj,gridC(giC)*gPh)

                                            call basefun(GridBsim,nbsim,bj,vals,inds)
                                            call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                            call basefun(GridL,nl,lj,valsl,indsl)

                                            do giWp=1,ngpW
                                                do bcc=1,2
                                                do mcc=1,2
                                                do lcc=1,2
                                                    kcc=indsm(mcc)
                                                    distop(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)=distop(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                enddo
                                                enddo
                                                enddo
                                            enddo




                                        case(4) !Payment optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=taxsimo(bc,lc,mc,hc,giW,gij)+(1.0d0-thet(gij))*FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y


                                            hj=GridH(hc)
                                            cj=sum(Phvals*consPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consPpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            mj=sum(Phvals*mPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            lj=sum(Phvals*lPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                            hcc=hc

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)*(1.0d0-thet(gij))+dpconsdisto(bc,lc,mc,hc,giW,gij)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))*(1.0d0-thet(gij))+dhconsdisto(bc,lc,mc,hc,giW,gij)
                                            consdisto(bc,lc,mc,hc,giW,gij)=consdisto(bc,lc,mc,hc,giW,gij)+cj*(1.0d0-thet(gij))
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hconsdisto(bc,lc,mc,hc,giW,gij)+uChi_h*cj*(1.0d0-thet(gij))

                                            call basefun(GridBsim,nbsim,bj,vals,inds)
                                            call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                            call basefun(GridL,nl,lj,valsl,indsl)


                                            do giWp=1,ngpW
                                                do bcc=1,2
                                                    do mcc=1,2
                                                    do lcc=1,2
                                                        distop(inds(bcc),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)=distop(inds(bcc),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*zytrans(gij,giW,giWp)*disto(bc,lc,mc,hc,giW,gij)*(1.0d0-thet(gij))
                                                    enddo
                                                    enddo
                                                enddo
                                            enddo


                                    end select
                                enddo
                            enddo
                        enddo
                    enddo
                enddo
                agepathEinc(it,gij) = sum(zydist(gij,:)*ypsgrid(gij,gif,:,giAY))
!                agepathEincown(it,gij) = sum(disto(*ypsgrid(gij,gif,:,giAY))


            !Retired people
            case(Jwork+1:Jtot-1)

                do giW=1,ngpW
                    do bc=1,nbsim
                       if(distr(bc,giW,gij) .eq. 0.0d0) cycle
                        iW=giW
                        giWp=giW
                        giExo=(giAgg-1)*ngpW+giW

                        taxsimr(bc,giW,gij)=FnTax(pgrid(gij,giW))/pgrid(gij,giW)



                        if(sum(Phvals*WbuySim(Phinds,bc,giExo,gij)) .le. sum(Phvals*WrentSim(Phinds,bc,giExo,gij)) ) then

                            hj=sum(Phvals*hconsRpolSim(Phinds,bc,giExo,gij))
                            bj=sum(Phvals*bRpolSim(Phinds,bc,giExo,gij))
                            cj=sum(Phvals*consRpolSim(Phinds,bc,giExo,gij))
                            cj2=sum(Phvals2*consRpolSim(Phinds2,bc,giExo,gij))


                            hconsdistr(bc,giW,gij)=hj
                            consdistr(bc,giW,gij)=cj
                            dpconsdistr(bc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                            dhconsdistr(bc,giW,gij)=0.0d0
                            bdistr(bc,giW,gij)=bj
                            call basefun(GridBsim,nbsim,bj,vals,inds)

                            distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*distr(bc,giW,gij)
                            distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*distr(bc,giW,gij)

!                            !if(abs(temp-1.0d0) .gt. 0.00000001d0) print*,'3 ', gij,vals(1)+vals(2)
                        else

                            hj=sum(Phvals*hconsBpolSim(Phinds,bc,giExo,gij))
                            cj=sum(Phvals*consBpolSim(Phinds,bc,giExo,gij))
                            cj2=sum(Phvals2*consBpolSim(Phinds2,bc,giExo,gij))
                            bj=sum(Phvals*bBpolSim(Phinds,bc,giExo,gij))

                            hconsdistr(bc,giW,gij)=hj

                            consdistr(bc,giW,gij)=cj
                            bdistr(bc,giW,gij)=bj
                            buydistr(bc,giW,gij)=1.0d0
                            dpconsdistr(bc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                            dhconsdistr(bc,giW,gij)=0.0d0

                            call basefun(GridBsim,nbsim,bj,vals,inds)

                            temp=0.0d0
                            do bcc=1,2
                                do mcc=1,4
                                do jcc=1,2
                                    hcc=hIBpolRet(bcinds(jcc,bc),giExo,Phinds(mcc),ijret)
                                    lcc=lIBpolRet(bcinds(jcc,bc),giExo,Phinds(mcc),ijret)
                                    kcc=MtoMsim(mIBpolRet(bcinds(jcc,bc),giExo,Phinds(mcc),ijret))
                                    !temp=temp+vals(bcc)*Phinds(mcc)
                                    distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+bcvals(jcc,bc)*vals(bcc)*Phvals(mcc)*distr(bc,giW,gij)
                                enddo
                                enddo
                            enddo
                            !if(abs(temp-1.0d0) .gt. 0.00000001d0) print*,'4 ',gij,temp
                       endif
                    enddo
                    do hc=1,nh
                        do mc=1,nmsim
                            do lc = 1,nl
                                do bc = 1,nbsim
                                   if(disto(bc,lc,mc,hc,giW,gij) .eq. 0.0d0) cycle
                                    iW=giW
                                    giWp=giW
                                    giExo=(giAgg-1)*ngpW+giW
                                    y=pgrid(gij,giW)

                                    Wsellbuy=sum(Phvals*WsellbuySim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    Wsellrent=sum(Phvals*WsellrentSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                    helpWsell=max(Wsellrent,Wsellbuy)
                                    helpWfore=sum(Phvals*WforeSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    helpWrefin=sum(Phvals*WrefinSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    helpWpay=sum(Phvals*WpaySim(Phinds,bc,lc,mc,hc,giExo,gij))

                                    helpVals(1) =   helpWfore
                                    helpVals(2) =   helpWsell
                                    helpVals(3) =   helpWrefin
                                    helpVals(4) =   helpWpay


                                    polind=maxloc(helpVals(1:4),dim=1)


                                    select case(polind)

                                        case(1) !Foreclosure optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTax(y)/y

                                            cj=sum(Phvals*consFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consFpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
!                                            hj=GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
                                            hj=cj*uChi_h

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                            foredisto(bc,lc,mc,hc,giW,gij)=1.0d0

                                            call basefun(GridBsim,nbsim,bj,vals,inds)

                                            distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*disto(bc,lc,mc,hc,giW,gij)
                                            distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*disto(bc,lc,mc,hc,giW,gij)
                                            !if(abs(vals(1)+vals(2)-1.0d0) .gt. 0.000001d0) print*,'5 ',gij,vals(1)+vals(2)


                                        case(2) !Selling optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y


                                            selldisto(bc,lc,mc,hc,giW,gij)=1.0d0



                                            if(Wsellbuy .gt. Wsellrent) then

                                                !Compute value of liquid assets after the sale
                                                bjp=GridBsim(bc)+GridH(hc)*(gPh-GridMsim(mc)*(1.0d0+rm)-GridL(lc)*(1.0d0+rl))-FnAdj(gPh*GridH(hc))
                                                bjps=bjp+(FnTax(y)-FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj)))

                                                hj=sum(Phvals*hconsBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj=sum(Phvals*consBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj2=sum(Phvals2*consBpolOSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                                bj=sum(Phvals*bBpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                                dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                                dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                                consdisto(bc,lc,mc,hc,giW,gij)=cj
                                                hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                                buydisto(bc,lc,mc,hc,giW,gij)=1.0d0
                                                bdisto(bc,lc,mc,hc,giW,gij)=bj

                                                call basefun(GridBsim,nbsim,bj,vals,inds)


                                                if(bjps .lt. 0.0d0) then
                                                    call basefun(GridBS,nbs,bjps,valsS,indsS)
                                                    do jcc=1,4
                                                    do mcc=1,2
                                                        do bcc=1,2

                                                            hcc=hIBpolRetS(indsS(mcc),giExo,Phinds(jcc),ijret)
!                                                            temp=temp+vals(bcc)*valsS(mcc)*Phvals(jcc)
                                                            kcc=MtoMsim(mIBpolRetS(indsS(mcc),giExo,Phinds(jcc),ijret))
                                                            lcc=lIBpolRetS(indsS(mcc),giExo,Phinds(jcc),ijret)
                                                            distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*Phvals(jcc)*disto(bc,lc,mc,hc,giW,gij)
                                                        enddo
                                                    enddo
                                                    enddo



                                                else
                                                    call basefun(GridB,nb,bjps,valsS,indsS)
                                                    do jcc=1,4
                                                    do mcc=1,2
                                                        do bcc=1,2
                                                            hcc=hIBpolRet(indsS(mcc),giExo,Phinds(jcc),ijret)
                                                            kcc=MtoMsim(mIBpolRet(indsS(mcc),giExo,Phinds(jcc),ijret))
                                                            lcc=lIBpolRet(indsS(mcc),giExo,Phinds(jcc),ijret)
!                                                            temp=temp+vals(bcc)*valsS(mcc)*Phvals(jcc)
                                                            distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)=distop(inds(bcc),lcc,kcc,hcc,giWp,gij+1)+ vals(bcc)*valsS(mcc)*Phvals(jcc)*disto(bc,lc,mc,hc,giW,gij)
                                                        enddo
                                                    enddo
                                                    enddo

                                                endif


                                            else

                                                hj=sum(Phvals*hconsRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj=sum(Phvals*consRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                                cj2=sum(Phvals2*consRpolOSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                                bj=sum(Phvals*bRpolOSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                                dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                                dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                                consdisto(bc,lc,mc,hc,giW,gij)=cj
                                                hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                                bdisto(bc,lc,mc,hc,giW,gij)=bj

                                                buydisto(bc,lc,mc,hc,giW,gij)=0.0d0

                                                call basefun(GridBsim,nbsim,bj,vals,inds)
                                                distrp(inds(1),giWp,gij+1)=distrp(inds(1),giWp,gij+1)+vals(1)*disto(bc,lc,mc,hc,giW,gij)
                                                distrp(inds(2),giWp,gij+1)=distrp(inds(2),giWp,gij+1)+vals(2)*disto(bc,lc,mc,hc,giW,gij)


                                            endif

                                        case(3) !Refinance optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y


                                            hj=GridH(hc)
                                            cj=sum(Phvals*consNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consNpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            mj=sum(Phvals*mNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            lj=sum(Phvals*lNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=uChi_h*cj
                                            bdisto(bc,lc,mc,hc,giW,gij)=bj


                                            refidisto(bc,lc,mc,hc,giW,gij)=1.0d0


                                            hcc=hc
!                                            mj=min(mj,gridC(giC)*gPh)

                                            call basefun(GridBsim,nbsim,bj,vals,inds)
                                            call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                            call basefun(GridL,nl,lj,valsl,indsl)

                                            temp=0.0d0
                                            do bcc=1,2
                                            do mcc=1,2
                                            do lcc=1,2
!                                                kcc=MtoMsim(mINpolRet(bcinds(jcc,bc),lc,mc,hc,giExo,Phinds(mcc),ijret))
                                                kcc=indsm(mcc)
                                                distop(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)=distop(inds(bcc),indsl(lcc),kcc,hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*disto(bc,lc,mc,hc,giW,gij)

                                            enddo
                                            enddo
                                            enddo
                                            !if(abs(temp-1.0d0) .gt. 0.00000001d0) print*,'10 ',gij,temp

                                        case(4) !Payment optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y


                                            hj=GridH(hc)
                                            cj=sum(Phvals*consPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consPpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            mj=sum(Phvals*mPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            lj=sum(Phvals*lPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=uChi_h*cj
                                            bdisto(bc,lc,mc,hc,giW,gij)=bj

                                            hcc=hc


                                            call basefun(GridBsim,nbsim,bj,vals,inds)
                                            call basefun(GridMsim,nmsim,mj,valsm,indsm)
                                            call basefun(GridL,nl,lj,valsl,indsl)

                                            temp=0.0d0
                                            do lcc=1,2
                                            do bcc=1,2
                                                do mcc=1,2
                                                    !lcc=1 !lIPpolRet(bc,lc,mc,hc,giExo,Phinds(jcc),ijret)
                                                    distop(inds(bcc),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)=distop(inds(bcc),indsl(lcc),indsm(mcc),hcc,giWp,gij+1)+vals(bcc)*valsm(mcc)*valsl(lcc)*disto(bc,lc,mc,hc,giW,gij)
                                                    !temp=temp+vals(bcc)*valsm(mcc)*Phvals(jcc)
                                                enddo
                                            enddo
                                            enddo


                                    end select
                                enddo
                            enddo
                        enddo
                    enddo
                enddo




            !Dying people
            case(Jtot)

                do giW=1,ngpW
                    do bc=1,nbsim
                       if(distr(bc,giW,gij) .eq. 0.0d0) cycle
                        iW=giW
                        giWp=giW
                        giExo=(giAgg-1)*ngpW+giW
                        taxsimr(bc,giW,gij)=FnTax(pgrid(gij,giW))/pgrid(gij,giW)



                        hj=sum(Phvals*hconsRpolSim(Phinds,bc,giExo,gij))
                        bj=sum(Phvals*bRpolSim(Phinds,bc,giExo,gij))
                        cj=sum(Phvals*consRpolSim(Phinds,bc,giExo,gij))
                        cj2=sum(Phvals2*consRpolSim(Phinds2,bc,giExo,gij))


                        hconsdistr(bc,giW,gij)=hj
                        consdistr(bc,giW,gij)=cj
                        dpconsdistr(bc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                        dhconsdistr(bc,giW,gij)=0.0d0
                        beqdistr(bc,giW)=bj
                    enddo
                    do hc=1,nh
                        do mc=1,nmsim
                            do lc = 1,nl
                                do bc = 1,nbsim
                                   if(disto(bc,lc,mc,hc,giW,gij) .eq. 0.0d0) cycle
                                    iW=giW
                                    giWp=giW
                                    giExo=(giAgg-1)*ngpW+giW
                                    y=pgrid(gij,giW)


                                    helpWfore=sum(Phvals*WforeSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    helpWpay=sum(Phvals*WpaySim(Phinds,bc,lc,mc,hc,giExo,gij))
                                    helpWsell=sum(Phvals*WsellrentSim(Phinds,bc,lc,mc,hc,giExo,gij))



                                    helpVals(1) =   helpWfore
                                    helpVals(2) =   helpWsell
                                    helpVals(3) =   helpWpay




                                    polind=maxloc(helpVals(1:3),dim=1)


                                    select case(polind)

                                        case(1) !Foreclosure optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTax(y)/y


                                            cj=sum(Phvals*consFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consFpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bFpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
!                                            hj=GridH(hc)*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
                                            hj=cj*uChi_h

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj
                                            foredisto(bc,lc,mc,hc,giW,gij)=1.0d0


                                            beqdisto(bc,lc,mc,hc,giW)=bj

                                        case(2) !Selling optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y

                                            cj=sum(Phvals*consNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consNpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            hj=sum(Phvals*hconsNpolSim(Phinds,bc,lc,mc,hc,giExo,gij))


                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj


                                            selldisto(bc,lc,mc,hc,giW,gij)=1.0d0
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=hj

                                            beqdisto(bc,lc,mc,hc,giW)=bj


                                        case(3) !Payment optimal

                                            taxsimo(bc,lc,mc,hc,giW,gij)=FnTaxM(y,GridH(hc)*(GridMsim(mc)+GridL(lc)),min(0.0d0,bj))/y


                                            hj=GridH(hc)
                                            cj=sum(Phvals*consPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            cj2=sum(Phvals2*consPpolSim(Phinds2,bc,lc,mc,hc,giExo,gij))
                                            bj=sum(Phvals*bPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))
                                            mj=sum(Phvals*mPpolSim(Phinds,bc,lc,mc,hc,giExo,gij))

                                            dpconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/log(gelas)
!                                            dhconsdisto(bc,lc,mc,hc,giW,gij)=(log(cj2)-log(cj))/((gelas*gPh-GridMsim(mc))/(gPh-GridMsim(mc)))
                                            consdisto(bc,lc,mc,hc,giW,gij)=cj
                                            hconsdisto(bc,lc,mc,hc,giW,gij)=uChi_h*cj

                                            beqdisto(bc,lc,mc,hc,giW)=bj+hj*(gPh-mj)-FnAdj(gPh*hj)


                                    end select
                                enddo
                            enddo
                        enddo
                    enddo
                enddo

                agepathEpen(it,gij) = sum(zydist(Jwork,:)*pgrid(gij,:))

       end select


       agepathEbuy(it,ij) = (SUM(buydisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij))+SUM(buydistr(:,:,ij)*distr(:,:,ij)))
        agepathEsell(it,ij) = (SUM(selldisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)))
!        agepathEincown(it,ij) = SUM(ymat(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)*(1.0d0+buydisto(:,:,:,:,:,ij)-selldisto(:,:,:,:,:,ij))) + SUM(ymatr(:,:,ij)*distr(:,:,ij)*(buydistr(:,:,ij)))
!        agepathEincrent(it,ij) = SUM(ymatr(:,:,ij)*distr(:,:,ij)*(1.0d0-buydistr(:,:,ij)))+sum(ymat(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)*(selldisto(:,:,:,:,:,ij)-buydisto(:,:,:,:,:,ij)))
        agepathEincown(it,ij) = SUM(ymat(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij))
        agepathEincrent(it,ij) = SUM(ymatr(:,:,ij)*distr(:,:,ij))
        agepathEfore(it,ij) = (SUM(foredisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)))
        agepathErent(it,ij) = sum(hconsdistr(:,:,ij)*distr(:,:,ij)*(1.0d0-buydistr(:,:,ij)))+sum(hconsdisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)*(selldisto(:,:,:,:,:,ij)-buydisto(:,:,:,:,:,ij)))+(SUM(foredisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)*hconsdisto(:,:,:,:,:,ij)))
        agepathErefi(it,ij) = (SUM(refidisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)))
        agepathEcon(it,ij) = (SUM(consdisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij))+SUM(consdistr(:,:,ij)*distr(:,:,ij)))
        agepathEhcon(it,ij) = (SUM(hconsdisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij))+SUM(hconsdistr(:,:,ij)*distr(:,:,ij)))
        agepathEdh(it,ij) = sum(disto(:,:,:,:,:,ij)*dhconsdisto(:,:,:,:,:,ij))
        agepathEdp(it,ij) = sum(disto(:,:,:,:,:,ij)*dpconsdisto(:,:,:,:,:,ij))+sum(distr(:,:,ij)*dpconsdistr(:,:,ij))
        agepathEtax(it,ij) = sum(disto(:,:,:,:,:,ij)*taxsimo(:,:,:,:,:,ij))+sum(distr(:,:,ij)*taxsimr(:,:,ij))
        agepathElev(it,ij) = SUM((mmatj)*disto(:,:,:,:,:,ij))/gPh
       if(ij .lt. Jtot) then
          agepathEnw(it,ij) = SUM((bmatj+(gPh-mmatj-lmatj)*hmatj)*distop(:,:,:,:,:,ij+1))+SUM(bmatjr*distrp(:,:,ij+1))
          agepathEnwown(it,ij) = SUM((bmatj+(gPh-mmatj-lmatj)*hmatj)*distop(:,:,:,:,:,ij+1))
          agepathEnwrent(it,ij) = SUM(bmatjr*distrp(:,:,ij+1))
          agepathEown(it,ij) = SUM(distop(:,:,:,:,:,ij+1))
          agepathEass(it,ij) = (SUM(bmatj*distop(:,:,:,:,:,ij+1),MASK=bmatj.ge.0.0d0)+SUM(bmatjr*distrp(:,:,ij+1)))
          agepathEloc(it,ij) = SUM(bmatj*distop(:,:,:,:,:,ij+1),MASK=bmatj.lt.0.0d0)
          agepathEhouse(it,ij) = SUM(hmatj*distop(:,:,:,:,:,ij+1))
          agepathEmort(it,ij) = SUM(hmatj*mmatj*distop(:,:,:,:,:,ij+1))
          agepathEhnwshare(it,ij) = SUM(((gPh-mmatj-lmatj)*hmatj)/(bmatj+(gPh-mmatj-lmatj)*hmatj)*distop(:,:,:,:,:,ij+1),MASK=(bmatj+(gPh-mmatj-lmatj)*hmatj).GT.0.01d0)
          agepathEhnw(it,ij) = gPh*agepathEhouse(it,ij)-agepathEmort(it,ij)-agepathEloc(it,ij)
          agepathEmortpos(it,ij) = SUM(distop(:,:,2:nmsim,:,:,ij+1))
          agepathElocpos(it,ij) = SUM(distop(1:bzerosim-1,:,:,:,:,ij+1))
!          agepathElev(it,ij) = SUM((lmatj+mmatj)*distop(:,:,:,:,:,ij+1))/gPh
!          agepathElev(it,ij) = SUM((lmatj+mmatj)*disto(:,:,:,:,:,ij))/gPh
!          agepathElev(it,ij) = (agepathEmort(it,ij)-agepathEloc(it,ij))/(gPh*agepathEhouse(it,ij))
!          agepathElev(it,ij) = (agepathEmort(it,ij))/(gPh*agepathEhouse(it,ij))

       else

          agepathEnw(it,ij) = SUM((bmatj+(gPh-mmatj-lmatj)*hmatj)*disto(:,:,:,:,:,ij))+SUM(bmatjr*distr(:,:,ij))
          agepathEnwown(it,ij) = SUM((bmatj+(gPh-mmatj-lmatj)*hmatj)*disto(:,:,:,:,:,ij))
          agepathEnwrent(it,ij) = SUM(bmatjr*distr(:,:,ij))
          agepathEown(it,ij) = SUM(disto(:,:,:,:,:,ij)) !-agepathEsell(it,ij)-agepathEfore(it,ij)
          agepathEhnwshare(it,ij) = SUM(((gPh-mmatj-lmatj)*hmatj)/(bmatj+(gPh-mmatj-lmatj)*hmatj)*disto(:,:,:,:,:,ij),MASK=(bmatj+(gPh-mmatj-lmatj)*hmatj).GT.0.01d0)
          agepathEass(it,ij) = (SUM(bmatj*disto(:,:,:,:,:,ij))+SUM(bmatjr*distr(:,:,ij)))
          agepathEloc(it,ij) = SUM(lmatj*disto(:,:,:,:,:,ij))
          agepathEhouse(it,ij) = SUM(hmatj*disto(:,:,:,:,:,ij))
          agepathEmort(it,ij) = SUM(hmatj*mmatj*disto(:,:,:,:,:,ij))
          agepathEhnw(it,ij) = gPh*agepathEhouse(it,ij)-agepathEmort(it,ij)-agepathEloc(it,ij)
          agepathEmortpos(it,ij) = SUM(disto(:,:,2:nmsim,:,:,ij))


     endif
     agepathEconown(it,ij) = (SUM(consdisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij)))/sum(disto(:,:,:,:,:,ij)+1.0d-6)
     agepathEconrent(it,ij) = SUM(consdistr(:,:,ij)*distr(:,:,ij))/sum(distr(:,:,ij)+1.0d-6)


!     agepathEconinclow(it,ij) = (SUM(consdisto(:,:,:,:,1:2,ij)*disto(:,:,:,:,1:2,ij))+SUM(consdistr(:,1:2,ij)*distr(:,1:2,ij)))/(sum(disto(:,:,:,:,1:2,ij))+sum(distr(:,1:2,ij)))
!     agepathEconinchigh(it,ij) = (SUM(consdisto(:,:,:,:,4:5,ij)*disto(:,:,:,:,4:5,ij))+SUM(consdistr(:,4:5,ij)*distr(:,4:5,ij)))/(sum(disto(:,:,:,:,4:5,ij))+sum(distr(:,4:5,ij)))

     agepathEconlowltv(it,ij) = (SUM(consdisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij),MASK=(mmatj/gPh).lt.0.55d0))/(sum(disto(:,:,:,:,:,ij),MASK=(mmatj/gPh).lt.0.55d0)+1.0d-6)
     agepathEconhighltv(it,ij) = (SUM(consdisto(:,:,:,:,:,ij)*disto(:,:,:,:,:,ij),MASK=(mmatj/gPh).ge.0.55d0))/(sum(disto(:,:,:,:,:,ij),MASK=(mmatj/gPh).ge.0.55d0)+1.0d-6)

  enddo



!    print*,(sum(hconsdistr(:,:,:)*distr)+sum(hconsdisto(:,:,:,:,:,:)*disto))/(dble(Jtot)),Ht
!    Ht=(sum(hconsdistr(:,:,:)*distr)+sum(hconsdisto(:,:,:,:,:,:)*disto))/(dble(Jtot))



    pathEinc(it) = sum(zydist(1:Jwork,:)*ypsgrid(1:Jwork,gif,:,giAY))
    pathEcon(it) = (SUM(consdisto(:,:,:,:,:,:)*disto(:,:,:,:,:,:))+SUM(consdistr(:,:,:)*distr(:,:,:)))
    pathEhcon(it) = Pr*((SUM(hconsdisto(:,:,:,:,:,:)*disto(:,:,:,:,:,:))+SUM(hconsdistr(:,:,:)*distr(:,:,:))))
    pathEhcon(it) = pathEhcon(it)/(pathEhcon(it)+pathEcon(it))
!    pathEhconlow(it) = (Pr*((SUM(hconsdisto(:,:,:,:,1,1:5)*disto(:,:,:,:,1,1:5))+SUM(hconsdistr(:,1,1:5)*distr(:,1,1:5))))/((SUM(consdisto(:,:,:,:,1,1:5)*disto(:,:,:,:,1,1:5))+SUM(consdistr(:,1,1:5)*distr(:,1,1:5)))) +  Pr*((SUM(hconsdisto(:,:,:,:,1,Jwork-4:Jwork)*disto(:,:,:,:,1,Jwork-4:Jwork))+SUM(hconsdistr(:,1,Jwork-4:Jwork)*distr(:,1,Jwork-4:Jwork))))/((SUM(consdisto(:,:,:,:,1,Jwork-4:Jwork)*disto(:,:,:,:,1,Jwork-4:Jwork))+SUM(consdistr(:,1,Jwork-4:Jwork)*distr(:,1,Jwork-4:Jwork)))))/2.0d0
!    pathEhconhigh(it) = Pr*((SUM(hconsdisto(:,:,:,:,ngpzy,Jwork/2:Jwork)*disto(:,:,:,:,ngpzy,Jwork/2:Jwork))+SUM(hconsdistr(:,ngpzy,Jwork/2:Jwork)*distr(:,ngpzy,Jwork/2:Jwork))))/((SUM(consdisto(:,:,:,:,ngpzy,Jwork/2:Jwork)*disto(:,:,:,:,ngpzy,Jwork/2:Jwork))+SUM(consdistr(:,ngpzy,Jwork/2:Jwork)*distr(:,ngpzy,Jwork/2:Jwork))))
    pathEbuy(it) = (SUM(buydisto(:,:,:,:,:,:)*disto(:,:,:,:,:,:))+SUM(buydistr(:,:,:)*distr(:,:,:)))
    pathEsell(it) = (SUM(selldisto(:,:,:,:,:,:)*disto(:,:,:,:,:,:)))
    pathEown(it) = SUM(agepathEown(it,:))/real(Jtot) !sum(disto(:,:,:,:,:,:)) !+pathEbuy(it)-pathEsell(it)
    pathEass(it) = (SUM(bmat*disto)+SUM(bmatr*distr))
    pathEloc(it) = SUM(lmat*disto)
    pathEhouse(it) = SUM(hmat*disto)
    pathEmort(it) = SUM(mmat*hmat*disto)
    pathEhnw(it) = gPh*pathEhouse(it)-pathEmort(it)
    pathErefi(it) = (SUM(refidisto(:,:,:,:,:,:)*disto(:,:,:,:,:,:)))
    pathEfore(it) = (SUM(foredisto(:,:,:,:,:,:)*disto(:,:,:,:,:,:)))
    pathEmortpos(it) = SUM(disto(:,:,2:nm,:,:,:))
    pathEbeq(it) = (SUM(beqdisto*disto(:,:,:,:,:,Jtot))+SUM(beqdistr*distr(:,:,Jtot)))
    pathEbeqpos(it) = SUM(disto(:,:,:,:,:,Jtot),MASK=beqdisto.gt.0.0d0)+SUM(distr(:,:,Jtot),MASK=beqdistr.gt.0.0d0)
    distrp(:,:,1)=0.0d0
    if(posinitw .eq. 0) then
       distrp(bzerosim,:,1)=zydist(1,:)
    else
       distrp(bzerosim,:,1)=zydist(1,:)*initadist(:,1)
!       print*,pathEbeq(it)
!       print*,initnw
       do izy=1,ngpzy
!          bj=initashare(izy)*min(pathEbeq(it),2.0d0)/(initadist(izy,2)*zydist(1,izy))
          bj=initnw(izy)
          call basefun(GridBsim,nbsim,bj,vals,inds)
          distrp(inds(1),izy,1)=distrp(inds(1),izy,1)+vals(1)*zydist(1,izy)*initadist(izy,2)
          distrp(inds(2),izy,1)=distrp(inds(2),izy,1)+vals(2)*zydist(1,izy)*initadist(izy,2)
       enddo
       if(abs(sum(distrp(:,:,1))-1.0d0) .gt. 1.0d-6) then
          print*,'Something wrong initial dist'
          print*,sum(distrp(:,:,1))
          pause
       endif
    endif
    disto=distop
    distr=distrp


enddo
if(DoIRF .eq. 0 .and. exogenous .eq. 0) then
   distsso=disto
   distssr=distr
   Hsteady=Ht
endif
if(DoExu .eq. 1) then
   distssoexu=disto
   distssrexu=distr
   Htexu=Ht
endif


allocate(nw(nh*nmsim*nl*nbsim+nbsim))
allocate(nwdist(nh*nmsim*nl*nbsim+nbsim))
allocate(nworder(nh*nmsim*nl*nbsim+nbsim))
allocate(nwy((nh*nmsim*nl*nbsim+nbsim)*ngpzy*Jwork))
allocate(nwydist((nh*nmsim*nl*nbsim+nbsim)*ngpzy*Jwork))
allocate(nwyorder((nh*nmsim*nl*nbsim+nbsim)*ngpzy*Jwork))
allocate(hy((nh*nmsim*nl*nbsim)*ngpzy*Jwork))
allocate(hydist((nh*nmsim*nl*nbsim)*ngpzy*Jwork))
allocate(hyorder((nh*nmsim*nl*nbsim)*ngpzy*Jwork))
allocate(Si(nh*nmsim*nl*nbsim+nbsim))
allocate(Si1(nh*nmsim*nl*nbsim+nbsim))
allocate(hnw(nh*nmsim*nl*nbsim))
allocate(ltv(nh*nmsim*nl*nbsim))
allocate(ltvdist(nh*nmsim*nl*nbsim))
allocate(hnwdist(nh*nmsim*nl*nbsim))
allocate(hnworder(nh*nmsim*nl*nbsim))
allocate(ltvorder(nh*nmsim*nl*nbsim))
allocate(hnwshare(nh*nmsim*nl*nbsim))
allocate(hnwsharedist(nh*nmsim*nl*nbsim))
allocate(hnwshareorder(nh*nmsim*nl*nbsim))
allocate(liq(nbsim))
allocate(liqdist(nbsim))
allocate(liqorder(nbsim))




index1=0
index2=0
DO hc=1,nh
DO mc=1,nmsim
DO lc=1,nl
DO bc=1,nbsim
    index1=index1+1
    nw(index1)=GridBsim(bc)+(gPh-(GridL(lc)+GridMsim(mc)))*GridH(hc)
    nwdist(index1)=sum(distsso(bc,lc,mc,hc,:,:))
    hnw(index1)=(gPh-(GridL(lc)+GridMsim(mc)))*GridH(hc)
    if(nw(index1) .ne. 0) then

       hnwshare(index1)=hnw(index1)/nw(index1)
    else
       hnwshare(index1)=1.0d0
    endif

    hnwdist(index1)=sum(distsso(bc,lc,mc,hc,:,:))
    do ij=1,Jwork
    do giz=1,ngpzy
       index2=index2+1
       nwy(index2)=(GridBsim(bc)+(gPh-(GridL(lc)+GridMsim(mc)))*GridH(hc))/ypsgrid(ij,1,giz,giAy)
       nwydist(index2)=distsso(bc,lc,mc,hc,giz,ij)
       hy(index2)=(gPh*GridH(hc))/ypsgrid(ij,1,giz,giAy)
       hydist(index2)=distsso(bc,lc,mc,hc,giz,ij)

    enddo
    enddo

!    liq(index1)=GridBsim(bc)
!    liqdist(index1)=sum(distsso(bc,lc,mc,hc,:,:))
    ltv(index1)=GridMsim(mc)/gPh
!    ltvdist(index1)=sum(distsso(bc,lc,mc,hc,:,:))
!    liqinc(index1)=GridBsim(bc)+(gPh-(GridL(lc)+GridMsim(mc)))*GridH(hc)
!    liqincdist(index1)=sum(distsso(bc,lc,mc,hc,:,:))


ENDDO
ENDDO
ENDDO
ENDDO



DO bc=1,nbsim
    index1=index1+1
    nw(index1)=GridBsim(bc)
    nwdist(index1)=sum(distssr(bc,:,:))

    liq(bc)=GridBsim(bc)
    liqdist(bc)=sum(distssr(bc,:,:))+sum(distsso(bc,:,:,:,:,:))

    do ij=1,Jwork
    do giz=1,ngpzy
       index2=index2+1
       nwy(index2)=(GridBsim(bc))/ypsgrid(ij,1,giz,giAy)
       nwydist(index2)=distssr(bc,giz,ij)
    enddo
    enddo



ENDDO

!hnw=nw(1:nh*nmsim*nl*nbsim)-liq(1:nh*nmsim*nl*nbsim)
!hnwshare=hnw/(nw(1:nh*nmsim*nl*nbsim)+1.0d-10)

!hnwdist=nwdist
!hnwsharedist=nwdist

nwdist=nwdist/sum(nwdist)
nwydist=nwydist/sum(nwydist)
hydist=hydist/sum(hydist)
hnwdist=hnwdist/sum(hnwdist)
!hnwsharedist=hnwshar/sum(hnwsharedist)
liqdist=liqdist/sum(liqdist)
!ltvdist=ltvdist/sum(ltvdist)
hnwsharedist=hnwdist
ltvdist=hnwdist

call quick_sort(nw,nworder)
call quick_sort(nwy,nwyorder)
call quick_sort(hy,hyorder)
call quick_sort(hnw,hnworder)
call quick_sort(hnwshare,hnwshareorder)
call quick_sort(liq,liqorder)
call quick_sort(ltv,ltvorder)

tempsum1=0.0d0
tempsum2=nwdist(nworder(1))
do ij=1,nh*nmsim*nl*nbsim+nbsim-1
   if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .gt. 0.1d0)) then
      nwperc(1)=nw(ij)
   endif
   if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .gt. 0.25d0)) then
      nwperc(2)=nw(ij)
   endif
   if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .gt. 0.5d0)) then
      nwperc(3)=nw(ij)
   endif
   if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .gt. 0.75d0)) then
      nwperc(4)=nw(ij)
   endif
   if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .gt. 0.9d0)) then
      nwperc(5)=nw(ij)
      exit
   endif

   tempsum1=tempsum2
   tempsum2=tempsum2+nwdist(nworder(ij+1))

enddo

tempsum1=0.0d0
tempsum2=nwydist(nwyorder(1))
do ij=1,(nh*nmsim*nl*nbsim+nbsim)*ngpzy*Jwork-1
   if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .gt. 0.1d0)) then
      nwyperc(1)=nwy(ij)
   endif
   if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .gt. 0.25d0)) then
      nwyperc(2)=nwy(ij)
   endif
   if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .gt. 0.5d0)) then
      nwyperc(3)=nwy(ij)
   endif
   if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .gt. 0.75d0)) then
      nwyperc(4)=nwy(ij)
   endif
   if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .gt. 0.9d0)) then
      nwyperc(5)=nwy(ij)
      exit
   endif

   tempsum1=tempsum2
   tempsum2=tempsum2+nwydist(nwyorder(ij+1))

enddo

tempsum1=0.0d0
tempsum2=hydist(hyorder(1))
do ij=1,(nh*nmsim*nl*nbsim)*ngpzy*Jwork-1
   if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .gt. 0.1d0)) then
      hyperc(1)=hy(ij)
   endif
   if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .gt. 0.25d0)) then
      hyperc(2)=hy(ij)
   endif
   if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .gt. 0.5d0)) then
      hyperc(3)=hy(ij)
   endif
   if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .gt. 0.75d0)) then
      hyperc(4)=hy(ij)
   endif
   if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .gt. 0.9d0)) then
      hyperc(5)=hy(ij)
      exit
   endif

   tempsum1=tempsum2
   tempsum2=tempsum2+hydist(hyorder(ij+1))

enddo


tempsum1=0.0d0
tempsum2=liqdist(liqorder(1))
do ij=1,nbsim-1
   if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .gt. 0.1d0)) then
      liqperc(1)=liq(ij)
   endif
   if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .gt. 0.25d0)) then
      liqperc(2)=liq(ij)
   endif
   if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .gt. 0.5d0)) then
      liqperc(3)=liq(ij)
   endif
   if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .gt. 0.75d0)) then
      liqperc(4)=liq(ij)
   endif
   if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .gt. 0.9d0)) then
      liqperc(5)=liq(ij)
      exit
   endif

   tempsum1=tempsum2
   tempsum2=tempsum2+liqdist(liqorder(ij+1))

enddo

tempsum1=0.0d0
tempsum2=hnwdist(hnworder(1))
do ij=1,nh*nmsim*nl*nbsim-1
   if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .gt. 0.1d0)) then
      hnwperc(1)=hnw(ij)
   endif
   if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .gt. 0.25d0)) then
      hnwperc(2)=hnw(ij)
   endif
   if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .gt. 0.5d0)) then
      hnwperc(3)=hnw(ij)
   endif
   if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .gt. 0.75d0)) then
      hnwperc(4)=hnw(ij)
   endif
   if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .gt. 0.9d0)) then
      hnwperc(5)=hnw(ij)
      exit
   endif

   tempsum1=tempsum2
   tempsum2=tempsum2+hnwdist(hnworder(ij+1))

enddo

tempsum1=0.0d0
tempsum2=hnwsharedist(hnwshareorder(1))
do ij=1,nh*nmsim*nl*nbsim-1
   if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .gt. 0.1d0)) then
      hnwshareperc(1)=hnwshare(ij)
   endif
   if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .gt. 0.25d0)) then
      hnwshareperc(2)=hnwshare(ij)
   endif
   if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .gt. 0.5d0)) then
      hnwshareperc(3)=hnwshare(ij)
   endif
   if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .gt. 0.75d0)) then
      hnwshareperc(4)=hnwshare(ij)
   endif
   if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .gt. 0.9d0)) then
      hnwshareperc(5)=hnwshare(ij)
      exit
   endif

   tempsum1=tempsum2
   tempsum2=tempsum2+hnwsharedist(hnwshareorder(ij+1))

enddo

gij=0
bc=1
do while( (ltv(bc) .le. 0.0d0) .and. (bc .lt. nh*nmsim*nl*nbsim-1))
   bc = bc + 1
enddo

nomortmeas = sum(ltvdist(ltvorder(1:bc-1)))
tempsum1=0.0d0
tempsum2=ltvdist(ltvorder(bc))/(1.0d0-nomortmeas)

do ij=bc,nh*nmsim*nl*nbsim-1
!   if(ltv(ij) .eq. 0.0d) then


!   elseif(gij .eq. 0) then
!      gij=1

!   else
      if( (tempsum1 .LE. 0.10d0) .and. (tempsum2 .ge. 0.1d0)) then
         ltvperc(1)=ltv(ij)
      endif
      if( (tempsum1 .LE. 0.25d0) .and. (tempsum2 .ge. 0.25d0)) then
         ltvperc(2)=ltv(ij)
      endif
      if( (tempsum1 .LE. 0.50d0) .and. (tempsum2 .ge. 0.50d0)) then
         ltvperc(3)=ltv(ij)
      endif
      if( (tempsum1 .LE. 0.75d0) .and. (tempsum2 .ge. 0.75d0)) then
         ltvperc(4)=ltv(ij)
      endif
      if( (tempsum1 .LE. 0.90d0) .and. (tempsum2 .ge. 0.90d0)) then
         ltvperc(5)=ltv(ij)
         exit
      endif

   tempsum1=tempsum2
   tempsum2=tempsum2+ltvdist(ltvorder(ij+1))/(1.0d0-nomortmeas)

enddo





Si(1)=nw(1)*nwdist(nworder(1))
Si1(1)=0.0d0
do index1=2,nh*nmsim*nl*nbsim+nbsim
    Si(index1)=Si(index1-1)+nw(index1)*nwdist(nworder(index1))
    Si1(index1)=Si(index1-1)
enddo
Gini=1.0d0-SUM(nwdist(nworder)*(Si+Si1))/Si(nh*nmsim*nl*nbsim+nbsim);


print*, 'Wealth Gini:', Gini
do gij=1,Jtot

Vlogcon(gij)=((SUM(log(consdisto(:,:,:,:,:,gij)+cmin)**2.0d0*disto(:,:,:,:,:,gij))+SUM((log(consdistr(:,:,gij)+cmin)**2.0d0*distr(:,:,gij)))))-((SUM(log(consdisto(:,:,:,:,:,gij)+cmin)*disto(:,:,:,:,:,gij))+SUM((log(consdistr(:,:,gij)+cmin)*distr(:,:,gij)))))**2.0d0
Vloghcon(gij)=((SUM(log(hconsdisto(:,:,:,:,:,gij)+cmin)**2.0d0*disto(:,:,:,:,:,gij))+SUM((log(hconsdistr(:,:,gij)+cmin)**2.0d0*distr(:,:,gij)))))-((SUM(log(hconsdisto(:,:,:,:,:,gij)+cmin)*disto(:,:,:,:,:,gij))+SUM((log(hconsdistr(:,:,gij)+cmin)*distr(:,:,gij)))))**2.0d0

if(gij .le. Jwork) then
   Einc(gij)=sum(zydist(gij,:)*ypsgrid(gij,gif,:,giAY))
   Vloginc(gij)=(sum(zydist(gij,:)*(log(ypsgrid(gij,gif,:,giAY))**2.0d0)))-(sum(zydist(gij,:)*(log(ypsgrid(gij,gif,:,giAY)))))**2.0d0
else
   Epen(gij)=sum(zydist(Jwork,:)*pgrid(gij,:))
   Einc(gij)=sum(zydist(Jwork,:)*pgrid(gij,:))
   Vloginc(gij)=(sum(zydist(Jwork,:)*(log(pgrid(gij,:))**2.0d0)))-(sum(zydist(Jwork,:)*(log(pgrid(gij,:)))))**2.0d0
endif
enddo
OPEN(3, FILE = trim(OutputDir) // 'Vlogcon.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Vlogcon)
OPEN(3, FILE = trim(OutputDir) // 'Vloghcon.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Vloghcon)
OPEN(3, FILE = trim(OutputDir) // 'Vloginc.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Vloginc)

if(1 .eq. 0) then
print*, 'Exp share h:', sum(pathEhcon(250:Tsim))/((dble(Tsim)-249.0d0))
print*, 'Dem Elasticity:',sum(Demelas(250:Tsim))/((dble(Tsim)-249.0d0))
print*, 'Avg tax:', sum(agepathEtax(250:Tsim,:))/(dble(Jtot)*(dble(Tsim)-249.0d0))
print*, 'Avg house:', sum(pathEhouse(250:Tsim))/((dble(Tsim)-249.0d0)*dble(Jtot))
print*, 'Avg own:', sum(pathEown(250:Tsim))/((dble(Tsim)-249.0d0))
print*, 'Avg mort:', sum(pathEmort(250:Tsim))/((dble(Tsim)-249.0d0)*dble(Jtot))
print*, 'Avg hnw:', sum(pathEhnw(250:Tsim))/((dble(Tsim)-249.0d0)*dble(Jtot))
print*, 'Avg ass:', sum(pathEass(250:Tsim))/((dble(Tsim)-249.0d0)*dble(Jtot))
print*, 'Avg nw:', (sum(pathEass(250:Tsim))/((dble(Tsim)-249.0d0))+sum(pathEhnw(250:Tsim))/((dble(Tsim)-249.0d0)))/dble(Jtot)
print*, 'Avg lev:', sum(pathEmort(250:Tsim))/sum(Phpath(250:Tsim)*pathEhouse(250:Tsim))
print*, 'Avg beq:', sum(pathEbeq(250:Tsim))/((dble(Tsim)-249.0d0))
print*, 'Frac beq>0:', sum(pathEbeqpos(250:Tsim))/((dble(Tsim)-249.0d0))
print*, 'Avg rent:', ((sum(agepathErent(250:Tsim,:))/(dble(Jtot)*(dble(Tsim)-249.0d0)-sum(agepathEown(250:Tsim,:)))))
print*, 'Avg incrent:', (sum(agepathEincrent(250:Tsim,1:Jwork))/(dble(Jwork)*(dble(Tsim)-249.0d0)-sum(agepathEown(250:Tsim,1:Jwork))))/(sum(agepathEincown(250:Tsim,1:Jwork))/sum(agepathEown(250:Tsim,1:Jwork)))
print*, 'Equity<0: ', sum(hnwdist(ltvorder),MASK=ltv.ge.1.0d0)
print*, 'Equity<10: ', sum(hnwdist(ltvorder),MASK=ltv.ge.0.9d0)
print*, 'Equity<20: ', sum(hnwdist(ltvorder),MASK=ltv.ge.0.8d0)
print*, 'Equity<30: ', sum(hnwdist(ltvorder),MASK=ltv.ge.0.7d0)
print*, 'Own<30:', sum(agepathEown(250:Tsim,1:4))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own<32:', sum(agepathEown(250:Tsim,1:5))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own<34:', sum(agepathEown(250:Tsim,1:6))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own<36:', sum(agepathEown(250:Tsim,1:7))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own>70:', sum(agepathEown(250:Tsim,Jwork+4:Jtot))/((dble(Tsim)-249.0d0)*dble(Jtot-Jwork-3))
print*, 'Turnover:', sum(agepathEsell(250:Tsim,:))/(dble(Jtot)*(dble(Tsim)-249.0d0))
print*, 'Low Share:', sum(pathEhconlow(250:Tsim))/((dble(Tsim)-249.0d0))
print*, 'High Share:', sum(pathEhconhigh(250:Tsim))/((dble(Tsim)-249.0d0))



else

print*, 'Exp share h:', sum(pathEhcon(itstop:Tsim))/((dble(Tsim-itstop+1)))
print*, 'Dem Elasticity:',sum(Demelas(itstop:Tsim))/((dble(Tsim-itstop+1)))
print*, 'Avg tax:', sum(agepathEtax(itstop:Tsim,:))/(dble(Jtot)*(dble(Tsim-itstop+1)))
print*, 'Avg house:', sum(pathEhouse(itstop:Tsim))/((dble(Tsim-itstop+1))*dble(Jtot))
print*, 'Avg own:', sum(pathEown(itstop:Tsim))/((dble(Tsim-itstop+1)))
print*, 'Avg mort:', sum(pathEmort(itstop:Tsim))/((dble(Tsim-itstop+1))*dble(Jtot))
print*, 'Avg hnw:', sum(pathEhnw(itstop:Tsim))/((dble(Tsim-itstop+1))*dble(Jtot))
print*, 'Avg ass:', sum(pathEass(itstop:Tsim))/((dble(Tsim-itstop+1))*dble(Jtot))
print*, 'Avg nw:', (sum(pathEass(itstop:Tsim))/((dble(Tsim-itstop+1)))+sum(pathEhnw(itstop:Tsim))/((dble(Tsim-itstop+1))))/dble(Jtot)
print*, 'Avg lev:', sum(pathEmort(itstop:Tsim))/sum(Phpath(itstop:Tsim)*pathEhouse(itstop:Tsim))
print*, 'Avg beq:', sum(pathEbeq(itstop:Tsim))/((dble(Tsim-itstop+1)))
print*, 'Frac beq>0:', sum(pathEbeqpos(itstop:Tsim))/((dble(Tsim-itstop+1)))
print*, 'Avg rent:', sum(agepathErent(itstop:Tsim,:))/((dble(Tsim-itstop+1))*dble(Jtot))
!print*, 'Avg rent:', ((sum(agepathErent(itstop:Tsim,:))/(dble(Jtot)*(dble(Tsim-itstop+1))-sum(agepathEown(itstop:Tsim,:)))))

!print*, 'Avg incrent:', (sum(agepathEincrent(itstop:Tsim,1:Jwork))/(dble(Jwork)*(dble(Tsim-itstop+1))-sum(agepathEown(itstop:Tsim,1:Jwork))))/(sum(agepathEincown(itstop:Tsim,1:Jwork))/sum(agepathEown(itstop:Tsim,1:Jwork)))
print*, 'Avg incrent:', (sum(agepathEincrent(itstop:Tsim,1:Jwork))/(dble(Jwork)*(dble(Tsim-itstop+1))-sum(agepathEown(itstop:Tsim,1:Jwork))))/(sum(agepathEincown(itstop:Tsim,1:Jwork))/sum(agepathEown(itstop:Tsim,1:Jwork)))
print*, 'Equity<0: ', sum(hnwdist(ltvorder),MASK=ltv.ge.1.0d0)
print*, 'Equity<10: ', sum(hnwdist(ltvorder),MASK=ltv.ge.0.9d0)
print*, 'Equity<20: ', sum(hnwdist(ltvorder),MASK=ltv.ge.0.8d0)
print*, 'Equity<30: ', sum(hnwdist(ltvorder),MASK=ltv.ge.0.7d0)
print*, 'Own<30:', sum(agepathEown(itstop:Tsim,1:4))/((dble(Tsim-itstop+1))*4.0d0)
print*, 'Own<32:', sum(agepathEown(250:Tsim,1:5))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own<34:', sum(agepathEown(250:Tsim,1:6))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own<36:', sum(agepathEown(250:Tsim,1:7))/((dble(Tsim)-249.0d0)*4.0d0)
print*, 'Own>70:', sum(agepathEown(itstop:Tsim,Jwork+4:Jtot))/((dble(Tsim-itstop+1))*dble(Jtot-Jwork-3))
print*, 'Turnover:', sum(agepathEsell(itstop:Tsim,:))/(dble(Jtot)*(dble(Tsim-itstop+1)))
print*, 'Low Share:', sum(pathEhconlow(itstop:Tsim))/((dble(Tsim-itstop+1)))
print*, 'High Share:', sum(pathEhconhigh(itstop:Tsim))/((dble(Tsim-itstop+1)))


endif
do hc=1,nh
print*,'Frac own H:          ', hc,sum(distsso(:,:,:,hc,:,:))/sum(distsso)
enddo
do hc=1,5
print*,'NW Dists: ',hc,nwperc(hc)
enddo
do hc=1,5
print*,'HNW Dists: ',hc,hnwperc(hc)
enddo
do hc=1,5
print*,'HNW Share Dists: ',hc,hnwshareperc(hc)
enddo
do hc=1,5
print*,'Liq Dists: ',hc,liqperc(hc)
enddo
do hc=1,5
print*,'NW/Y Dists: ',hc,nwyperc(hc)
enddo
do hc=1,5
print*,'H/Y Dists: ',hc,hyperc(hc)
enddo
do hc=1,5
print*,'LTV Dists: ',hc,ltvperc(hc)
enddo
print*,'Frac No Mort: ',nomortmeas
print*, 'Mean(NW/Y)',sum(nwy*nwydist(nwyorder))/sum(nwydist(nwyorder))




deallocate(nw)
deallocate(nwdist)
deallocate(nworder)
deallocate(nwy)
deallocate(nwydist)
deallocate(nwyorder)
deallocate(hy)
deallocate(hydist)
deallocate(hyorder)
deallocate(hnw)
deallocate(hnwdist)
deallocate(hnworder)
deallocate(hnwshare)
deallocate(hnwsharedist)
deallocate(hnwshareorder)
deallocate(liq)
deallocate(liqdist)
deallocate(liqorder)
deallocate(ltvorder)
deallocate(ltv)


deallocate(Si)
deallocate(Si1)


deallocate(pmsim)

deallocate(hconsRpolOSim)
deallocate(consRpolOSim)
deallocate(bRpolOSim)
deallocate(hconsBpolOSim)
deallocate(consBpolOSim)
deallocate(bBpolOSim)
deallocate(mBpolOSim)
deallocate(lBpolOSim)
deallocate(hBpolOSim)


deallocate(WforeSim)
deallocate(WrefinSim)
deallocate(WpaySim)
deallocate(WsellbuySim)
deallocate(WsellrentSim)
deallocate(hconsdistop)
deallocate(hconsdistrp)
deallocate(consdistop)
deallocate(consdistrp)
deallocate(beqdisto)
deallocate(beqdistr)
deallocate(taxsimo)
deallocate(taxsimr)


deallocate(hconsRpolSim)
deallocate(consRpolSim)
deallocate(bRpolSim)
deallocate(hconsBpolSim)
deallocate(consBpolSim)
deallocate(bBpolSim)
deallocate(mBpolSim)
deallocate(lBpolSim)
deallocate(hBpolSim)

deallocate(WrentSim)
deallocate(WbuySim)


deallocate(hconsPpolSim)
deallocate(consPpolSim)
deallocate(bPpolSim)
deallocate(mPpolSim)
deallocate(lPpolSim)


deallocate(hconsNpolSim)
deallocate(consNpolSim)
deallocate(bNpolSim)
deallocate(mNpolSim)
deallocate(lNpolSim)

deallocate(hconsFpolSim)
deallocate(consFpolSim)
deallocate(bFpolSim)







end subroutine SimulateStochGEDist

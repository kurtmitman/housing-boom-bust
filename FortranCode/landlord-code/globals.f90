!****************************************************************************
!
!  PURPOSE:  Module for global variables
!
!  VERSION: 0.1, 12-June-2012
!
!  LAST EDITED BY: Kurt
!
!  CHANGES SINCE PREV VERSION:
!****************************************************************************


module Globals
use params

implicit none

!GLOBALS FOR DIRECTORIES
character(len=100)  OutputDir
character(len=100)  OutputDirSims
character(len=100)  InputParamFile
integer :: AdjustCostModel
integer :: NoBankBelief
integer :: OnlyBankBelief
integer :: NoRentalBelief
integer :: nhr
integer :: DoIRF
integer :: DoRf
integer :: DoMqm
integer :: Modify
integer :: beliefshock
integer :: RationInc(ngpAgg)
double precision :: RationIncLevel
double precision ::Htexu,liquid,InterestCap
!Threadprivate counters
integer :: GeRent,HELOCVal,payextra
integer :: gij,giPh,giPhp,giPhh,giPhhp,giPhr,giPhrp,giW,giWp,giR,gif,giz,gizp,ghcc,gih,gil,gim,giAgg,giHD,giAy,giAggp,giCD,giExo,giExop,giC,giZh,girf
double precision :: gliq,gmort,ghouse,gloc,gPh,grf,grm,rf_base,grl,gPr
double precision :: rent_tax
double precision    :: gvals(2)
integer             :: ginds(2)
integer             :: MtoMsim(nmsim)
integer             :: bcinds(2,nbsim)
double precision    :: adjust,kappa
double precision    :: defwedge
double precision    :: Goldmine(7)
double precision    :: Goldminevals(7)
double precision    :: bcvals(2,nbsim)
double precision    :: bet_h
double precision    :: rent_elas
double precision    :: adjval,hservices
double precision    :: MortDeduct,mortmarkup,helocmarkup
double precision    :: ltax,tautax
double precision    :: beqlev,beqmin
double precision    ::bmin,Ar
integer             :: exurent
double precision    :: scoef1(ngpPh),scoef2(ngpPh),scoef3(ngpPh)
double precision    :: bppcoef
double precision    :: mjprob,mjscale,Gini
double precision    :: HDX,HDY,HDZ
!Steady state simulations
double PRECISION, allocatable, dimension(:,:)::hsimss,msimss,lsimss,bsimss,hconssimss,csimss,beqsimss,psimss
INTEGER, allocatable, dimension(:,:)::hsimIss,buysimss,ownsimss,lsimIss,refisimss,foresimss,sellsimss
integer             :: iAggsteady
integer             :: DoSims
double precision    :: disutil(Jret)
double precision    :: GridBelief(ngpHD)
double precision    :: GridDemand(ngpNu)
double precision    :: GridAY(ngpAY)
double precision    :: Hsteady
double precision    :: GridRf(ngpRf)
double precision    :: GridDTI(ngpC)
double precision    :: Trans(ngpPh)
double precision    :: RfGrid(ngpAgg)
double precision    :: AdjGrid(ngpAgg)
double precision    :: AyGrid(ngpAgg)
integer :: DoInterestCap(ngpAgg)
!Rental cost conversion
double precision    :: phi_h

!Housing Depreciation
double precision    :: deltah,zbar

INTEGER             :: tstart,tend

double precision    :: initashare(ngpzy)
double precision    :: initadist(ngpzy,2)

!Mortgage Fixed Cost
double precision    :: MortFC

!Housing Maintenance Cost
double precision    :: HMRate

!Utility Cost of Foreclosure
double precision    :: epsprph,epsphy
double precision    :: xi

!Utility of homeownership

double precision    :: omega

double precision    :: GSElim
integer             :: GSE
!Economies of scale

double precision    :: phi(Jtot)
double precision    :: nchild(Jtot*2)
double precision    :: adults(Jtot*2)

! Risk aversion

double precision    :: sig

! CES Utility function params

double precision    :: alph
double precision    :: nu

! Discount Factor

double precision    :: bet

!Mortality Shock

double precision    :: delt(Jret)

!Moving Shock

double precision    :: thet(Jwork)

double precision    :: HELOCmax
double precision    :: LTVmax

double precision    :: LTVGrid(ngpAgg)
double precision    :: RmGrid(ngpAgg)
double precision    :: WedgeGrid(ngpAgg)
double precision    :: PhiHGrid(ngpAgg)
double precision    :: Markupgrid(ngpAgg)
double precision    :: RlGrid(ngpAgg)
double precision    :: LTIGrid(ngpAgg)
double precision    :: HELOCGrid(ngpAgg)
double precision    :: FCGrid(ngpAgg)
double precision    :: NuGrid(ngpAgg)
double precision    :: GridPhiH(ngpC)
double precision    :: ZhGrid(ngpAgg)
double precision    :: GridPh(ngpPhh)
double precision    :: GridPr(ngpPhr)
double precision    :: GridPrPh(ngpPhr)
! DRS For Housing

double precision    :: alpha_h


!Grids for assets etc

double precision    :: GridB(nb)
double precision    :: GridBsim(nbsim)
double precision    :: GridMsim(nmsim)
double precision    :: GridBS(nbs)
double precision    :: GridM(nm)
double precision    :: GridL(nl)
double precision    :: GridH(nh)
double precision    :: PhGrid(ngpPh)
double precision    :: PrGrid(ngpPh)
double precision    :: PrPhGrid(ngpPh)
double precision    :: GridC(ngpC)
double precision    :: GridZh(ngpZh)

integer             :: WtoRI(ngpW)
integer             :: AtoY(ngpAgg)
integer             :: AtoR(ngpAgg)
integer             :: AtoD(ngpAgg)
integer             :: AtoC(ngpAgg)
integer             :: AtoZ(ngpAgg)
integer             :: CHZYtoA(ngpC,ngpHD,ngpZh,ngpAy)

!Coefficients for the forecast

double precision    :: aa0(ngpAgg,ngpAgg),aa1(ngpAgg,ngpAgg),aa2(ngpAgg,ngpAgg)
double precision    :: aa0p(ngpAgg,ngpAgg),aa1p(ngpAgg,ngpAgg),aa2p(ngpAgg,ngpAgg)
double precision    :: bb0(ngpAgg,ngpAgg),bb1(ngpAgg,ngpAgg),bb2(ngpAgg,ngpAgg)
double precision    :: bb0p(ngpAgg,ngpAgg),bb1p(ngpAgg,ngpAgg),bb2p(ngpAgg,ngpAgg)

double precision    :: PtoV1(ngpPh,ngpAgg,ngpAgg),PtoV2(ngpPh,ngpAgg,ngpAgg)
integer             :: PtoP1(ngpPh,ngpAgg,ngpAgg),PtoP2(ngpPh,ngpAgg,ngpAgg)
double precision :: EPhp(ngpPh,ngpAgg),Phpmat(ngpPh,ngpAgg,ngpAgg)

double precision    :: PrtoV1(ngpPh,ngpAgg,ngpAgg),PrtoV2(ngpPh,ngpAgg,ngpAgg)
integer             :: PrtoP1(ngpPh,ngpAgg,ngpAgg),PrtoP2(ngpPh,ngpAgg,ngpAgg)
double precision :: EPrp(ngpPh,ngpAgg),Prpmat(ngpPh,ngpAgg,ngpAgg)

double precision,dimension(ngpPh)   ::XSHdem
double precision,dimension(ngpPhh,ngpPhr)   ::xsdemand
double precision,dimension(ngpPhh,ngpPhr)   ::xsdemand1
double precision,dimension(ngpPhh,ngpPhr)   ::xsdemand2

! Mortgage interest rate
double precision    :: rm
double precision    :: rl
double precision    :: ru

double precision    :: rf

! Transition for Aggregate Price
double precision    :: transMatPh(ngpPh,ngpPh)
double precision    :: transMatAgg(ngpAgg,ngpAgg)

! Transition for Individual State
double precision    :: transMatC(ngpC,ngpC)

! Transition for Individual State
double precision    :: transMatRf(ngpRf,ngpRf)

! Transition for Individual State
double precision    :: transMatZh(ngpZh,ngpZh)

! Transition for Individual State
double precision    :: transMatHD(ngpHD,ngpHD)

! Transition for Individual State
double precision    :: transMatAy(ngpAy,ngpAy)

! Transition for Individual State
double precision    :: transMatW(ngpW,ngpW)




TYPE SimulationType
        integer, dimension(nsim)   :: jsim,psimI,eysimI,zysimI,buysim,sellsim,foresim,lsimI,ownsim,refisim,hsimI,diesimI,thetasim
        double precision, dimension(nsim)   :: ysim,ypssim,csim,hsim,hconssim,psim,lsim,taxsim,asim,msim,bsim,qlsim,qmsim,beqsim,hnwsim,delhnwsim,nwsim,delnwsim,mpcsim,csim2
END TYPE SimulationType
type(SimulationType)                    :: simtrans(1:500)    !-1 is previous steady state, Ttransition+1 is to store ass and dur decisions in simulation



double precision   :: Phpath(-Jtot:Tsim),AYpath(-Jtot:Tsim),HDpath(-Jtot:Tsim),Prpath(-Jtot:Tsim),Htpath(-Jtot:Tsim)
integer    :: iPhpath(-Jtot:Tsim),iAYpath(-Jtot:Tsim),iHDpath(-Jtot:Tsim),iAggpath(-Jtot:Tsim)
integer    :: DoExu

!Working Life Value and Policy Functions

double precision, dimension(nbsim,ngpW,Jtot) :: distssr,distssrexu,consdistr,dpconsdistr,hconsdistr,hdemdistr,bdistr,buydistr,dhconsdistr
double precision, allocatable, dimension(:,:,:,:,:,:) :: distsso,distssoexu,consdisto,dpconsdisto,hconsdisto,bdisto,buydisto,foredisto,refidisto,selldisto,dhconsdisto,hdemdisto
double precision, dimension(nbsim,nl,nmsim,nh,ngpW) :: initdistsso

double precision, dimension(nbsim,ngpW,Jtot) :: distr,distrp
double precision, dimension(nbsim,nl,nmsim,nh,ngpW,Jtot) :: disto,distop

double precision, allocatable   :: GridHR(:)
double precision, allocatable   ::    VownStay(:,:,:,:,:,:,:)
!integer, allocatable   ::    foreOwnStayPol(:,:,:,:,:,:,:)
!integer, allocatable   ::    buyOwnStayPol(:,:,:,:,:,:,:)
!integer, allocatable   ::    sellOwnStayPol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    consOwnStayPol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    hconsOwnStayPol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    bOwnStayPol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    mOwnStayPol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    hOwnStayPol(:,:,:,:,:,:,:)

double precision, allocatable   ::    VownMove(:,:,:,:,:,:,:)
!integer, allocatable            ::    foreOwnMovePol(:,:,:,:,:,:,:)
!integer, allocatable            ::    buyOwnMovePol(:,:,:,:,:,:,:)
!integer, allocatable            ::    sellOwnMovePol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    consOwnMovePol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    hconsOwnMovePol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    bOwnMovePol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    mOwnMovePol(:,:,:,:,:,:,:)
!double precision, allocatable   ::    hOwnMovePol(:,:,:,:,:,:,:)

double precision, allocatable   ::    Vown(:,:,:,:,:,:,:)
double precision, allocatable   ::    EEVown(:,:,:,:,:,:,:)



integer, dimension(nb,ngpExo,ngpPh,Jwork):: hIBpol,lIBpol,mIBpol
integer, dimension(nb,ngpExo,ngpPh,Jret):: hIBpolRet,lIBpolRet,mIBpolRet
integer, dimension(nbs,ngpExo,ngpPh,Jwork):: hIBpolS,lIBpolS,mIBpolS
integer, dimension(nbs,ngpExo,ngpPh,Jret):: hIBpolRetS,lIBpolRetS,mIBpolRetS
double precision, allocatable, dimension(:,:,:,:)   ::  Wrent,consRpol,hconsRpol,bRpol,hRpol,mRpol
double precision, dimension(nbs,ngpExo,ngpPh,Jwork)   ::  WrentS,consRpolS,hconsRpolS,bRpolS,hRpolS,mRpolS
double precision, dimension(nb,ngpExo,ngpPh,Jwork):: Wbuy,consBpol,hconsBpol,bBpol,hBpol,mBpol,lBpol
double precision, dimension(nbs,ngpExo,ngpPh,Jwork):: WbuyS,consBpolS,hconsBpolS,bBpolS,hBpolS,mBpolS,lBpolS
double precision, dimension(nb,ngpExo,ngpPh,Jret)   ::  WrentRet,consRPolRet,hconsRPolRet,bRPolRet,hRPolRet,mRPolRet,WbuyRet,consBPolRet,hconsBPolRet,bBPolRet,hBPolRet,mBPolRet,lBPolRet
double precision, dimension(nbs,ngpExo,ngpPh,Jret)   ::  WrentRetS,consRPolRetS,hconsRPolRetS,bRPolRetS,hRPolRetS,mRPolRetS,WbuyRetS,consBPolRetS,hconsBPolRetS,bBPolRetS,hBPolRetS,mBPolRetS,lBPolRetS










!integer, dimension(nb,ngpExo,ngpPh,Jwork):: ex_hIBpol,ex_lIBpol,ex_mIBpol
!integer, dimension(nb,ngpExo,ngpPh,Jret):: ex_hIBpolRet,ex_lIBpolRet,ex_mIBpolRet
!integer, dimension(nbs,ngpExo,ngpPh,Jwork):: ex_hIBpolS,ex_lIBpolS,ex_mIBpolS
!integer, dimension(nbs,ngpExo,ngpPh,Jret):: ex_hIBpolRetS,ex_lIBpolRetS,ex_mIBpolRetS
!double precision, allocatable, dimension(:,:,:,:)   ::  ex_Wrent,ex_consRpol,ex_hconsRpol,ex_bRpol,ex_hRpol,ex_mRpol
!double precision, dimension(nbs,ngpExo,ngpPh,Jwork)   ::  ex_WrentS,ex_consRpolS,ex_hconsRpolS,ex_bRpolS,ex_hRpolS,ex_mRpolS
!double precision, dimension(nb,ngpExo,ngpPh,Jwork):: ex_Wbuy,ex_consBpol,ex_hconsBpol,ex_bBpol,ex_hBpol,ex_mBpol,ex_lBpol
!double precision, dimension(nbs,ngpExo,ngpPh,Jwork):: ex_WbuyS,ex_consBpolS,ex_hconsBpolS,ex_bBpolS,ex_hBpolS,ex_mBpolS,ex_lBpolS
!double precision, dimension(nb,ngpExo,ngpPh,Jret)   ::  ex_WrentRet,ex_consRPolRet,ex_hconsRPolRet,ex_bRPolRet,ex_hRPolRet,ex_mRPolRet,ex_WbuyRet,ex_consBPolRet,ex_hconsBPolRet,ex_bBPolRet,ex_hBPolRet,ex_mBPolRet,ex_lBPolRet
!double precision, dimension(nbs,ngpExo,ngpPh,Jret)   ::  ex_WrentRetS,ex_consRPolRetS,ex_hconsRPolRetS,ex_bRPolRetS,ex_hRPolRetS,ex_mRPolRetS,ex_WbuyRetS,ex_consBPolRetS,ex_hconsBPolRetS,ex_bBPolRetS,ex_hBPolRetS,ex_mBPolRetS,ex_lBPolRetS




!integer, dimension(nb,ngpExo,ngpPh,Jwork):: base_hIBpol,base_lIBpol,base_mIBpol
!integer, dimension(nb,ngpExo,ngpPh,Jret):: base_hIBpolRet,base_lIBpolRet,base_mIBpolRet
!integer, dimension(nbs,ngpExo,ngpPh,Jwork):: base_hIBpolS,base_lIBpolS,base_mIBpolS
!integer, dimension(nbs,ngpExo,ngpPh,Jret):: base_hIBpolRetS,base_lIBpolRetS,base_mIBpolRetS
!double precision, allocatable, dimension(:,:,:,:)   ::  base_Wrent,base_consRpol,base_hconsRpol,base_bRpol,base_hRpol,base_mRpol
!double precision, dimension(nbs,ngpExo,ngpPh,Jwork)   ::  base_WrentS,base_consRpolS,base_hconsRpolS,base_bRpolS,base_hRpolS,base_mRpolS
!double precision, dimension(nb,ngpExo,ngpPh,Jwork):: base_Wbuy,base_consBpol,base_hconsBpol,base_bBpol,base_hBpol,base_mBpol,base_lBpol
!double precision, dimension(nbs,ngpExo,ngpPh,Jwork):: base_WbuyS,base_consBpolS,base_hconsBpolS,base_bBpolS,base_hBpolS,base_mBpolS,base_lBpolS
!double precision, dimension(nb,ngpExo,ngpPh,Jret)   ::  base_WrentRet,base_consRPolRet,base_hconsRPolRet,base_bRPolRet,base_hRPolRet,base_mRPolRet,base_WbuyRet,base_consBPolRet,base_hconsBPolRet,base_bBPolRet,base_hBPolRet,base_mBPolRet,base_lBPolRet
!double precision, dimension(nbs,ngpExo,ngpPh,Jret)   ::  base_WrentRetS,base_consRPolRetS,base_hconsRPolRetS,base_bRPolRetS,base_hRPolRetS,base_mRPolRetS,base_WbuyRetS,base_consBPolRetS,base_hconsBPolRetS,base_bBPolRetS,base_hBPolRetS,base_mBPolRetS,base_lBPolRetS





integer, allocatable, dimension(:,:,:,:,:,:,:)::mINpol,lINpol,lIPpol
integer, allocatable, dimension(:,:,:,:,:,:,:)::mINpolRet,lINpolRet,lIPpolRet

double precision, allocatable, dimension(:,:,:,:,:,:,:):: Wsell,buySpol,consSpol,hconsSpol,bSpol,hSpol,mSpol
double precision, allocatable, dimension(:,:,:,:,:,:,:):: Wfore,consFpol,hconsFpol,bFpol,hFpol,mFpol
double precision, allocatable, dimension(:,:,:,:,:,:,:):: Wrefin,consNpol,hconsNpol,bNpol,hNpol,mNpol,lNpol
double precision, allocatable, dimension(:,:,:,:,:,:,:):: Wpay,consPpol,hconsPpol,bPpol,hPpol,mPpol,lPpol


double precision, allocatable, dimension(:,:,:,:,:,:,:):: WsellRet,buySPolRet,consSPolRet,hconsSPolRet,bSPolRet,hSPolRet,mSPolRet
double precision, allocatable, dimension(:,:,:,:,:,:,:):: WforeRet,consFPolRet,hconsFPolRet,bFPolRet,hFPolRet,mFPolRet
double precision, allocatable, dimension(:,:,:,:,:,:,:):: WrefinRet,consNPolRet,hconsNPolRet,bNPolRet,hNPolRet,mNPolRet,lNPolRet
double precision, allocatable, dimension(:,:,:,:,:,:,:):: WpayRet,consPPolRet,hconsPPolRet,bPPolRet,hPPolRet,mPPolRet,lPPolRet

!integer, allocatable, dimension(:,:,:,:,:,:,:)::ex_mINpol,ex_lINpol,ex_lIPpol
!integer, allocatable, dimension(:,:,:,:,:,:,:)::ex_mINpolRet,ex_lINpolRet,ex_lIPpolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_Wsell,ex_buySpol,ex_consSpol,ex_hconsSpol,ex_bSpol,ex_hSpol,ex_mSpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_Wfore,ex_consFpol,ex_hconsFpol,ex_bFpol,ex_hFpol,ex_mFpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_Wrefin,ex_consNpol,ex_hconsNpol,ex_bNpol,ex_hNpol,ex_mNpol,ex_lNpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_Wpay,ex_consPpol,ex_hconsPpol,ex_bPpol,ex_hPpol,ex_mPpol,ex_lPpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_WsellRet,ex_buySPolRet,ex_consSPolRet,ex_hconsSPolRet,ex_bSPolRet,ex_hSPolRet,ex_mSPolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_WforeRet,ex_consFPolRet,ex_hconsFPolRet,ex_bFPolRet,ex_hFPolRet,ex_mFPolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_WrefinRet,ex_consNPolRet,ex_hconsNPolRet,ex_bNPolRet,ex_hNPolRet,ex_mNPolRet,ex_lNPolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: ex_WpayRet,ex_consPPolRet,ex_hconsPPolRet,ex_bPPolRet,ex_hPPolRet,ex_mPPolRet,ex_lPPolRet

!integer, allocatable, dimension(:,:,:,:,:,:,:)::base_mINpol,base_lINpol,base_lIPpol
!integer, allocatable, dimension(:,:,:,:,:,:,:)::base_mINpolRet,base_lINpolRet,base_lIPpolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_Wsell,base_buySpol,base_consSpol,base_hconsSpol,base_bSpol,base_hSpol,base_mSpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_Wfore,base_consFpol,base_hconsFpol,base_bFpol,base_hFpol,base_mFpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_Wrefin,base_consNpol,base_hconsNpol,base_bNpol,base_hNpol,base_mNpol,base_lNpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_Wpay,base_consPpol,base_hconsPpol,base_bPpol,base_hPpol,base_mPpol,base_lPpol
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_WsellRet,base_buySPolRet,base_consSPolRet,base_hconsSPolRet,base_bSPolRet,base_hSPolRet,base_mSPolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_WforeRet,base_consFPolRet,base_hconsFPolRet,base_bFPolRet,base_hFPolRet,base_mFPolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_WrefinRet,base_consNPolRet,base_hconsNPolRet,base_bNPolRet,base_hNPolRet,base_mNPolRet,base_lNPolRet
!double precision, allocatable, dimension(:,:,:,:,:,:,:):: base_WpayRet,base_consPPolRet,base_hconsPPolRet,base_bPPolRet,base_hPPolRet,base_mPPolRet,base_lPPolRet

















double precision, dimension(nbsim,nl,nmsim,nh,ngpW)::bmatj,mmatj,hmatj,lmatj,ymatj
double precision, dimension(nbsim,nl,nmsim,nh,ngpW,Jtot)::bmat,mmat,hmat,lmat,ymat
double precision, dimension(nbsim,ngpW,Jtot)::bmatr,ymatr
double precision, dimension(nbsim,ngpW)::bmatjr,ymatjr

double precision, dimension(nb,ngpExo,ngpPh,Jwork)    :: Vrent,buyRentpol,consRentpol,hconsRentpol,bRentpol,hRentpol,mRentpol
double precision, dimension(nb,Jwork)    :: EVrent
double precision, dimension(nb,Jwork,ngpExo,ngpPh)    :: EEVrent
double precision, dimension(nb,nl,nm,nh,Jwork)        :: EVown
double precision, dimension(nb,nl,nm,nh,Jret)         :: EVownRet

!Retired Life Value and Policy Functions

double precision, dimension(nb,ngpExo,ngpPh,Jret)    :: VrentRet
double precision, dimension(nb,Jret)    :: EVrentRet
double precision, dimension(nb,Jret,ngpExo,ngpPh)    :: EEVrentRet


double precision, allocatable   ::    VownRet(:,:,:,:,:,:,:)
double precision, allocatable   ::    EEVownRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    foreOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    buyOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    sellOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    consOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    hconsOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    bOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    mOwnPolRet(:,:,:,:,:,:,:)
!double precision, allocatable   ::    hOwnPolRet(:,:,:,:,:,:,:)



!Mortgage Functions

double precision, allocatable   ::  qmret(:,:,:,:,:,:,:)
double precision, allocatable   ::  qm(:,:,:,:,:,:,:)
double precision, allocatable   ::  qmret_e(:,:,:,:,:,:,:)
double precision, allocatable   ::  qmret_e_exo(:,:,:,:,:,:,:)
double precision, allocatable   ::  qm_e(:,:,:,:,:,:,:)
double precision, allocatable   ::  qm_e_exo(:,:,:,:,:,:,:)
double precision, allocatable   ::  ql(:,:,:,:,:,:,:)
double precision, allocatable   ::  qlret(:,:,:,:,:,:,:)
double precision, allocatable   ::  bankgain_mort(:,:,:,:,:,:)
double precision, allocatable   ::  bankgain_loc(:,:,:,:,:,:)
double precision, allocatable   ::  bankgain_mort_move(:,:,:,:,:,:)
double precision, allocatable   ::  bankgain_loc_move(:,:,:,:,:,:)


!double precision,dimension(nsim,Jtot)   :: hsim,msim,bsim,conssim,hconssim,qmsim
!integer,dimension(nsim,Jtot)    :: buysim,ownsim,sellsim,foresim,thetasim,zysimI,refisim
!integer,dimension(nsim,Jtot+1)  :: hsimI,lsimI
integer,dimension(nsim)         :: psisimI
integer,dimension(nsim,Jtot)    :: zysimI,thetasim
integer,dimension(psinum,ngpzy)  :: Wind








!GLOBALS FOR GRIDS

double precision, allocatable              	:: fylevgrid(:)        !fixed effect component levels, earnings
double precision, allocatable              	:: fyslogrid(:)        !fixed effect component slopes, earnings

double precision, dimension(Jtot,ngpp)       	:: pgrid,holdpgrid        !pensions grid

double precision, dimension(ngpey)              		:: eygrid        !transitory component, earnings
double precision, dimension(Jwork,ngpzy)             :: zygrid        !permanent component, earnings


double precision, dimension(Jwork,ngpfy,ngpzy,ngpey,ngpAy) :: ygrid 		!earnings
double precision, dimension(Jwork,ngpfy,ngpzy,ngpAy) 		:: ypsgrid 		!persistent component of earnings


!GLOBALS FOR INDICATORS
integer, dimension(ngpW)			:: zWind,fWind
integer, dimension(ngpR)			:: pRind
integer, dimension(ngpp)			:: Rind
integer, dimension(ngpfy)					:: fysloind,fylevind
integer, dimension(ngpfylev,ngpfyslo)		:: fyind

! integer  :: UseEZValFn

!GLOBALS FOR DISTRIBUTIONS AND TRANSITION MAPRICES
double precision, allocatable		:: initliq(:)
double precision, allocatable		:: initill(:)
double precision, allocatable		:: initnw(:)
double precision, dimension(:,:), allocatable		:: initliqdist
double precision, dimension(:,:), allocatable		:: initnwdist
double precision, dimension(:,:), allocatable		:: initilldist
double precision, allocatable		    :: fylevdist(:)
double precision, allocatable		    :: fyslodist(:)

double precision, dimension(ngpey)		    :: eydist
double precision, dimension(Jwork,ngpzy,ngpzy)	:: zytrans
double precision, dimension(Jwork,ngpzy)			:: zydist

double precision, dimension(Jwork)		    :: kappay
double precision, dimension(Jwork)			:: Vzy


double precision, dimension(psinum)    :: psidist
integer                                 :: inithouse

!PARAMETER GLOBALS
double precision     :: Vfey,Vepsy,Vetay,Vzy0,rhoy,lambday,Vfem,Vepsm,Vetam,rhom,agem,age2m,targetUsualKY,Vphy,phvarygrowth,targetMeanIlliquid,targetMedianIlliquid,targetP75Illiquid,rholevslo
double precision		:: inflation,nomretliq,nomretill,nomborrow,qfree,qill,qborr,taxurcg,mturcg,nomretgov,qgov,taxbase,phaseout,phaseoutamt,servflow,ndweight
double precision     :: gamC,sigC,borrowlim,bondparam,ratematch,debtmatch,maxdeduction,maxdedret
double precision		:: ctax,liqtax,illtax
double precision		:: surprob(Jret),popsize(Jtot),popdist(Jtot)
integer		:: posinitwealth,sameinitwealth,sameborrowlim


double precision, dimension(:,:), allocatable	:: uerand,zyrand,eyrand,dierand,initliqrand,initillrand,thetarand
double precision     :: poprand(nsim)

!GLOBALS TO STORE SIMULATION RESULTS
double precision, dimension(nsim, Jwork)	    :: ysim,yavsim,yavsimforpen,ypssim,eysim,zysim,usim
!double precision, dimension(nsim, Jtot)	    :: csim,ctotsim,flowsim,ssim,xsim,psim,taxsim,Vsim,dVsim,adjsim,penrand,incsim,mpcsim,mpadjsim,lplsim,lpl3sim,lpl6sim,mpctotsim,aveadjcost
!double precision, dimension(nsim, Jtot+1)	:: asim,lsim


integer, dimension(nsim)	    	:: fysimI,fyslosimI,fylevsimI,fysimtransI
integer, dimension(nsim, Jtot)	    :: psimI
integer, dimension(nsim, Jtot+1)	:: dedsimI,mpdedsimI
integer, dimension(nsim, Jwork)	    :: eysimI


double precision :: yavall

double precision :: sshdemand
double precision :: ssph(ngpAgg)

double precision, dimension(Jwork,ngpfy)	        :: nbl		! natural borrowing limits (liquid assets)
double precision, dimension(Jwork,ngpfy,ngpzy)	    :: abl		! actual borrowing limits


double precision, dimension(ngpd)					:: dgrid		!deductions grid
double precision, dimension(ngpdret)					:: dgridret		!deductions grid

double precision :: ph_pers,ph_std,ay_std
integer          :: ph_nstd
integer          :: demmodel
double precision :: nwyperc(5),ltvperc(5),nwperc(5),hnwperc(5),liqperc(5),hnwshareperc(5)
!Lifecycle Stats Stuff

real(8), dimension(Jwork)           :: Eyav,Vloglabinc,Eunemp,Elabinc,Euiben
real(8), dimension(Jtot)            :: Econ,Etotcon,Eflow,Vlogtotcon,Vlogflow,Etax,Vlogcon,Vlogcono,Vlogconr,Eadj,Empc,Empadj,Enetlabinc,Eliqpl,CORRil,Ehcon,Ehouse,Emort,Emortpos,Elev,Eass,Eown,Efore,Esell,Epen,Einc,Vloginc,Vloghcon

real(8), dimension(Tsim)     :: pathEage,pathEinc,pathEcon,pathEass,pathEliq,pathEtax,pathEasspos,pathEloc,pathEhouse,pathEmort,pathEmortpos,pathEbeq,pathEbeqpos
real(8), dimension(Tsim)     :: pathEtranpos,pathEpen,pathElocpos,pathEhpos,pathEbuy,pathEmlpos,pathEhnw,pathEhcon
real(8), dimension(Tsim,Jtot)    :: agepathEage,agepathEinc,agepathEcon,agepathEsav,agepathEass,agepathEliq,agepathEtax,agepathEasspos,agepathEliqpos,agepathEdh,agepathEdp
real(8), dimension(Tsim,Jtot)    :: agepathEtranpos,agepathEpen,agepathEincown,agepathEincrent,agepathEnw,AgepathEnwown,agepathEnwrent,agepathEhnw,agepathErent,agepathEhnwshare
real(8), dimension(Tsim,Jtot)    :: agepathEconown,agepathEconrent,agepathEconinclow,agepathEconinchigh,agepathEconlowltv,agepathEconhighltv,agepathEhcon


real(8), dimension(Tsim)     :: pathEown,pathErefi,pathEsell,pathEfore,pathEforem,pathElev
real(8), dimension(Tsim,Jtot)     :: agepathEown,agepathErefi,agepathEsell,agepathEfore,agepathEforem,agepathElev,agepathEloc,agepathEhouse,agepathEmort,agepathEbuy,agepathEmortpos,agepathEbeq
real(8), dimension(Tsim,Jtot)     :: agepathElocpos,agepathEmlpos,agepathEhnwsim,agepathEdelhnwsim
double precision, dimension(Tsim)            :: phrand,aggrand


!THREADPRIVATE GLOBALS
!$OMP THREADPRIVATE(gij,giPh,giPhp,giPhh,giPhhp,giPhr,giPhrp,giW,giWp,giR,gliq,gmort,ghouse,gloc,gvals,ginds,gif,giz,ghcc,gih,gil,gim,giAgg,giHD,giAy,giAggp,gPh,EVown,EVownRet,EVRent,EVRentRet,giExo,giExop,giC,giZh,giRf,grf,grm,grl,gPr)



end module globals

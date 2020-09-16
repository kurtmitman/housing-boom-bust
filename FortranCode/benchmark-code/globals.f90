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
integer :: gij,giPh,giPhp,giW,giWp,giR,gif,giz,gizp,ghcc,gih,gil,gim,giAgg,giHD,giAy,giAggp,giCD,giExo,giExop,giC,giZh,girf
double precision :: gliq,gmort,ghouse,gloc,gPh,grf,grm,rf_base,grl,gPr
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
double precision    :: GridHR(nhr)
double precision    :: PhGrid(ngpPh)
double precision    :: PrGrid(ngpPh,ngpAgg)
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

double precision    :: aa0(ngpAgg,ngpAgg),aa1(ngpAgg,ngpAgg)
double precision    :: aa0p(ngpAgg,ngpAgg),aa1p(ngpAgg,ngpAgg)

double precision    :: PtoV1(ngpPh,ngpAgg,ngpAgg),PtoV2(ngpPh,ngpAgg,ngpAgg)
integer             :: PtoP1(ngpPh,ngpAgg,ngpAgg),PtoP2(ngpPh,ngpAgg,ngpAgg)
double precision :: EPhp(ngpPh,ngpAgg),Phpmat(ngpPh,ngpAgg,ngpAgg)
double precision,dimension(ngpPh)   ::XSHdem

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
double precision, allocatable   ::  qm_e(:,:,:,:,:,:,:)
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





end module globals

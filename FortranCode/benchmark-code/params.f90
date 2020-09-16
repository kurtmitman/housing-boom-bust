module params


implicit none


character(len=*), parameter     :: InputDir = "Input/"                  !to read input files


double precision,parameter::cmin=1.0d-8
integer,parameter   :: Benchmark = 0
integer,parameter   :: BiAnnual = 1
integer,parameter   :: Jwork = 22*(2-BiAnnual)
integer,parameter   :: Jret = 8*(2-BiAnnual)
integer,parameter   :: Jtot=Jret+Jwork
integer,parameter   :: Tsim = 5000+(1-Benchmark)*5000
integer,parameter   :: perfectcorr=0
double precision,parameter   :: gam=0.78d0
integer,parameter   :: oneguy=0
integer,parameter   :: ngpPh = 13
integer,parameter   :: nbneg = 7
integer,parameter   :: nb = 19+nbneg !20+nbneg
integer,parameter   :: nbs = 6
integer,parameter   :: nm = 22
integer,parameter   :: nl = 1 !ngpPh+8
integer,parameter   :: nh = 6 - 3*oneguy
integer,parameter   :: nhr =  3 - 2*oneguy
integer,parameter   :: msimscale = 3
integer,parameter   :: bsimscale = 3
integer,parameter   :: nmsim = msimscale*(nm-1)+1
integer,parameter   :: nbsim = bsimscale*(nb-1)+1
integer,parameter   :: bzerosim = (nbneg-1)*bsimscale+1
integer,parameter   :: ngptheta = 2
integer,parameter :: ngpC      = 2-Benchmark !2
integer,parameter   :: ngpZh = 1
integer,parameter   :: ngpHD = 1+2*(1-Benchmark) !3
integer,parameter   :: ngpAY = 2
integer,parameter   :: ngpRf = 1
integer,parameter   :: ngpNu = 1
integer,parameter   :: ngpAgg = ngpC*ngpAY*ngpZh*ngpHD*ngpRf*(1-perfectcorr)+perfectcorr*ngpAy
integer,parameter :: ngpPv      = 1              !price variation
integer,parameter   :: DoMJ = 0

double precision, parameter :: a0=0.259d0
double precision, parameter :: a1=-0.768d0
double precision, parameter :: a2=0.031d0
double precision, parameter :: a3=-1.0d0/0.768d0


integer,parameter   :: Display=0 !0
integer,parameter   :: readparameters=1

integer,parameter   :: nsimss=5000 !oneguy+50000+(1-oneguy)
integer,parameter   :: nsim=oneguy+(Jtot*nsimss)*(1-oneguy)
integer,parameter   :: nsimtrans=nsim
integer,parameter   :: psinum = 1 !2


integer,parameter  ::  WhichStochModel                  = 1     !to allow various calibrations

integer,parameter  ::  NoDeathInRetirement              = 1 !must be 1 for model with EZ pref
integer,parameter  ::  FlatEarningsProfile              = 0
integer,parameter  ::  NoInitialWealth                  = 0 !0

integer,parameter  ::  FixedPension                             =oneguy
integer,parameter  ::  LinearPension                    = 0
integer,parameter  ::  ConcavePension                   = 1-oneguy     !cant run without zy or fy risk

integer,parameter :: ngpey      = 1 !2              !transitory component earnings
integer,parameter :: ngpzy      = oneguy+5*(1-oneguy)+DoMJ !11 !15                        !permanent component earnings
integer,parameter :: ngpfylev   = 1 !5                                      !fixed effect earnings: levels
integer,parameter :: ngpfyslo   = 1 !5                              !fixed effect earnings: slopes
integer,parameter :: ngpfy = ngpfyslo*ngpfylev

integer,parameter :: ngpp       = ngpzy*ngpfy*ngppv                       !number of pension points

integer,parameter :: ngpW = ngpfy*ngpzy*ngppv         !points in state for workers loop, not including x or a or l
integer,parameter :: ngpR = ngpp                !points in state for retirees loop,  not including x or a or l

integer,parameter :: ngpd = 1
integer,parameter :: ngpdret = 1

double precision, parameter  ::  Numeraire                        = 0.5d0     !1 for annual av earns, 4 for quarterly av earns
double precision, parameter      :: DataAvAnnualEarns = 53000.0d0
double precision, parameter   :: meanpension             = 0.40d0
double precision, parameter   :: stdvpension              =  3000.0 /(DataAvAnnualEarns/Numeraire)
double precision, parameter   :: pencap           = 0.50d0 
double precision, parameter   :: fracmainearner = 0.84
double precision, parameter   :: ssbend1          = 2124.0*(1.d0+dble(BiAnnual)) /(DataAvAnnualEarns/Numeraire)
double precision, parameter   :: ssbend2          = 12808.0*(1.0d0+dble(BiAnnual)) /(DataAvAnnualEarns/Numeraire)

double precision, parameter   :: bmax = 30.0d0*Numeraire

double precision, parameter   :: rw = 0.01d0 !Borrowing wedge

integer,parameter   :: ngpExo  = ngpW*ngpAgg


end module params

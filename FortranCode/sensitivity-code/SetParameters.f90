SUBROUTINE SetParameters

USE Params
USE Globals
USE Procedures
USE Funcs

IMPLICIT NONE

double precision::VzyJwork,rhopow,blimfrac,r_real_free_annual,junk,omegatemp,thettemp,meankap
double precision::lb(0:3)
integer::rebc,ij,ib
character(len=100) ParamOutputDir
character(len=100) ParamOutputDirSims

ParamOutputDir = "/scratch/km2277/GETest/Results/"
ParamOutputDirSims = "/scratch/km2277/GETest/Results/sims/"
NoBankBelief = 0
OnlyBankBelief = 0
NoRentalBelief =0
beliefshock=1
HDX=0.10d0
HDY=0.10d0
HDZ=0.50d0
liquid=0.0d0
DoSims=0
DoRf=0
DoMqm=0
HELOCVal=1
payextra=4
AdjustCostModel=0
GeRent=0
mjscale=1.0d0
mjprob=0.04d0
Ar=1.0d0/0.045d0
hservices=0.01d0
Goldminevals=(/-0.3d0,-0.15d0,-0.05d0,0.0d0,0.05d0,0.15d0,0.30d0/)

adjval=0.07d0
!TARGET CAPITAL INCOME RATIO
MortDeduct=0.75d0
GSE=0
GSElim=0.80d0
rf=0.025d0
rm=0.03d0
rl=0.04d0
HELOCmax=0.95d0
LTVmax=1.0d0
bmin = -1.0d0
beqlev=2360.0d0/20.0d0
beqmin=273000.0d0/DataAvAnnualEarns
tautax=0.151d0
ltax=0.75d0
mortfc=2000.0d0/DataAvAnnualEarns
rent_elas=0.4d0
ph_pers=1.0d0
ph_std=0.075d0
ay_std=0.035d0

ph_nstd=3
epsprph=0.89d0
epsphy=0.0d0

alph=-6.66d0
nu=1.0d0/50001.0d0
omegas=0.0d0

alph=0.5d0
nu=0.86d0
inithouse=0

bet=0.95d0
xi=3.0d0
omega=1.0d0
phi=1.0d0
sig=2.0d0
!sig=8.0d0
thet=0.05d0

phi_h=0.075d0 !Baseline
!phi_h=1.d0 !No Rent
!phi_h = 0.0d0 !beta_h case
deltah=0.0148d0
alpha_h=0.375d0
HMRate=deltah+0.01d0
ALLOCATE(initliq(ngpzy+ngpfylev-1))
ALLOCATE(initill(ngpzy+ngpfylev-1))
ALLOCATE(initnw(ngpzy+ngpfylev-1))
ALLOCATE(initliqdist(ngpzy+ngpfylev-1,2))
ALLOCATE(initilldist(ngpzy+ngpfylev-1,2))
ALLOCATE(initnwdist(ngpzy+ngpfylev-1,2))

popsize(1:Jtot)=1.0d0
popdist=popsize/sum(popsize)

!EARNINGS PROCESS
SELECT CASE (WhichStochModel)

        CASE(-1) !Uemp only
                Vepsy   = 0.0d0
                Vfey    = 0.0d0 !var fix effect
                Vzy0    = 0.0d0 !initial var persistent
                Vetay   = 0.0d0 !0.0 !var pers shocks
                rhoy    = 0.0d0 !0.99d0 !0.99
                lambday = 0.0   !probability of switch at quarterly level: only active if StaySwitchModelForzy=1
                rholevslo = 0.0d0
                                Vphy    = 0.0d0

        CASE(0)
                Vepsy   = 0.0
                Vfey    = 0.2723 !var fix effect
                Vzy0    = 0.0d0 !initial var persistent
                Vetay   = 0.0d0 !0.0 !var pers shocks
                rhoy    = 0.999d0 !0.99d0 !0.99
                lambday = 0.0   !probability of switch at quarterly level: only active if StaySwitchModelForzy=1
                rholevslo = 0.0d0
                Vphy    = 0.00002199574


        CASE(1) !AR only
                Vepsy   = 0.0d0 !(0.005d0)**2
                Vfey    = 0.0
                Vzy0    = 0.18d0
                rhoy    = (0.97d0)**(1.0d0+dble(BiAnnual))
                lambday = 1.0d0/1.0d0   !probability of switch at quarterly level: only active if StaySwitchModelForzy=1
                IF( rhoy .LT. 1.0d0 ) THEN
                        rhopow  = rhoy**(2.0d0*(real(Jwork)-1.0d0))
                        Vetay   = (Jwork*4.0d0*0.003d0*(1.0d0+dble(BiAnnual))+(1.0d0-rhoy**(2.0d0*real(Jwork)))*Vzy0)*(1.0d0-rhoy**(2.0d0))/(1.0d0-rhopow)
                ELSE
                        Vetay   = (1.0d0+dble(BiAnnual))*4.0d0*0.003d0
                ENDIF

                rholevslo = 0.0d0
                Vphy    = 0.0d0

        CASE(2) !FE + AR(1)
                Vepsy   = 0.0
                Vfey    = 0.1105
                Vzy0    = 0.0 !initial var persistent
                Vetay   = 0.00959 !var pers shocks
                rhoy    = 0.99d0
                lambday = 0.0   !probability of switch at quarterly level: only active if StaySwitchModelForzy=1
        CASE(3) !FE + AR(1) + IID
                Vepsy   = 0.34
                Vfey    = 0.11
                Vzy0    = 0.0 !initial var persistent
                Vetay   = 0.01 !var pers shocks
                rhoy    = 0.9902
                lambday = 0.0   !probability of switch at quarterly level: only active if StaySwitchModelForzy=1


END SELECT

print*,'Vetay: ',Vetay


IF (FlatEarningsProfile==1) THEN
        kappay = 0.0
ELSE
        SELECT CASE (WhichStochModel)

                CASE(0)
                lb(0) = -2.06665738921957d0
                lb(1) = 0.00858498077162529d0
                lb(2) = -4.09994229331142e-005
                lb(3) = -1.14130361947752e-007


                CASE(1)
                lb(0) = 5.080779897862075d0
                lb(1) = 0.354212100578216d0
                lb(2) = -0.006568425066786
                lb(3) = 0.000039013352423


!age^3: 0.000039013352423
!age^2: -0.006568425066786
!age: 0.354212100578216
!constant:  5.080779897862075



        END SELECT
        meankap=0.0d0
        DO ij = 1,Jwork
                kappay(ij) = lb(0)
                DO ib = 1,3
                        kappay(ij) = kappay(ij) + ((dble(ij-1)*2.0d0+22.5d0)**ib)*lb(ib)
                END DO
                meankap=meankap+exp(kappay(ij))/dble(Jwork)
        END DO
        kappay = kappay-log(meankap)
END IF
aa0=log(0.5d0)*(0.1d0)
aa1=0.9d0



OPEN(1, FILE = InputDir // "nchild.txt")
DO ij=1,Jtot*2-1
    Read(1,*) nchild(ij)
ENDDO
CLOSE(1)
OPEN(1, FILE = InputDir // "adults.txt")
DO ij=1,Jtot*2-1
    Read(1,*) adults(ij)
ENDDO
CLOSE(1)
OPEN(1, FILE = InputDir // "equiv.txt")
DO ij=1,Jtot*2-1
    Read(1,*) nchild(ij)
ENDDO
CLOSE(1)
!adults=2.0d0
initliqdist=0.0d0
initnwdist=0.0d0
initnw=0.0d0
OPEN(1, FILE = InputDir // 'initwbroad.txt')
DO ij=1,ngpzy-DoMJ
   READ(1,*) initliqdist(ij,1),initliqdist(ij,2),initliq(ij),initilldist(ij,1),initilldist(ij,2),initill(ij),initnwdist(ij,1),initnwdist(ij,2),initnw(ij)
END DO
CLOSE(1)

initliq=initliq/(DataAvAnnualEarns/(Numeraire))
initill=initill/(DataAvAnnualEarns/(Numeraire))
initnw=initnw/(DataAvAnnualEarns/(Numeraire))



phi=1.0d0
DO ij=1,Jwork
   phi(ij)=nchild(1+2*(ij-1))+1.0d0
ENDDO

if(NoRent .eq. 0) then
OPEN(1, FILE = InputDir // "nhr.txt")
Read(1,*) nhr
CLOSE(1)
ALLOCATE(GridHR(nhr))
OPEN(1, FILE = InputDir // "nhrvals.txt")
DO ij=1,nhr
    Read(1,*) GridHR(ij)
ENDDO
CLOSE(1)
else
nhr = 1
ALLOCATE(GridHR(nhr))
GridHR(1) = 0.1d0
endif



!READ IN PARAMETERS FROM TEXT FILE
IF (ReadParameters==1) THEN
    OPEN(1, FILE = InputDir // trim(InputParamFile))
    READ(1,*) OutputDir
    READ(1,*) OutputDirSims
    READ(1,*) bet
    READ(1,*) xi
    READ(1,*) omegatemp
    READ(1,*) alph
    READ(1,*) nu
    READ(1,*) thettemp
    READ(1,*) rf_base
    READ(1,*) mortmarkup
    READ(1,*) helocmarkup
    READ(1,*) HELOCmax
    READ(1,*) LTVmax
    READ(1,*) beqlev
    READ(1,*) beqmin
    READ(1,*) mortfc
    READ(1,*) GeRent
    READ(1,*) rent_elas
    READ(1,*) HMRate
    READ(1,*) alpha_h
    READ(1,*) deltah
    READ(1,*) demmodel
    READ(1,*) omegas
    CLOSE(1)

    omega=omegatemp

    if(thettemp .gt. 0.0d0) then

       thet=thettemp
    endif

    mortfc = mortfc / (DataAvAnnualEarns/Numeraire)
    beqmin=beqmin / (DataAvAnnualEarns/Numeraire)
    if(demmodel .eq. 0) phi=1.0d0
    if(Benchmark .eq. 0) then
       OPEN(1, FILE = trim(OutputDir) // "a0.txt")
       DO ij=1,ngpAgg
          DO giAgg=1,ngpAgg
             Read(1,*) aa0(ij,giAgg)
          ENDDO
       ENDDO
       CLOSE(1)
       OPEN(1, FILE = trim(OutputDir) // "a1.txt")
       DO ij=1,ngpAgg
          DO giAgg=1,ngpAgg
             Read(1,*) aa1(ij,giAgg)
          ENDDO
       ENDDO
       CLOSE(1)

    endif

ELSE
   OutputDir=ParamOutputDir
   OutputDirSims=ParamOutputDirSims

END IF

bet_h=bet

if(GeRent .eq. 0) then
   zbar=exp((1.0d0-alpha_h)*(log(deltah*1.8d0*0.64d0))-log(0.55d0)-alpha_h*log(alpha_h))
else
   zbar=exp((1.0d0-alpha_h)*(log(deltah*(2.4d0*0.64d0+0.36d0*1.2d0)))-alpha_h*log(0.55d0)-alpha_h*log(alpha_h))
endif
adjust=0.25d0
kappa=(1.0d0/deltah)*(0.7d0**alpha_h)/(2.4d0*0.64d0+0.36d0*1.2d0)**(adjust)
print*,'Kappa: ',kappa

END SUBROUTINE SetParameters

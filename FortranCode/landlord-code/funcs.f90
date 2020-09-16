module funcs

use params
use globals

implicit none

contains

DOUBLE PRECISION FUNCTION FnVFRC(bsav)

USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)


cons=gliq-FnQb(bsav,0.0d0)*bsav

if(cons .LT. cmin) then
    FnVFRC = 1.d10+10.0d0*(cmin-cons)
else
    call basefun(GridB,nb,bsav,vals,inds)
    FnVFRC = -1.0d0*(urent_cont(cons,gij)+bet*(vals(1)*EVrent(inds(1),gij)+vals(2)*EVrent(inds(2),gij)))
endif

END FUNCTION FnVFRC

DOUBLE PRECISION FUNCTION FnVFRRetC(bsav)

USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)


cons=gliq-FnQb(bsav,0.0d0)*bsav

if(cons .LT. cmin) then
FnVFRRetC = 1.d10+10.0d0*(cmin-cons)

else
call basefun(GridB,nb,bsav,vals,inds)
FnVFRRetC = -1.0d0*(urent_cont(cons,gij)+bet*(vals(1)*EVrentRet(inds(1),gij)+vals(2)*EVrentRet(inds(2),gij)))
endif

END FUNCTION FnVFRRetC


DOUBLE PRECISION FUNCTION FnVFRRetJC(bsav)

USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)


cons=gliq-FnQb(bsav,0.0d0)*bsav

if(cons .LT. cmin) then
    FnVFRRetJC = 1.d10+10.0d0*(cmin-cons)

else
    call basefun(GridB,nb,bsav,vals,inds)
!    FnVFRRetJ = -1.0d0*(urent(cons,GridH(gih),gij)+ubequest(bsav))
    FnVFRRetJC = -1.0d0*(urent_cont(cons,gij)+vals(1)*ubequest(GridB(inds(1)))+vals(2)*ubequest(GridB(inds(2))))
endif

END FUNCTION FnVFRRetJC




! Housing Maintenance Costs
double precision function FnMaint(hh)

double precision, intent(in)    :: hh
     FnMaint=gPh*hh*(deltah+hmrate)

end function FnMaint

! Progressive Tax Function of Gouveia-Strauss
double precision function FnTax(ly)

double precision, intent(in)    :: ly

!    FnTax=a0*(ly-(ly**(a1)+a2)**(a3))
FnTax=ly-ltax*ly**(1.0d0-tautax)
!FnTax=0.25d0*ly-0.25d0


end function FnTax

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Progressive Tax Function of Gouveia-Strauss
! with mortgage interest deduction

double precision function FnTaxM(ly,mort,loc)

double precision, intent(in)    :: ly,mort,loc
double precision                :: lyp
double precision                :: intpay
    !Calculate how much of payment was interest
    intpay = (grm*mort+grl*loc)*MortDeduct
    lyp = ly-intpay
    lyp = max(lyp,0.0d0)
    FnTaxM=min(ly-ltax*ly**(1.0d0-tautax),lyp-ltax*lyp**(1.0d0-tautax))

!    FnTaxM=0.25d0*ly-0.25d0

end function FnTaxM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Calculate the mortgage payment based on balance and years remaining
double precision function FnPm(mort,age)

double precision, intent(in)    :: mort
integer, intent(in)             :: age
integer                         :: payleft

!    payleft = Jtot-age+1
!    payleft = 1
    if(ShortTerm .eq. 0) then
      payleft = Jtot-age+1+payextra
      FnPm = mort*grm*((1.0d0+grm)**payleft)/((1.0d0+grm)**payleft - 1.0d0)
    else
      FnPm = mort*(1.0d0+grm)
    endif


end function FnPm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function ubequest(c)

double precision,intent(in) :: c

         ubequest=beqlev*((c+beqmin)**(1.0d0-sig))/(1.0d0-sig)

end function ubequest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function urent_cont(ctilde,age)

double precision,intent(in) :: ctilde
integer,intent(in)          :: age
double precision            :: hh,nunu,h,c




nunu=NuGrid(giAgg)
c=ctilde/(1.0d0+gPr*(gPr*nunu/(1.0d0-nunu))**(1.0d0/(alph-1.0d0)))
h=c*gPr*(gPr*nunu/(1.0d0-nunu))**(1.0d0/(alph-1.0d0))
if(h .gt. GridHR(1)) then
   h=GridHR(1)
   c=ctilde-gPr*h
endif


    urent_cont=(phi(age)**(sig-1.0d0)*(nunu*(c**alph)+(1.0d0-nunu)*(h**alph))**((1.0d0-sig)/alph)-1.0d0)/    (1.0d0-sig)


!    urent=((phi(age)**(sig-1.0d0))*((c**nu)*((hh)**(1.0d0-nu)))**(1.0d0-sig)-1.0d0)/    (1.0d0-sig)

end function urent_cont

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function urent(c,h,age)

double precision,intent(in) :: c,h
integer,intent(in)          :: age
double precision            :: hh,nunu,Pr,uChi,uPhi

! nunu=NuGrid(giAgg)
!
!
!
!     urent=(phi(age)**(sig-1.0d0)*(nunu*(c**alph)+(1.0d0-nunu)*(h**alph))**((1.0d0-sig)/alph)-1.0d0)/    (1.0d0-sig)

nunu=NuGrid(giAgg)
Pr=Prgrid(giPh)
uChi = 1.0d0/(1.0d0+(Pr**(alph/(1.0d0-alph)))*((nunu/(1.0d0-nunu))**(1.0d0/(1.0d0-alph))))
uPhi = ((1.0d0-nunu)*Pr**(-alph)*uChi**(alph-1.0d0))**((1.0d0-sig)/alph)

! cu = (1.0d0-uChi)*c
! hu = uChi*c/Pr
    ! uown=(phi(age)**(sig-1.0d0)*(nunu*(cu**alph)+(1-nunu)*((hu)**alph))**((1.0d0-sig)/alph)-1.0d0)/    (1.0d0-sig)
    urent=(phi(age)**(sig-1.0d0)*uPhi*c**(1.0d0-sig)-1.0d0)/    (1.0d0-sig)


!    urent=((phi(age)**(sig-1.0d0))*((c**nunu)*((h)**(1.0d0-nunu)))**(1.0d0-sig)-1.0d0)/    (1.0d0-sig)

end function urent



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function uown(c,h,age)

double precision,intent(in) :: c,h
integer,intent(in)          :: age
double precision            :: hh,nunu,Pr,uPhi,uChi
double precision            :: cu,hu
!hh=(1.0d0+dble(BiAnnual))*h

!if(age .LE. Jwork) then
!   hh=h*hservices
!else
!   hh=h*hservices*omega
!endif

!hh=h*hservices*omega
nunu=NuGrid(giAgg)
Pr=Prgrid(giPh)
uChi = 1.0d0/(1.0d0+(Pr**(alph/(1.0d0-alph)))*((nunu/(1.0d0-nunu))**(1.0d0/(1.0d0-alph))))
uPhi = ((1.0d0-nunu)*Pr**(-alph)*uChi**(alph-1.0d0))**((1.0d0-sig)/alph)

! cu = (1.0d0-uChi)*c
! hu = uChi*c/Pr
    ! uown=(phi(age)**(sig-1.0d0)*(nunu*(cu**alph)+(1-nunu)*((hu)**alph))**((1.0d0-sig)/alph)-1.0d0)/    (1.0d0-sig)
    uown=(phi(age)**(sig-1.0d0)*uPhi*c**(1.0d0-sig)-1.0d0)/    (1.0d0-sig)+omega



!nu=GridDemand(giHD)

!    uown=((phi(age)**(sig-1.0d0))*((c**nunu)*((h)**(1.0d0-nunu)))**(1.0d0-sig)-1.0d0)/    (1.0d0-sig)

end function uown

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function ufore(c,h,age)

double precision,intent(in) :: c,h
integer,intent(in)          :: age
double precision	    :: hh

hh=omega*h*0.5d0/(dble(BiAnnual)+1.0d0)+(1.0d0-0.5d0/(dble(BiAnnual)+1.0d0))*GridHR(1)
hh=hh*hservices
!hh=(1.0d0+dble(BiAnnual))*hh

!    uown=(phi(age)**(sig-1.0d0)*(nu*(c**alph)+(1-nu)*((hh)**alph))**((1.0d0-sig)/alph)-1.0d0)/    (1.0d0-sig)

!nu=GridDemand(giHD)

    ufore=((phi(age)**(sig-1.0d0))*((c**nu)*((hh)**(1.0d0-nu)))**(1.0d0-sig)-1.0d0)/    (1.0d0-sig)

end function ufore




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function FnAdj(houseval)

double precision,intent(in) :: houseval

    FnAdj=AdjGrid(giAgg)*houseval

end function FnAdj

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function FnQb(bond,equity)

double precision,intent(in) :: bond,equity

    if(bond .GE. 0.0d0) then
       FnQb=1.0d0/(1.0d0+grf)
    elseif( (bond .GE. -HELOCmax*equity .and. HELOCVal .eq. 0) .OR. (bond .GE. -HELOCmax*gPh*ghouse .and. HELOCVal .eq. 1)) then
!    elseif(bond .GE. -HELOCmax*gPh*ghouse) then
       FnQb=1.0d0/(1.0d0+grl)
    else
       FnQb=0.0d0
    end if


end function FnQb


double precision FUNCTION FnSSBenefit(lmearns)

USE Params
USE Globals

IMPLICIT NONE

double precision, INTENT(IN) :: lmearns
double precision                         :: lindearns(2),lben(2)
INTEGER                         :: i

lindearns(1)    = fracmainearner*lmearns
lindearns(2)    = (1.0d0-fracmainearner)*lmearns

DO i =1,2
        IF (lindearns(i)<= ssbend1) THEN
                lben(i) = 0.9*lindearns(i)
        ELSE IF (lindearns(i)<= ssbend2) THEN
            lben(i) = 0.9*ssbend1 + 0.32*(lmearns - ssbend1)
        ELSE
            lben(i) = 0.9*ssbend1 + 0.32*(ssbend2-ssbend1) + 0.15*(lmearns - ssbend2)
        END IF
END DO

!FnSSBenefit = SUM(lben)*4.0d0
FnSSBenefit = SUM(lben)

END FUNCTION FnSSBenefit



DOUBLE PRECISION FUNCTION  FnGridPers(lx)

USE Params
USE Globals
USE Procedures

IMPLICIT NONE

double precision, INTENT(IN)         :: lx
double precision,DIMENSION(Jwork)    :: lwidth,lvar
double precision                     :: ltemp1,ltemp2,ltemp3,ltemp4,lvetacond
INTEGER             :: iz1,iz2,ij

lvetacond = Vetay/lambday       !variance of innovation conditional on a change

! get boundaries and fill in wijh equally spaced points
DO ij = 1,Jwork
    zygrid(ij,1)    = -lx*sqrt(Vzy(ij))
    zygrid(ij,ngpzy) = lx*sqrt(Vzy(ij))
    lwidth(ij) = (zygrid(ij,ngpzy)-zygrid(ij,1))/real(ngpzy-1)
    DO iz1 = 2, ngpzy-1
        zygrid(ij,iz1) = zygrid(ij,1) + lwidth(ij)*(iz1-1)
    END DO
END DO

! fill in transijion matrix using normal distribution
DO ij = 1,Jwork-1
    DO iz1 = 1,ngpzy
        CALL cumnor ((zygrid(ij+1,1)+0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), zytrans(ij,iz1,1), ltemp2)
                zytrans(ij,iz1,1) = lambday*zytrans(ij,iz1,1)
                IF(iz1==1) zytrans(ij,iz1,1) = zytrans(ij,iz1,1) + 1.0-lambday

            DO iz2 = 2,ngpzy-1
                CALL cumnor ((zygrid(ij+1,iz2)+0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp1, ltemp2)
                CALL cumnor ((zygrid(ij+1,iz2)-0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp3, ltemp4)
            zytrans(ij,iz1,iz2) = ltemp1 - ltemp3
                        zytrans(ij,iz1,iz2) = lambday*zytrans(ij,iz1,iz2)
                        IF(iz1==iz2) zytrans(ij,iz1,iz2) = zytrans(ij,iz1,iz2) + 1.0-lambday
        END DO

        CALL cumnor ((zygrid(ij+1,ngpzy)-0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp3,zytrans(ij,iz1,ngpzy))
                zytrans(ij,iz1,ngpzy) = lambday*zytrans(ij,iz1,ngpzy)
                IF(iz1==ngpzy) zytrans(ij,iz1,ngpzy) = zytrans(ij,iz1,ngpzy) + 1.0-lambday

        zytrans(ij,iz1,:) = zytrans(ij,iz1,:)/sum(zytrans(ij,iz1,:))
    END DO
END DO

zytrans(Jwork,:,:)=0.0d0
do iz1 = 1,ngpzy
    zytrans(Jwork,iz1,iz1)=1.0d0
enddo

!find distribution at first period
CALL cumnor( (zygrid(1,1)+0.5*lwidth(1))/sqrt(Vzy(1)), zydist(1,1), ltemp2)
DO iz1 = 2,ngpzy-1
    CALL cumnor ((zygrid(1,iz1)+0.5*lwidth(1))/sqrt(Vzy(1)), ltemp1, ltemp2)
    CALL cumnor ((zygrid(1,iz1)-0.5*lwidth(1))/sqrt(Vzy(1)), ltemp3, ltemp4)
    zydist(1,iz1) = ltemp1 - ltemp3
END DO
CALL cumnor ((zygrid(1,ngpzy)-0.5*lwidth(1))/sqrt(Vzy(1)), ltemp3, zydist(1,ngpzy))
zydist(1,:) = zydist(1,:)/sum(zydist(1,:))

!find uncondijional distributions
DO ij = 2,Jwork
    zydist(ij,:) = MATMUL(zydist(ij-1,:),zytrans(ij-1,:,:))
    zydist(ij,:) = zydist(ij,:) /sum(zydist(ij,:) )
END DO


!find variance
DO ij = 1,Jwork
    lvar(ij) = DOT_PRODUCT(zygrid(ij,:)**2,zydist(ij,:)) - DOT_PRODUCT(zygrid(ij,:),zydist(ij,:))**2
END DO

!moment
FnGridPers  = SUM((Vzy -lvar)**2)


END FUNCTION FnGridPers





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



DOUBLE PRECISION FUNCTION  FnGridPersMJ(lx)

USE Params
USE Globals
USE Procedures

IMPLICIT NONE

double precision, INTENT(IN)         :: lx
double precision,DIMENSION(Jwork)    :: lwidth,lvar
double precision                     :: ltemp1,ltemp2,ltemp3,ltemp4,lvetacond
INTEGER             :: iz1,iz2,ij

    lvetacond = Vetay       !variance of innovation conditional on a change

    ! get boundaries and fill in wijh equally spaced points
    DO ij = 1,Jwork
        zygrid(ij,1)    = -lx*sqrt(Vzy(ij))
        zygrid(ij,ngpzy-1) = lx*sqrt(Vzy(ij))
        zygrid(ij,ngpzy) = zygrid(ij,ngpzy-1)+mjscale
        lwidth(ij) = (zygrid(ij,ngpzy-1)-zygrid(ij,1))/real(ngpzy-2)
        DO iz1 = 2, ngpzy-2
            zygrid(ij,iz1) = zygrid(ij,1) + lwidth(ij)*(iz1-1)
        END DO
    END DO
    zytrans=0.0d0
    ! fill in transijion matrix using normal distribution
    DO ij = 1,Jwork-1
        DO iz1 = 1,ngpzy-1
            CALL cumnor ((zygrid(ij+1,1)+0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), zytrans(ij,iz1,1), ltemp2)
            zytrans(ij,iz1,1) = zytrans(ij,iz1,1)

            DO iz2 = 2,ngpzy-2
                CALL cumnor ((zygrid(ij+1,iz2)+0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp1, ltemp2)
                CALL cumnor ((zygrid(ij+1,iz2)-0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp3, ltemp4)
                zytrans(ij,iz1,iz2) = ltemp1 - ltemp3
                zytrans(ij,iz1,iz2) = zytrans(ij,iz1,iz2)
                IF(iz1==iz2) zytrans(ij,iz1,iz2) = zytrans(ij,iz1,iz2)
            END DO

            IF(iz1 .LT. ngpzy-1) then
                CALL cumnor ((zygrid(ij+1,ngpzy-1)-0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp3,zytrans(ij,iz1,ngpzy-1))
                zytrans(ij,iz1,ngpzy)=0.0d0
            ELSEIF(iz1 .EQ. ngpzy-1) then
                CALL cumnor ((zygrid(ij+1,ngpzy-1)-0.5*lwidth(ij+1)-rhoy*zygrid(ij,iz1))/sqrt(lvetacond), ltemp3,zytrans(ij,iz1,ngpzy-1))
                zytrans(ij,iz1,ngpzy-1)=zytrans(ij,iz1,ngpzy-1)-mjprob
                zytrans(ij,iz1,ngpzy)=mjprob
            endif
            zytrans(ij,iz1,:) = zytrans(ij,iz1,:)/sum(zytrans(ij,iz1,:))
        END DO
        zytrans(ij,ngpzy,ngpzy)=0.5d0
        zytrans(ij,ngpzy,ngpzy-1)=0.5d0

    END DO

    zytrans(Jwork,:,:)=0.0d0
    do iz1 = 1,ngpzy
        zytrans(Jwork,iz1,iz1)=1.0d0
    enddo

    !find distribution at first period
    CALL cumnor( (zygrid(1,1)+0.5*lwidth(1))/sqrt(Vzy(1)), zydist(1,1), ltemp2)
    DO iz1 = 2,ngpzy-2
        CALL cumnor ((zygrid(1,iz1)+0.5*lwidth(1))/sqrt(Vzy(1)), ltemp1, ltemp2)
        CALL cumnor ((zygrid(1,iz1)-0.5*lwidth(1))/sqrt(Vzy(1)), ltemp3, ltemp4)
        zydist(1,iz1) = ltemp1 - ltemp3
    END DO
    CALL cumnor ((zygrid(1,ngpzy-1)-0.5*lwidth(1))/sqrt(Vzy(1)), ltemp3, zydist(1,ngpzy-1))
    zydist(1,ngpzy)=0.0d0
    zydist(1,:) = zydist(1,:)/sum(zydist(1,:))

    !find uncondijional distributions
    DO ij = 2,Jwork
        zydist(ij,:) = MATMUL(zydist(ij-1,:),zytrans(ij-1,:,:))
        zydist(ij,:) = zydist(ij,:) /sum(zydist(ij,:) )
    END DO


    !find variance
    DO ij = 1,Jwork
        lvar(ij) = DOT_PRODUCT(zygrid(ij,:)**2,zydist(ij,:)) - DOT_PRODUCT(zygrid(ij,:),zydist(ij,:))**2
    END DO

    !moment
    FnGridPersMJ  = SUM((Vzy -lvar)**2)


END FUNCTION FnGridPersMJ






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine basefun(grid_x,npx,x,vals,inds)

implicit none
! this subroutine returns the values and the indices of the two basis
! functions that are positive on a given x in the grid_x

double precision,intent(in) :: x
integer , intent(in):: npx
double precision, intent(in) :: grid_x (npx)
double precision, intent(out) ::vals(2)
integer ,intent(out) ::inds(2)
integer :: i,ju,jl,jm

if(npx .LT. 2) then

    inds(1)=1
    inds(2)=1
    vals(1)=1.0d0
    vals(2)=0.0d0


else
    jl=1     !
    ju=npx   !

    do

        if (ju-jl<=1) exit
            jm=(ju+jl)/2
        if (x>=grid_x(jm)) then
            jl=jm
        else
            ju=jm
        endif

    end do

        i=jl+1
        vals(2)=( x-grid_x(i-1) )/(grid_x(i)-grid_x(i-1))
        !vals(1)=( grid_x(i)-x )/(grid_x(i)-grid_x(i-1))
        vals(2)=max(0.0d0,min(1.0d0,vals(2)))
        vals(1)=1.0d0-vals(2)
        inds(2)=i
        inds(1)=i-1
endif
end subroutine basefun




!-----------------------------------------------

SUBROUTINE BiLinInterp1 (nx,x,ny,y,f,xi,yi,fi)
!this does linear interpolation of f(x,y) at points (xi,yi)
!requires x and y to be sorted in ascending order
!extrapolates out of range
IMPLICIT NONE
INTEGER, INTENT(in)		:: nx,ny
double precision, INTENT(in)	:: x(:),y(:),f(:,:),xi,yi
double precision, INTENT(out)	:: fi
double precision	            :: xL,xH,yL,yH,fLL,fHH,fLH,fHL,dxdy
INTEGER					:: xlocL,ylocL

xlocL = MAXLOC(x,1,MASK=xi>x)
ylocL = MAXLOC(y,1,MASK=yi>y)

IF (xi<=x(1)) THEN
	xlocL = 1
END IF

IF (xLocL>=nx) THEN
	xLocL = nx-1
END IF

IF (yi<=y(1)) THEN
	ylocL = 1
END IF

IF (yLocL>=ny) THEN
	yLocL = ny-1
END IF

xL  = x(xlocL)
xH  = x(xlocL +1)
yL  = y(ylocL)
yH  = y(ylocL +1)
fLL = f(xlocL,ylocL)
fLH = f(xlocL,ylocL+1)
fHL = f(xlocL+1,ylocL)
fHH = f(xlocL+1,ylocL+1)

dxdy = (xH-xL)*(yH-yL)
fi = fLL*(xH-xi)*(yH-yi)/(dxdy) + fHL*(xi-xL)*(yH-yi)/(dxdy) + fLH*(xH-xi)*(yi-yL)/(dxdy) + fHH*(xi-xL)*(yi-yL)/(dxdy)


END SUBROUTINE BiLinInterp1

SUBROUTINE BiLinInterp2 (nx,x,ny,y,f,xi,yi,fi)
!this does linear interpolation of f(x,y) at points (xi,yi)
!requires x and y to be sorted in ascending order
!doesn't extrapolate out of range
IMPLICIT NONE
INTEGER, INTENT(in)		:: nx,ny
double precision, INTENT(in)	:: x(:),y(:),f(:,:),xi,yi
double precision, INTENT(out)	:: fi
double precision	            :: xL,xH,yL,yH,fLL,fHH,fLH,fHL,dxdy,xi2,yi2
INTEGER					:: xlocL,ylocL

xlocL = MAXLOC(x,1,MASK=xi>x)
ylocL = MAXLOC(y,1,MASK=yi>y)

xi2=xi
yi2=yi
IF (xi<=x(1)) THEN
   xlocL = 1
   xi2=x(1)
END IF

IF (xLocL>=nx) THEN
   xLocL = nx-1
   xi2=x(nx)
END IF

IF (yi<=y(1)) THEN
   ylocL = 1
   yi2=y(1)
END IF

IF (yLocL>=ny) THEN
   yLocL = ny-1
   yi2=y(ny)
END IF

xL  = x(xlocL)
xH  = x(xlocL +1)
yL  = y(ylocL)
yH  = y(ylocL +1)
fLL = f(xlocL,ylocL)
fLH = f(xlocL,ylocL+1)
fHL = f(xlocL+1,ylocL)
fHH = f(xlocL+1,ylocL+1)

dxdy = (xH-xL)*(yH-yL)
fi = fLL*(xH-xi2)*(yH-yi2)/(dxdy) + fHL*(xi2-xL)*(yH-yi2)/(dxdy) + fLH*(xH-xi2)*(yi2-yL)/(dxdy) + fHH*(xi2-xL)*(yi2-yL)/(dxdy)


END SUBROUTINE BiLinInterp2





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE TriLinInterp1 (nx,x,ny,y,nm,m,f,xi,yi,mi,fi)
!this does linear interpolation of f(x,y,m) at points (xi,yi,mi)
!requires x,y,m to be sorted in ascending order
!extrapolates out of range
IMPLICIT NONE
INTEGER, INTENT(in)             :: nx,ny,nm
DOUBLE PRECISION, INTENT(in)  :: x(:),y(:),m(:),f(:,:,:),xi,yi,mi
DOUBLE PRECISION, INTENT(out) :: fi
DOUBLE PRECISION                  :: mL,mH,fL,fH,mic
INTEGER                                 :: xlocL,ylocL,mlocL



mlocL = MAXLOC(m,1,MASK=mi>m)
IF (mi<=m(1)) THEN
mlocL = 1
END IF

IF (mLocL>=nm) THEN
mLocL = nm-1
END IF
call BiLinInterp1(nx,x,ny,y,f(:,:,mlocL),xi,yi,fL)
call BiLinInterp1(nx,x,ny,y,f(:,:,mlocL+1),xi,yi,fH)

mL = m(mlocL)
mH = m(mlocL+1)
mic=max(mL,min(mi,mH))
fi = fL + (mic-mL)*((fH-fL)/(mH-mL))


END SUBROUTINE TriLinInterp1



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



end module funcs

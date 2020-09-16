!
!   FnVFR.f90
!   
!
!   Created by Kurt Mitman on 14/10/14.
!

DOUBLE PRECISION FUNCTION FnVFR(bsav)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)


cons=gliq-FnQb(bsav,0.0d0)*bsav

if(cons .LT. cmin) then
    FnVFR = 1.d10+10.0d0*(cmin-cons)

else
    call basefun(GridB,nb,bsav,vals,inds)
    FnVFR = -1.0d0*(urent(cons,GridHR(gih),gij)+bet*(vals(1)*EVrent(inds(1),gij)+vals(2)*EVrent(inds(2),gij)))
endif

END FUNCTION FnVFR


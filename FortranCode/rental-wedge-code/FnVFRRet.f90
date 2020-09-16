!
!   FnVFRRet.f90
!   
!
!   Created by Kurt Mitman on 17/10/14.


DOUBLE PRECISION FUNCTION FnVFRRet(bsav)

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
FnVFRRet = 1.d10+10.0d0*(cmin-cons)

else
call basefun(GridB,nb,bsav,vals,inds)
FnVFRRet = -1.0d0*(urent(cons,GridHR(gih),gij)+bet*(vals(1)*EVrentRet(inds(1),gij)+vals(2)*EVrentRet(inds(2),gij)))
endif

END FUNCTION FnVFRRet


!
!   VnVFRRetJ.f90
!   
!
!
!   Created by Kurt Mitman on 14/10/14.
!

DOUBLE PRECISION FUNCTION FnVFRRetJ(bsav)

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
    FnVFRRetJ = 1.d10+10.0d0*(cmin-cons)

else
!    call basefun(GridB,nb,bsav,vals,inds)
    FnVFRRetJ = -1.0d0*(urent(cons,GridHR(gih),gij)+ubequest(bsav))
!    FnVFRRetJ = -1.0d0*(urent(cons,GridHR(gih),gij)+vals(1)*ubequest(GridB(inds(1)))+vals(2)*ubequest(GridB(inds(2))))
endif

END FUNCTION FnVFRRetJ


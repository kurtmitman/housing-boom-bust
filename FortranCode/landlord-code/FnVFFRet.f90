!
!   FnVFFRet.f90
!   
!
!   Created by Kurt Mitman on 10/10/14.
!
!

DOUBLE PRECISION FUNCTION FnVFFRet(bsav)

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

if(cons .GT. cmin) then


    call basefun(GridB,nb,bsav,vals,inds)

!    FnVFFRet = -1.0d0*(ufore(cons,ghouse,gij)-xi+bet*(vals(1)*EVrentRet(inds(1),gij)+vals(2)*EVrentRet(inds(2),gij)))
    FnVFFRet = -1.0d0*(urent(cons,GridHR(1),gij)-xi+bet*(vals(1)*EVrentRet(inds(1),gij)+vals(2)*EVrentRet(inds(2),gij)))

else

FnVFFRet = 1.0d10+10.0d0*(cmin-cons)

endif


END FUNCTION FnVFFRet

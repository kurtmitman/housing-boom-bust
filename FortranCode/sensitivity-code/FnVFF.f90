!
!   FnVFF.f90
!
!
!   Created by Kurt Mitman on 10/10/14.
!
!

DOUBLE PRECISION FUNCTION FnVFF(bsav)

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

    FnVFF = -1.0d0*(urent(cons,GridHR(1),gij)-xi+bet*(vals(1)*EVrent(inds(1),gij)+vals(2)*EVrent(inds(2),gij)))

else

   FnVFF = 1.0d10+10.0d0*(cmin-cons)

endif


END FUNCTION FnVFF

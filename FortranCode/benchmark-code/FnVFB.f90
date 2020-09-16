!
!   FnVFB.f90
!   
!
!
!   Created by Kurt Mitman on 10/10/14.
!
!

DOUBLE PRECISION FUNCTION FnVFB(bsav)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons,qmtemp,qltemp
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)



call basefun(GridB,nb,bsav,vals,inds)

call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qm(:,gil,gim,gih,giExo,:,gij+1),bsav,gPh,qmtemp)
call BiLinInterp1(nb,GridB,ngpPh,PhGrid,ql(:,gil,gim,gih,giExo,:,gij+1),bsav,gPh,qltemp)


cons=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(bsav,(gPh-gmort)*ghouse)*bsav


if(cons .GT. cmin) then

    FnVFB = -1.0d0*(uown(cons,ghouse,gij)+bet*(vals(1)*EVown(inds(1),gil,gim,gih,gij)+vals(2)*EVown(inds(2),gil,gim,gih,gij)))

else

    FnVFB = 1.0d10+10.0d0*(cmin-cons)

endif


END FUNCTION FnVFB

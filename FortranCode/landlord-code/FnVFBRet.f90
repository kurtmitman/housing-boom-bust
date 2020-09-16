!
!   FnVFBRet.f90
!
!
!   Created by Kurt Mitman on 17/10/14.

DOUBLE PRECISION FUNCTION FnVFBRet(bsav)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons,qltemp,qmtemp
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)



call basefun(GridB,nb,bsav,vals,inds)

! call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qmret(:,gil,gim,gih,giExo,:,gij+1),bsav,gPh,qmtemp)
! call BiLinInterp1(nb,GridB,ngpPh,PhGrid,qlret(:,gil,gim,gih,giExo,:,gij+1),bsav,gPh,qltemp)

qmtemp = sum(vals*qmret(:,gil,gim,gih,giExo,giPh,gij+1))
qltemp = sum(vals*qlret(:,gil,gim,gih,giExo,giPh,gij+1))

cons=gliq+(qltemp*gloc+qmtemp*gmort)*ghouse-FnQb(bsav,(gPh-gmort)*ghouse)*bsav


if(cons .GT. cmin) then

FnVFBRet = -1.0d0*(uown(cons,ghouse,gij)+bet*(vals(1)*EVownRet(inds(1),gil,gim,gih,gij)+vals(2)*EVownRet(inds(2),gil,gim,gih,gij)))

else

FnVFBRet = 1.0d10+10.0d0*(cmin-cons)

endif


END FUNCTION FnVFBRet

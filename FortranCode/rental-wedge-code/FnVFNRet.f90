!
!   FnVFNRet.f90
!
!
!   Created by Kurt Mitman on 10/10/14.
!
!

DOUBLE PRECISION FUNCTION FnVFNRet(bsav)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons,qltemp,qltemp2
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)



call basefun(GridB,nb,bsav,vals,inds)


cons=gliq-FnQb(bsav,gPh*ghouse-gmort)*bsav


if(cons .GT. cmin) then

FnVFNRet = -1.0d0*(uown(cons,ghouse,gij)+bet*(vals(1)*sum(gvals*EVownRet(inds(1),gil,ginds,gih,gij))+vals(2)*sum(gvals*EVownRet(inds(2),gil,ginds,gih,gij))))

else

FnVFNRet = 1.0d10+10.0d0*(cmin-cons)

endif


END FUNCTION FnVFNRet

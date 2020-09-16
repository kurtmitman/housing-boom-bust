!
!   FnVFV.f90
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
DOUBLE PRECISION             :: cons
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)



call basefun(GridB,nb,bsav,vals,inds)

cons=gliq+(vals(1)*(qm(inds(1),gil,gim,gih,giW,giAgg,gij+1)*gmort+ql(inds(1),gil,gim,gih,giW,giAgg,gij+1)*gloc)+(vals(2)*(qm(inds(2),gil,gim,gih,giW,giAgg,gij+1)*gmort+ql(inds(2),gil,gim,gih,giW,giAgg,gij+1)*gloc)))*ghouse-FnQb(bsav,0.0d0)*bsav


if(cons .GT. cmin) then

FnVFB = -1.0d0*(uown(cons,ghouse,gij)+bet*(vals(1)*EVown(inds(1),gil,gim,gih,giW,giAgg,gij)+vals(2)*EVown(inds(2),gil,gim,gih,giW,giAgg,gij)))

else

FnVFB = 1.0d10+10.0d0*(cmin-cons)

endif


END FUNCTION FnVFB

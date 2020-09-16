!
!   VnVFPRetJ.f90
!   
!
!
!
!
!
!   Created by Kurt Mitman on 14/10/14.
!

DOUBLE PRECISION FUNCTION FnVFPRetJ(bsav)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: bsav
DOUBLE PRECISION             :: cons,houseval
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)


cons=gliq-FnQb(bsav,gPh*ghouse-gmort)*bsav

if(cons .LT. cmin) then
    FnVFPRetJ = 1.d10+10.0d0*(cmin-cons)

else

    FnVFPRetJ = -1.0d0*(uown(cons,ghouse,gij)+ubequest(bsav+gPh*ghouse-gmort*(1.0d0+grm)-FnAdj(gPh*ghouse)))
!    call basefun(GridB,nb,bsav,vals,inds)
!    houseval=gPh*ghouse-FnAdj(gPh*ghouse)

!    FnVFPRetJ = -1.0d0*(uown(cons,ghouse,gij)+vals(1)*ubequest(GridB(inds(1))+houseval)+vals(2)*ubequest(GridB(inds(2))+houseval))

endif



END FUNCTION FnVFPRetJ


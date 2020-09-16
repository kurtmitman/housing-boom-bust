!
!   FnXSH.f90
!
!
!   Created by Kurt Mitman on 19/03/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

DOUBLE PRECISION FUNCTION FnXSdemand(pph)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: pph(2)
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)

CALL  BiLinInterp2(ngpPhh,GridPh,ngpPhr,GridPrPh,XSdemand,pph(1),pph(2),FnXSdemand)
!FnXSdemand = (pph(1)-0.50)**2+(pph(2)-0.10d0)**2

END FUNCTION FnXSdemand

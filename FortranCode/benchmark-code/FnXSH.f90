!
!   FnXSH.f90
!   
!
!   Created by Kurt Mitman on 19/03/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

DOUBLE PRECISION FUNCTION FnXSH(pph)

USE Params
USE Globals
USE funcs
USE Procedures

IMPLICIT NONE

DOUBLE PRECISION, INTENT(IN) :: pph
DOUBLE PRECISION             :: vals(2)
INTEGER                      :: inds(2)

call basefun(PhGrid,ngpPh,pph,vals,inds)

FnXSH=sum(vals*XSHdem(inds))

!FnXSH=ispline(pph,PhGrid,XSHdem,scoef1,scoef2,scoef3,ngpPh)

END FUNCTION FnXSH

!
!   FnXSH.f90
!
!
!   Created by Kurt Mitman on 19/03/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

SUBROUTINE DFOVEC(N, MV, X, FVEC)
!     SUBROUTINE dfovec(n, mv, x, v_err) must be provided by the user.
!     It must provide the values of the vector function v_err(x) : R^n to R^{mv}
!     at the variables X(1),X(2),...,X(N), which are generated automatically in
!     a way that satisfies the bounds given in XL and XU.

!     Min  F(x) := Sum_{i=1}^{mv}  v_err_i(x)^2, s.t. xl <= x <= xu, x \in R^n,
!     where v_err(x) : R^n \to R^{mv} is a vector function.
  USE Params
  USE Globals
  USE funcs
  USE Procedures

  IMPLICIT NONE
      INTEGER, INTENT(IN) :: n,mv
      REAL(8), DIMENSION(n), INTENT(IN)   :: x
      REAL(8), DIMENSION(mv), INTENT(OUT) :: FVEC


CALL  BiLinInterp1(ngpPhh,GridPh,ngpPhr,GridPrPh,xsdemand1,x(1),x(2),FVEC(1))
CALL  BiLinInterp1(ngpPhh,GridPh,ngpPhr,GridPrPh,xsdemand2,x(1),x(2),FVEC(2))
!FnXSdemand = (pph(1)-0.50)**2+(pph(2)-0.10d0)**2

END SUBROUTINE DFOVEC

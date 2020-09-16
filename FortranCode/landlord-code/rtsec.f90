!
!   rtsec.f90
!   
!
!   Created by Kurt Mitman on 19/03/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

SUBROUTINE rtsec(func,lx1,lx2,facc)

IMPLICIT NONE

REAL(8), INTENT(IN) :: lx1,lx2,facc
REAL(8) :: lrtsec,x1,x2
INTEGER, PARAMETER :: MAXIT=30
INTEGER :: j
REAL(8) :: dx,f,fl,xl,ltemp
REAL(8), EXTERNAL       :: func

x1 = lx1
x2 = lx2

fl=func(x1)
if (abs(fl)<facc) return

f=func(x2)
if (abs(f)<facc) return


if (abs(fl) < abs(f)) then
lrtsec=x1
xl=x2
ltemp = fl
fl = f
f = ltemp
else
xl=x1
lrtsec=x2
end if

do j=1,MAXIT
dx=(xl-lrtsec)*f/(f-fl)
xl=lrtsec
fl=f
lrtsec=lrtsec+dx
f=func(lrtsec)
if (abs(f)<facc) return
end do

write(*,*)('rtsec: exceed maximum iterations')

END SUBROUTINE rtsec
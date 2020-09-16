SUBROUTINE zbrak(func,x1,x2,n,xb1,xb2,fb1,fb2,nbb)
!returns the nb brackets in the first nb positions in xb1, xb2
! and the associated function values in fb1,fb2
! if no solutions returns function values at x1 and x2 in fb1(1),fb2(1)
USE Params
USE Globals

IMPLICIT NONE
INTEGER, INTENT(IN) :: n
INTEGER, INTENT(OUT) :: nbb
double precision, INTENT(IN) :: x1,x2
double precision, INTENT(OUT) :: xb1(n),xb2(n),fb1(n),fb2(n)
double precision, EXTERNAL	:: func

INTEGER :: i
double precision :: dx
double precision, DIMENSION(0:n) :: f,x
LOGICAL, DIMENSION(1:n) :: mask

dx=(x2-x1)/real(n)
x(0) = x1
do i = 1,n
	x(i) = x(i-1)+dx
end do
do i=0,n
	f(i)=func(x(i))
!	write(*,*) x(i),f(i),gass
end do

mask=f(1:n)*f(0:n-1) <= 0.0
nbb=count(mask)
if(nb>0) then
	xb1(1:nbb)=pack(x(0:n-1),mask)
	xb2(1:nbb)=pack(x(1:n),mask)
	fb1(1:nbb)=pack(f(0:n-1),mask)
	fb2(1:nbb)=pack(f(1:n),mask)
else
	fb1(1) = f(0)
	fb2(1) = f(n)
	xb1(1) = x1
	xb2(1) = x2
!	write(*,*) 'x',x
!	write(*,*) 'f',f
end if

END SUBROUTINE zbrak



MODULE Procedures

USE Params
USE Globals

IMPLICIT NONE
CONTAINS



!-----------------------------------------------

double precision FUNCTION EZAGG(lx,ly)
double precision, INTENT(in)	::  lx,ly                    

IF(sigC==1.0) THEN
	EZAGG = (lx**(1.0-bet)) * (ly**bet)
ELSE
	EZAGG = ((1.0-bet)*(lx**(1.0-sigC)) + bet*(ly**(1.0-sigC))) ** (1.0/(1.0-sigC))
END IF

END FUNCTION EZAGG


!-----------------------------------------------

double precision FUNCTION INVEZ(lx)
double precision, INTENT(in)	::  lx                    

IF(gamC==1.0) THEN
	INVEZ = exp((1-bet)*lx)
ELSE
	INVEZ = (lx*(1.0-gamC)) **(1.0/(1.0-gamC))
END IF

END FUNCTION INVEZ
!-----------------------------------------------

double precision FUNCTION EZ(lx)
double precision, INTENT(in)	::  lx                    

IF(gamC==1.0) THEN
	EZ = log(lx)/(1.0-bet)
ELSE
	EZ = (lx**(1.0-gamC))/ (1.0-gamC) 
END IF

END FUNCTION EZ

!-----------------------------------------------

SUBROUTINE ReverseVector(n,xin,xout)
IMPLICIT NONE
INTEGER, INTENT(in)		:: n
double precision, INTENT(in)		:: xin(n)
double precision, INTENT(out)	:: xout(n)
INTEGER					:: i
double precision					:: lxin(n),lxout(n)

lxin = xin
DO i = 1,n
	lxout(i) = lxin(n-i+1)
END DO
xout = lxout

END SUBROUTINE ReverseVector
!-----------------------------------------------

SUBROUTINE LinInterp (n,x,y,ni,xi,yi)
!this does linear interpolation of (x,y) at points xi
!requires x to be sorted in ascending order
!extrapolates out of range
IMPLICIT NONE
INTEGER, INTENT(in)		:: n,ni
double precision, INTENT(in)	:: x(:),y(:),xi(:)
double precision, INTENT(out)	:: yi(:)
double precision, DIMENSION(ni)	:: xL,xH,yL,yH
INTEGER					:: i,locL(ni)


DO i = 1,ni

	LocL(i) = MAXLOC(x,1,MASK=xi(i)>x)
	
	IF (xi(i)<=x(1)) THEN
		LocL(i) = 1
	END IF

	IF (LocL(i)>=n) THEN 
		LocL(i) = n-1
	END IF


	xL(i) = x(locL(i))
	xH(i) = x(locL(i)+1)
	yL(i) = y(locL(i))
	yH(i) = y(locL(i)+1)
	
	yi(i) = yL(i) + (xi(i)-xL(i))*((yH(i)-yL(i))/(xH(i)-xL(i)))

END DO


END SUBROUTINE LinInterp

!-----------------------------------------------

SUBROUTINE LinInterp1 (n,x,y,xi,yi)
!this does linear interpolation of (x,y) at points only point,xi
!requires x to be sorted in ascending order
!extrapolates out of range
IMPLICIT NONE
INTEGER, INTENT(in)		:: n
double precision, INTENT(in)	:: x(:),y(:),xi
double precision, INTENT(out)	:: yi
double precision	            :: xL,xH,yL,yH
INTEGER					:: locL

LocL = MAXLOC(x,1,MASK=xi>x)

IF (xi<=x(1)) THEN
	LocL = 1
END IF

IF (LocL>=n) THEN 
	LocL = n-1
END IF

xL  = x(locL)
xH  = x(locL +1)
yL  = y(locL)
yH  = y(locL +1)

yi  = yL  + (xi -xL )*((yH -yL )/(xH -xL ))

END SUBROUTINE LinInterp1

!----------------------------------------------
SUBROUTINE WriteVector(f,iL,iH,mat)

INTEGER, INTENT(IN)			:: f,iL,iH
double precision,INTENT(in)		:: mat(iL:iH)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') 1
lstring = '('//trim(lstring) // 'F16.6)'
DO i1=iL,iH
	WRITE(f,lstring) mat(i1)
END DO

CLOSE(f)

END SUBROUTINE WriteVector
!----------------------------------------------
SUBROUTINE WriteVectorInteger(f,iL,iH,mat)

INTEGER, INTENT(IN)			:: f,iL,iH
INTEGER, INTENT(in)		:: mat(iL:iH)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') 1
lstring = '('//trim(lstring) // 'I16)'
DO i1=iL,iH
	WRITE(f,lstring) mat(i1)
END DO

CLOSE(f)

END SUBROUTINE WriteVectorInteger
!----------------------------------------------
SUBROUTINE WriteMatrix(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
double precision,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'F16.6)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrix
!----------------------------------------------
SUBROUTINE WriteMatrix2(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
double precision,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1,i2

WRITE(UNIT=lstring, FMT='(I5)') 1
lstring = '('//trim(lstring) // 'F16.6)'
DO i1=1,n1
    do i2=1,n2

    WRITE(f,lstring) (mat(i1,i2))
    END DO
END DO
CLOSE(f)

END SUBROUTINE WriteMatrix2


!----------------------------------------------
SUBROUTINE WriteMatrix3(f,n1,n2,n3,mat)

INTEGER, INTENT(IN)			:: f,n1,n2,n3
double precision,INTENT(in)		:: mat(n1,n2,n3)
CHARACTER		::lstring*80
INTEGER			::i3,i2

WRITE(UNIT=lstring, FMT='(I5)') n1
lstring = '('//trim(lstring) // 'F16.6)'
DO i3=1,n3
    DO i2=1,n2
    	WRITE(f,lstring) (mat(:,i2,i3))
    END DO
END DO

CLOSE(f)

END SUBROUTINE WriteMatrix3
!----------------------------------------------
SUBROUTINE WriteMatrixLH(f,iL,iH,n2,mat)
!same as write matrix but allows the indices of the first dimension to be different from 1:n1
INTEGER, INTENT(IN)			:: f,n2,iL,iH
double precision,INTENT(in)		:: mat(iL:iH,1:n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'F16.6)'
DO i1=iL,iH
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixLH
!----------------------------------------------
SUBROUTINE WriteMatrixLong(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
double precision,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'F20.14)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixLong
!----------------------------------------------
SUBROUTINE WriteMatrixExpon(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
double precision,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'E16.8)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixExpon
!----------------------------------------------
SUBROUTINE WriteMatrixCSVExpon(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
double precision,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2-1
lstring = '('//trim(lstring) // '(E16.8,","),E16.8)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixCSVExpon
!----------------------------------------------
SUBROUTINE WriteMatrixInteger(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
INTEGER,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2
lstring = '('//trim(lstring) // 'I16)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixInteger
!----------------------------------------------
SUBROUTINE WriteMatrixCSV(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
double precision,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2-1
lstring = '('//trim(lstring) // '(F16.6,","),F16.6)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixCSV
!----------------------------------------------
SUBROUTINE WriteMatrixCSVInteger(f,n1,n2,mat)

INTEGER, INTENT(IN)			:: f,n1,n2
INTEGER,INTENT(in)		:: mat(n1,n2)
CHARACTER		::lstring*80
INTEGER			::i1

WRITE(UNIT=lstring, FMT='(I5)') n2-1
lstring = '('//trim(lstring) // '(I16,","),I16)'
DO i1=1,n1
	WRITE(f,lstring) (mat(i1,:))
END DO

CLOSE(f)

END SUBROUTINE WriteMatrixCSVInteger
!----------------------------------------------

SUBROUTINE RandomDiscrete(Nout,Xout,Nin,Pin)
!generates Nout random draws from the integers 1 to Nin
!using probabilities in Pin
IMPLICIT NONE
INTEGER, INTENT(in)			:: Nout,Nin
INTEGER,INTENT(out)		:: Xout(:)
double precision, INTENT(in)  :: Pin(:)

INTEGER			::i1,i2
double precision      :: lran(Nout)

!IF(sum(Pin) .ne. 1.0) write(*,*) 'error in RandomDiscrete: Pin doesnt sum to 1.0'

CALL RANDOM_NUMBER(lran)

Xout(:) = 0
DO i1 = 1,Nout
    IF ( lran(i1) .le. Pin(1) ) THEN
        Xout(i1) = 1
    ELSE
        i2 = 2
        DO WHILE (i2 .le. Nin)
            IF ( (lran(i1) .le. SUM(Pin(1:i2)) ).and. (lran(i1) > SUM(Pin(1:i2-1)) ) ) THEN
                Xout(i1) = i2
                i2 = Nin+1
            ELSE
                i2 = i2+1
            END IF
        END DO
    END IF
END DO

END SUBROUTINE RandomDiscrete

!--------------------------------------------------------------
SUBROUTINE RandomDiscrete1(Xout,Nin,Pin)
!generates Nout random draws from the integers 1 to Nin
!using probabilities in Pin
IMPLICIT NONE
INTEGER, INTENT(in)			:: Nin
INTEGER,INTENT(out)		:: Xout
double precision, INTENT(in)  ::Pin(:)

INTEGER			::i2
double precision      :: lran

!IF(sum(Pin) .ne. 1.0) write(*,*) 'error in RandomDiscrete: Pin doesnt sum to 1.0'

CALL RANDOM_NUMBER(lran)

Xout = 0
IF ( lran .le. Pin(1) ) THEN
    Xout = 1
ELSE
    i2 = 2
    DO WHILE (i2 .le. Nin)
        IF ( (lran .le. SUM(Pin(1:i2)) ).and. (lran > SUM(Pin(1:i2-1)) ) ) THEN
            Xout = i2
            i2 = Nin+1
        ELSE
            i2 = i2+1
        END IF
    END DO
END IF


END SUBROUTINE RandomDiscrete1

!----------------------------------------------
SUBROUTINE DiscreteDist(Nout,Xout,Nin,Pin,lran)
!takes in random numbers and a prob dist over the integers 1 to Nin
!and returns the corresponding values
!Created 11/22/09 - ERS

INTEGER,    INTENT(out) :: Xout(:)
INTEGER,    INTENT(in)  :: Nin,Nout
double precision,    INTENT(in)  :: Pin(:)
double precision, INTENT(in)  :: lran(:)
INTEGER			        :: i1,i2

Xout(:) = 0
DO i1 = 1,Nout
    IF ( lran(i1) .le. Pin(1) ) THEN
        Xout(i1) = 1
    ELSE
        i2 = 2
        DO WHILE (i2 .le. Nin)
            IF ( (lran(i1) .le. SUM(Pin(1:i2)) ).and. (lran(i1) > SUM(Pin(1:i2-1)) ) ) THEN
                Xout(i1) = i2
                i2 = Nin+1
            ELSE
                i2 = i2+1
            END IF
        END DO
    END IF
END DO

END SUBROUTINE DiscreteDist

!-----------------------------------------------
SUBROUTINE DiscreteDist1(Xout,Nin,Pin,lran)
!takes in a random number and a prob dist over the integers 1 to Nin
!and returns the corresponding value

INTEGER, INTENT(in)     :: Nin
INTEGER,INTENT(out)     :: Xout
double precision, INTENT(in)     :: Pin(:),lran
INTEGER                 :: i2

Xout = 0
IF ( lran .le. Pin(1) ) THEN
   Xout = 1
ELSE
   i2 = 2
   DO WHILE (i2 .le. Nin)
       IF ( (lran .le. SUM(Pin(1:i2)) ).and. (lran > SUM(Pin(1:i2-1)) ) ) THEN
           Xout = i2
           i2 = Nin+1
       ELSE
           i2 = i2+1
       END IF
   END DO
END IF

END SUBROUTINE DiscreteDist1

!-----------------------------------------------
!--------------------------------------------------------------
RECURSIVE SUBROUTINE quick_sort(list, order)

! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

IMPLICIT NONE
double precision, DIMENSION (:), INTENT(IN OUT)  :: list
INTEGER, DIMENSION (:), INTENT(OUT)  :: order

! Local variable
INTEGER :: i

DO i = 1, SIZE(list)
  order(i) = i
END DO

CALL quick_sort_1(1, SIZE(list))

CONTAINS

RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
double precision                :: reference, temp
INTEGER, PARAMETER  :: max_simple_sort_size = 6

IF (right_end < left_end + max_simple_sort_size) THEN
  ! Use interchange sort for small lists
  CALL interchange_sort(left_end, right_end)

ELSE
  ! Use partition ("quick") sort
  reference = list((left_end + right_end)/2)
  i = left_end - 1; j = right_end + 1

  DO
    ! Scan list from left end until element >= reference is found
    DO
      i = i + 1
      IF (list(i) >= reference) EXIT
    END DO
    ! Scan list from right end until element <= reference is found
    DO
      j = j - 1
      IF (list(j) <= reference) EXIT
    END DO


    IF (i < j) THEN
      ! Swap two out-of-order elements
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    ELSE IF (i == j) THEN
      i = i + 1
      EXIT
    ELSE
      EXIT
    END IF
  END DO

  IF (left_end < j) CALL quick_sort_1(left_end, j)
  IF (i < right_end) CALL quick_sort_1(i, right_end)
END IF

END SUBROUTINE quick_sort_1


SUBROUTINE interchange_sort(left_end, right_end)
IMPLICIT NONE
INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
REAL                :: temp

DO i = left_end, right_end - 1
  DO j = i+1, right_end
    IF (list(i) > list(j)) THEN
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    END IF
  END DO
END DO

END SUBROUTINE interchange_sort

END SUBROUTINE quick_sort
!-----------------------------------------------------------------------
SUBROUTINE Percentile(n,x,p,q)
IMPLICIT NONE
INTEGER, INTENT(in)		:: n
double precision, INTENT(in)	:: x(n),p
double precision, INTENT(out)	:: q
double precision				:: lx(n),li,lx1,lx2
INTEGER					:: lxord(n),i1,i2

lx = x
li = (n+1)*p
i1 = dint(li)
i2 = i1+1
CALL quick_sort(lx,lxord)
lx1 = lx(i1)
lx2 = lx(i2)

q  = lx1  + (li -real(i1) )*((lx2 -lx1 )/real(i2 -i1 ))

END SUBROUTINE Percentile

!-----------------------------------------------------------------------
SUBROUTINE Rouwenhorst(n,p,q,R)
IMPLICIT NONE
INTEGER, INTENT(in)		:: n
double precision, INTENT(in)	:: p,q
double precision, INTENT(out)	:: R(n,n)
double precision				:: X(n,n), Y1(n,n), Y2(n,n), Y3(n,n), Y4(n,n)
INTEGER					:: j,i

X(1,1) = p
X(1,2) = 1-p
X(2,1) = 1-q
X(2,2) = q

IF(n>2) THEN
DO j = 2,n-1
	Y1(:,:) = 0.0
	Y2(:,:) = 0.0
	Y3(:,:) = 0.0
	Y4(:,:) = 0.0
	Y1(1:j,1:j) = X(1:j,1:j)
	Y2(1:j,2:j+1) = X(1:j,1:j)
	Y3(2:j+1,1:j) = X(1:j,1:j)
	Y4(2:j+1,2:j+1) = X(1:j,1:j)
	X = p*Y1 + (1-p)*Y2 + (1-q)*Y3 + q*Y4
	DO i = 2,j
		X(i,:) = X(i,:)/2.0
	END DO
END DO

END IF
R = X

END SUBROUTINE Rouwenhorst



SUBROUTINE FindLeftCross (n1,x1,y1,n2,x2,y2,xout)
!for two vectors with y1(1)>y2(1), it finds first point x where y2(x)>=x2(x)
IMPLICIT NONE
INTEGER, INTENT(in)		:: n1,n2
double precision, INTENT(in)	:: x1(n1),y1(n1),x2(n2),y2(n2)
double precision, INTENT(out)	:: xout
double precision	            :: y2atx1(n1), y1atx2(n2),x(n1+n2),ydiff(n1+n2),ydiffsort(n1+n2),xL,xH,yL,yH
INTEGER					:: i,order(n1+n2)

!first check for no left cross point
! IF (y1(1)<=y2(1)) THEN
! 	write(*,*) 'err in FindLeftCross: y2>y1 at start'
! 	return
! END IF

!THERE MUST BE A MORE EFFICIENT WAY!
!interpolate y2 at each point in n1 to find where cross
CALL LinInterp (n2,x2,y2,n1,x1,y2atx1)
!interpolate y1 at each point in n2 to find where cross
CALL LinInterp (n1,x1,y1,n2,x2,y1atx2)

!combines the two vectors
DO i = 1,n1+n2
	x(1:n1) = x1
	ydiff(1:n1) = y1-y2atx1
	x(n1+1:n1+n2) = x2
	ydiff(n1+1:n1+n2) = y1atx2-y2
END DO

CALL quick_sort(x, order)
DO i =1,n1+n2
	ydiffsort(i) = ydiff(order(i))
END DO

DO i = 1,n1+n2
	IF (ydiffsort(i)<=0.0) GOTO 10
END DO
IF(i == n1+n2+1) THEN
	!write(*,*)'problem in FindLeftCross: no crossing'
	!for all reasonable cash levels adjusting is better, perhaps if assets are really high
	xout = 999.99	
	return
END IF

10 xH = ydiffsort(i)
yH = x(i)

IF(i>1) THEN
	xL = ydiffsort(i-1)
	yL = x(i-1)
ELSE
	write(*,*)'prob in FindLeftCross: ydiff<=0 at first point'
	!need to extrapolate
	xL = ydiffsort(1)
	yL = x(1)
	
	xH = ydiffsort(2)
	yH = x(2)
	!xout = -100.0
	
	!return
END IF

xout  = yL   -xL *((yH -yL )/(xH -xL ))

END SUBROUTINE FindLeftCross

!-----------------------------------------------

SUBROUTINE PowerSpacedGrid (n,k,low,high,y)

!gives a grid spaced between low and high based on the unit interval with a function x^(1/k)
!k = 1 is linear, k = 0 is L-shaped

IMPLICIT NONE

INTEGER, INTENT(in)	:: n
double precision, INTENT(in)	:: k,low,high
double precision, INTENT(out) :: y(:)
INTEGER				:: i
double precision				:: x(n),z(n)

IF(n<2) THEN
	write(*,*) 'n must be at least 2 to make grids'
	return
END IF

IF (n==2) THEN
	y(1) = low
	y(2) = high
	return
END IF

x(1) = 0.0
x(n) = 1.0
DO i = 2,n-1
	x(i) = (i-1)/real(n-1)
END DO

z = x**(1.0/k)

y = low + (high-low)*z


END SUBROUTINE PowerSpacedGrid





SUBROUTINE OLS(n,k,x,y,beta)

!x is n x k
!y is n x 1
!beta is k x 1
IMPLICIT NONE
INTEGER, INTENT(in)		:: n,k
DOUBLE PRECISION, INTENT(in)		:: x(n,k),y(n)
DOUBLE PRECISION, INTENT(out)	:: beta(k)
DOUBLE PRECISION					:: lxx(k,k),lxxinv(k,k)
INTEGER					:: ierr

lxx = matmul(transpose(x),x)
CALL InvertMatrix(lxx, lxxinv, k, ierr)
beta = matmul(lxxinv,matmul(transpose(x),y))

END SUBROUTINE OLS




   subroutine spline (x, y, b, c, d, n)
!======================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
!  comments ...
!  spline.f90 program is based on fortran version of program spline.f
!  the accompanying function fspline can be used for interpolation
!======================================================================
implicit none
integer n
double precision x(n), y(n), b(n), c(n), d(n)
integer i, j, gap
double precision h

gap = n-1
! check input
if ( n < 2 ) return
if ( n < 3 ) then
  b(1) = (y(2)-y(1))/(x(2)-x(1))   ! linear interpolation
  c(1) = 0.
  d(1) = 0.
  b(2) = b(1)
  c(2) = 0.
  d(2) = 0.
  return
end if
!
! step 1: preparation
!
d(1) = x(2) - x(1)
c(2) = (y(2) - y(1))/d(1)
do i = 2, gap
  d(i) = x(i+1) - x(i)
  b(i) = 2.0*(d(i-1) + d(i))
  c(i+1) = (y(i+1) - y(i))/d(i)
  c(i) = c(i+1) - c(i)
end do
!
! step 2: end conditions 
!
b(1) = -d(1)
b(n) = -d(n-1)
c(1) = 0.0
c(n) = 0.0
if(n /= 3) then
  c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
  c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
  c(1) = c(1)*d(1)**2/(x(4)-x(1))
  c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
end if
!
! step 3: forward elimination 
!
do i = 2, n
  h = d(i-1)/b(i-1)
  b(i) = b(i) - h*d(i-1)
  c(i) = c(i) - h*c(i-1)
end do
!
! step 4: back substitution
!
c(n) = c(n)/b(n)
do j = 1, gap
  i = n-j
  c(i) = (c(i) - d(i)*c(i+1))/b(i)
end do
!
! step 5: compute spline coefficients
!
b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
do i = 1, gap
  b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
  d(i) = (c(i+1) - c(i))/d(i)
  c(i) = 3.*c(i)
end do
c(n) = 3.0*c(n)
d(n) = d(n-1)
end subroutine spline

  function ispline(u, x, y, b, c, d, n)
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! x, y    = the arrays of given data points
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
!=======================================================================
implicit none
double precision ispline
integer n
double precision  u, x(n), y(n), b(n), c(n), d(n)
integer i, j, k
double precision dx

! if u is ouside the x() interval take a boundary value (left or right)
if(u <= x(1)) then
  ispline = y(1)
  return
end if
if(u >= x(n)) then
  ispline = y(n)
  return
end if

!*
!  binary search for for i, such that x(i) <= u <= x(i+1)
!*
i = 1
j = n+1
do while (j > i+1)
  k = (i+j)/2
  if(u < x(k)) then
    j=k
    else
    i=k
   end if
end do
!*
!  evaluate spline interpolation
!*
dx = u - x(i)
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline



END MODULE Procedures

subroutine simplex(start, n, EPSILON, scale, iprint,fun)
  use params
  implicit none

  external fun


    integer, intent (in) :: n, iprint
  double precision, intent (inout), dimension(0:n-1) :: start
  double precision, intent (in) :: EPSILON, scale
  double precision :: fun
! Define Constants
  integer, parameter :: MAX_IT = 1000
  double precision, parameter :: ALPHA_S=1.0
  double precision, parameter :: BETA_S=0.5
  double precision, parameter :: GAMMA_S=2.0



! ======================================================================
! Variable Definitions
!
! Integer vs = vertex with the smallest value
! Integer vh = vertex with next smallest value
! Integer vg = vertex with largest value
! Integer i,j,m,row
! Integer k = track the number of function evaluations
! Integer itr = track the number of iterations
! double precision v = holds vertices of simplex
! double precision pn,qn = values used to create initial simplex
! double precision f = value of function at each vertex
! double precision fr = value of function at reflection point
! double precision fe = value of function at expansion point
! double precision fc = value of function at contraction point
! double precision vr = reflection - coordinates
! double precision ve = expansion - coordinates
! double precision vc = contraction - coordinates
! double precision vm = centroid - coordinates
! double precision min
! double precision fsum,favg,s,cent
! double precision vtmp = temporary array passed to finddelta
! ======================================================================

  Integer :: vs,vh,vg
  Integer :: i,j,k,itr,m,row
  double precision, dimension(:,:), allocatable :: v
  double precision, dimension(:), allocatable  :: f
  double precision, dimension(:), allocatable :: vr
  double precision, dimension(:), allocatable :: ve
  double precision, dimension(:), allocatable :: vc
  double precision, dimension(:), allocatable :: vtmp
  double precision, dimension(:), allocatable :: vm
  double precision :: pn,qn
  double precision :: fr,fe,fc
  double precision :: min,fsum,favg,cent,s

  allocate (v(0:n,0:n-1))
  allocate (f(0:n))
  allocate (vr(0:n-1))
  allocate (ve(0:n-1))
  allocate (vc(0:n-1))
  allocate (vm(0:n-1))
  allocate (vtmp(0:n-1))

! create the initial simplex
! assume one of the vertices is 0.0

  pn = scale*(sqrt(n+1.)-1.+n)/(n*sqrt(2.))
  qn = scale*(sqrt(n+1.)-1.)/(n*sqrt(2.))

  DO i=0,n-1
    v(0,i) = start(i)
  END DO

  DO i=1,n
    DO j=0,n-1
      IF (i-1 == j) THEN
        v(i,j) = pn + start(j)
      ELSE
        v(i,j) = qn + start(j)
      END IF
    END DO
  END DO


! find the initial function values

  DO j=0,n
! put coordinates into single dimension array
! to pass it to finddelta
    DO m=0,n-1
      vtmp(m) = v(j,m)
    END DO
    f(j) = fun(vtmp)
  END DO

! Print out the initial simplex
! Print out the initial function values

  IF (iprint == 0) THEN
    Write(*,*) "Initial Values"
    Write(*,300) ((v(i,j),j=0,n-1),f(i),i=0,n)
  END IF

  k = n+1

! begin main loop of the minimization

DO itr=1,MAX_IT
! find the index of the largest value
  vg = 0
  DO j=0,n
    IF (f(j) .GT. f(vg)) THEN
      vg = j
    END IF
  END DO

! find the index of the smallest value
  vs = 0
  DO j=0,n
    If (f(j) .LT. f(vs)) Then
      vs = j
    END IF
  END DO

! find the index of the second largest value
  vh = vs
  Do j=0,n
    If ((f(j) .GT. f(vh)) .AND. (f(j) .LT. f(vg))) Then
      vh = j
    END IF
  END DO

! calculate the centroid
  DO j=0,n-1
  cent = 0.0
    DO m=0,n
      If (m .NE. vg) Then
        cent = cent + v(m,j)
      END IF
    END DO
    vm(j) = cent/n
  END DO

! reflect vg to new vertex vr
  DO j=0,n-1
    vr(j) = (1+ALPHA_S)*vm(j) - ALPHA_S*v(vg,j)
  END DO
  fr = fun(vr)
  k = k+1

  If ((fr .LE. f(vh)) .AND. (fr .GT. f(vs))) Then
    DO j=0,n-1
      v(vg,j) = vr(j)
    END DO
    f(vg) = fr
  END IF

! investigate a step further in this direction
  If (fr .LE. f(vs)) Then
    DO j=0,n-1
      ve(j) = GAMMA_S*vr(j) + (1-GAMMA_S)*vm(j)
    END DO
    fe = fun(ve)
    k = k+1

! by making fe < fr as opposed to fe < f(vs), Rosenbrocks function
! takes 62 iterations as opposed to 64.

    If (fe .LT. fr) Then
      DO j=0,n-1
        v(vg,j) = ve(j)
      END DO
      f(vg) = fe
    Else
      DO j=0,n-1
        v(vg,j) = vr(j)
      END DO
      f(vg) = fr
    END IF
  END IF

! check to see if a contraction is necessary
  If (fr .GT. f(vh)) Then
    DO j=0,n-1
      vc(j) = BETA_S*v(vg,j) + (1-BETA_S)*vm(j)
    END DO
    fc = fun(vc)
    k = k+1
    If (fc .LT. f(vg)) Then
      DO j=0,n-1
        v(vg,j) = vc(j)
      END DO
    f(vg) = fc

! at this point the contraction is not successful,
! we must halve the distance from vs to all the
! vertices of the simplex and then continue.
! 10/31/97 - modified C program to account for
! all vertices.

  Else
    DO row=0,n
      If (row .NE. vs) Then
        DO j=0,n-1
          v(row,j) = v(vs,j)+(v(row,j)-v(vs,j))/2.0
        END DO
      END IF
    END DO
    DO m=0,n-1
      vtmp(m) = v(vg,m)
    END DO
    f(vg) = fun(vtmp)
    k = k+1

    DO m=0,n-1
      vtmp(m) = v(vh,m)
    END DO
    f(vh) = fun(vtmp)
    k = k+1
    END IF
  END IF

! print out the value at each iteration
  IF (iprint == 0) THEN
    Write(*,*) "Iteration ",itr
    Write(*,300) ((v(i,j),j=0,n-1),f(i),i=0,n)
  END IF

! test for convergence
  fsum = 0.0
  DO j=0,n
    fsum = fsum + f(j)
  END DO
  favg = fsum/(n+1.)
  s = 0.0
  DO j=0,n
    s = s + ((f(j)-favg)**2.)/n
  END DO
  s = sqrt(s)
  If (s .LT. EPSILON) Then
    EXIT ! Nelder Mead has converged - exit main loop
  END IF
END DO
! end main loop of the minimization
! find the index of the smallest value

  vs = 0
  DO j=0,n
    If (f(j) .LT. f(vs)) Then
      vs = j
    END IF
  END DO

!  print out the minimum

  DO m=0,n-1
    vtmp(m) = v(vs,m)
  END DO
  start=vtmp
  min = fun(vtmp)
  k = k+1

250  FORMAT(A29,F7.4)
300  FORMAT(F11.6,F11.6,F11.6)

  return
  end subroutine simplex

SUBROUTINE SimulateEarnings

!****************************************************************************
!
!  PROGRAM: SimulateEarnings
!
!  PURPOSE:  Simulate Earnings of Agents
!
!  VERSION:
!           1.0, 11-June-2012
!           FROM CONSUMPTION PAPER
!           SHOULDN'T NEED TO CHANGE
!
!  LAST EDITED BY: Kurt, 11-June-2012
!****************************************************************************



USE Params
USE Globals
USE Funcs
USE random
USE Procedures

IMPLICIT NONE

INTEGER		:: iseed(2),isize

INTEGER		:: in,ij,iznext(2),iAy
double precision		:: lpynext(2),lEygreat,lEyless,lEy


!set random number seed
iseed(1) = 6970
iseed(2) = 6971
isize = 2
CALL RANDOM_SEED(size = isize)
CALL RANDOM_SEED(put = iseed)


iAy=ngpAy
!unemployment
ysim(:,:)=0.0d0
!earnings
CALL RandomDiscrete(nsim, psisimI(:), psinum, psidist)
CALL RandomDiscrete(nsim, zysimI(:,1), ngpzy, zydist(1,:))


thetasim(:,:)=2
fysimI=1
DO in = 1,nsim

	zysim(in,1) = zygrid(1,zysimI(in,1))
    ypssim(in,1) = ypsgrid(1,fysimI(in),zysimI(in,1),iAy)
	ysim(in,1) = ypssim(in,1) !*exp(eysim(in,1))
   	yavsim(in,1) = ysim(in,1)
   	yavsimforpen(in,1) = min(ysim(in,1),pencap)


    DO ij = 2,Jwork
        CALL RandomDiscrete1(zysimI(in,ij), ngpzy, zytrans(ij-1,zysimI(in,ij-1),:))
		CALL RandomDiscrete1(eysimI(in,ij), ngpey, eydist)
		eysim(in,ij) = eygrid(eysimI(in,ij))
		zysim(in,ij) = zygrid(ij,zysimI(in,ij))
	    ypssim(in,ij) = ypsgrid(ij,fysimI(in),zysimI(in,ij),iAy)
! 			        ysim(in,ij) = ygrid(ij,fysimI(in),zysimI(in,ij),eysimI(in,ij))
        ysim(in,ij) = ypssim(in,ij) !*exp(eysim(in,ij))
        yavsim(in,ij) = ((ij-1)*yavsim(in,ij-1) + ysim(in,ij))/real(ij)
        yavsimforpen(in,ij) = ((ij-1)*yavsimforpen(in,ij-1) + min(ysim(in,ij),pencap))/real(ij)


    END DO

END DO


giAY=ngpAy
gif=1

yavall = sum(zydist(:,:)*ypsgrid(:,gif,:,giAY))/dble(Jwork) / GridAY(giAY)	!annual earnings = 1



ygrid = ygrid/yavall
ypsgrid  = ypsgrid/yavall
ysim = ysim/yavall
yavsim = yavsim/yavall
ypssim = ypssim/yavall





END SUBROUTINE SimulateEarnings

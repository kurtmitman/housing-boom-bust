!
!   ComputeBPPCoef.f90
!
!
!   Created by Kurt Mitman on 11/08/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!
subroutine ComputeBPPCoef


USE Params
USE Globals
USE Funcs
USE random
USE Procedures

IMPLICIT NONE


INTEGER                                             :: iseed(2),isize,iAgg,it,Phinds(2),iindex,startage,endage,in

double precision,allocatable,dimension(:)           :: gy,dc,dy
double precision,allocatable,dimension(:,:)         :: gyj,dcj
double precision                                    :: Edc,Edy,Egy

startage=3
endage=Jwork-2

allocate(gy((tend-tstart+1)*(endage-startage+1)*nsim))
allocate(dc((tend-tstart+1)*(endage-startage+1)*nsim))
allocate(dy((tend-tstart+1)*(endage-startage+1)*nsim))


iindex=0

do in=1,nsim
    do it=tstart,tend
        if( (simtrans(it)%jsim(in) .GT. 2) .AND. (simtrans(it)%jsim(in) .LT. (Jwork-1))) then
        iindex=iindex+1
        gy(iindex)=rhoy*(log(simtrans(it)%ysim(in))-rhoy*log(simtrans(it-1)%ysim(in)))+rhoy**2*(log(simtrans(it-1)%ysim(in))-rhoy*log(simtrans(it-2)%ysim(in)))+(log(simtrans(it+1)%ysim(in))-rhoy*log(simtrans(it)%ysim(in)))
        dc(iindex)=log(simtrans(it)%csim(in))-log(simtrans(it-1)%csim(in))
        dy(iindex)=log(simtrans(it)%ysim(in))-rhoy*log(simtrans(it-1)%ysim(in))

        endif

    enddo
enddo


Edc = SUM(dc)/dble(iindex)
Edy = SUM(dy)/dble(iindex)
Egy = SUM(gy)/dble(iindex)



print*,Egy,Edc,Edy
print*,sum((gy-Egy)*(dc-Edc)),sum((gy-Egy)*(dy-Edy))

bppcoef = sum((gy-Egy)*(dc-Edc))/sum((gy-Egy)*(dy-Edy))

print*,bppcoef






end subroutine ComputeBPPCoef

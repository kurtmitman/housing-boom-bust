!
!   ComputeLOM.f90
!
!
!   Created by Kurt Mitman on 24/01/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!

subroutine ComputeLOM

    use params
    use globals
    use funcs
    use procedures

    implicit none

    double precision :: vals(2)
    integer          :: inds(2)
    double precision :: Phpexu(ngpPh,ngpAgg,ngpAgg),Ephpexu(ngpPh,ngpAgg)
    do giPh=1,ngpPh

       Phpmat(giPh,:,:)=exp(aa0+aa1*log(Phgrid(giPh)))
\       Phpexu(giPh,:,:)=exp(aa0p+aa1*log(Phgrid(giPh)))
    enddo

    do giAgg=1,ngpAgg
    do giPh=1,ngpPh
        EPhp(giph,giAgg)=0.0d0
        do giAggp=1,ngpAgg
            EPhp(giph,giAgg)=EPhp(giph,giAgg)+transMatAgg(giAgg,giAggp)*Phpmat(giPh,giAgg,giAggp)
            EPhpexu(giph,giAgg)=EPhpexu(giph,giAgg)+transMatAgg(giAgg,giAggp)*Phpexu(giPh,giAgg,giAggp)
            call basefun(PhGrid,ngpPh,Phpmat(giPh,giAgg,giAggp),vals,inds)
            PtoP1(giPh,giAgg,giAggp)=inds(1)
            PtoP2(giPh,giAgg,giAggp)=inds(2)
            PtoV1(giPh,giAgg,giAggp)=vals(1)
            PtoV2(giPh,giAgg,giAggp)=vals(2)
        enddo
        giRf=AtoR(giAgg)

        rf=RfGrid(giAgg)


        if(GeRent .eq. 0) then
           PrGrid(giPh,giAgg)=0.05d0*(((1.0d0+dble(BiAnnual)))*(PhGrid(giPh)))**rent_elas
        else

           !Rental rate depends on risk-free rate - PrRf Benchmark
           PrGrid(giPh,giAgg)=max(1.0d-4,phi_h*(1.0d0-(1.0d0/(1.0d0+RfGrid(giAgg)))*(1.0d0-deltah))+PhGrid(giPh)*(1.0d0+hmrate)-(1.0d0-deltah)*EPhp(giPh,giAgg)/(1.0d0+RfGrid(giAgg)))


        endif
        print*,PhGrid(giPh),PrGrid(giPh,giAgg),EPhp(giPh,giAgg)
    enddo
    enddo


















end subroutine ComputeLOM

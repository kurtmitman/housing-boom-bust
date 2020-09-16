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

    double precision :: vals(2),vals2(2)
    integer          :: inds(2),inds2(2)
    double precision :: Phpexu(ngpPh,ngpAgg,ngpAgg),Ephpexu(ngpPh,ngpAgg)
    double precision :: Prpexu(ngpPh,ngpAgg,ngpAgg)

    do giPh=1,ngpPh

       Phpmat(giPh,:,:)=exp(aa0+aa1*log(Phgrid(giPh)))
       Prpmat(giPh,:,:)=exp(bb0+bb1*log(Phgrid(giPh))+bb2*log(PrPhgrid(giPh)))

       Phpexu(giPh,:,:)=exp(aa0p+aa1*log(Phgrid(giPh)))
       Prpexu(giPh,:,:)=exp(bb0p+bb1p*log(Phgrid(giPh))+bb2p*log(PrPhgrid(giPh)))
    enddo

    PRINT*,'Max price forecast error - pre'
    PRINT*,maxval(abs(Phpexu-Phpmat))

    PRINT*,'Max rent forecast error - pre'
    PRINT*,maxval(abs(Prpexu-Prpmat))
    
    do giPh=1,ngpPh

       Phpmat(giPh,:,:)=exp(aa0+aa1*log(Phgrid(giPh)))
       Prpmat(giPh,:,:)=exp(bb0+bb1*log(Phgrid(giPh))+bb2*log(PrPhgrid(giPh)))

    enddo

    do giAgg=1,ngpAgg
    do giPh=1,ngpPh
      EPhp(giph,giAgg)=0.0d0
      EPrp(giph,giAgg)=0.0d0
        do giAggp=1,ngpAgg
            EPhp(giph,giAgg)=EPhp(giph,giAgg)+transMatAgg(giAgg,giAggp)*Phpmat(giPh,giAgg,giAggp)
            EPrp(giph,giAgg)=EPrp(giph,giAgg)+transMatAgg(giAgg,giAggp)*Prpmat(giPh,giAgg,giAggp)
            EPhpexu(giph,giAgg)=EPhpexu(giph,giAgg)+transMatAgg(giAgg,giAggp)*Phpexu(giPh,giAgg,giAggp)
            call basefun(GridPh,ngpPhh,Phpmat(giPh,giAgg,giAggp),vals,inds)
            call basefun(GridPrPh,ngpPhr,Prpmat(giPh,giAgg,giAggp),vals2,inds2)
            PtoP1(giPh,giAgg,giAggp)=(inds(1)-1)*ngpPhr+inds2(1)
            PtoP2(giPh,giAgg,giAggp)=(inds(2)-1)*ngpPhr+inds2(2)
            PtoV1(giPh,giAgg,giAggp)=vals(1)*vals2(1)
            PtoV2(giPh,giAgg,giAggp)=vals(2)*vals2(2)
            PrtoP1(giPh,giAgg,giAggp)=inds2(1)+(inds(2)-1)*ngpPhr
            PrtoP2(giPh,giAgg,giAggp)=inds2(2)+(inds(1)-1)*ngpPhr
            PrtoV1(giPh,giAgg,giAggp)=vals2(1)*vals(2)
            PrtoV2(giPh,giAgg,giAggp)=vals2(2)*vals(1)
        enddo
        giRf=AtoR(giAgg)

        !Changed 11/20/15 to make grid on Rf on Agg for comovement
        rf=RfGrid(giAgg)
        print*,PhGrid(giPh),EPhp(giPh,giAgg)
        print*,PrPhGrid(giPh),EPrp(giPh,giAgg)
    enddo
    enddo






end subroutine ComputeLOM

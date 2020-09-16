subroutine AggGrids

USE Params
USE Globals
USE Procedures
Use Funcs

IMPLICIT NONE

INTEGER                 :: ie,ij,iff,iz,ip,iR,iW,ifslo,iflev,izp,iAy,iAgg,iZh,iHD,iC,iAyp,iZhp,iHDp,iCp,iAggp,iRp
double precision         :: lwidth,lxa,lxb,lxc,lfa,lfb,lfc,lm,lfval,lylow,lEz,lSDz,lalpha,lbeta,lEyav,lSDyav,lEpen,slowidth,levwidth
double precision    :: cctemp,cctemp1,cctemp2
double precision    :: sigz,rhop,LTVBase
double precision    :: PhGridTemp(ngpPh)
integer             :: MarkovSwitch,ID


MarkovSwitch=0
if(perfectcorr .eq. 0) then

GridDemand(1)=0.83d0 !0.70d0
GridDemand(ngpNu)=0.86d0

GridRf(1)=rf_base-0.0202d0
GridRf(ngpRf)=rf_base


GridC(ngpC)=0.8d0
GridC(1)=LTVMax
PhgridTemp(ngpPh)  = 0.5d0
PhgridTemp(1)  = -PhgridTemp(ngpPh)
lwidth = (PhgridTemp(ngpPh) - PhgridTemp(1)) / real(ngpPh - 1)

do iz=2,(ngpPh-1)
    PhgridTemp(iz) = PhgridTemp(1) + lwidth * real(iz - 1);
enddo
Phgrid=1.0d0+PhGridTemp
Phgrid=pHgrid/(1.0d0+dble(BiAnnual))

if(ngpAy .gt. 1) then


   GridAy(ngpAy)  = -(real(ngpAy)-1.0d0)*ay_std
   GridAy(1)  = -GridAy(ngpAy)
   lwidth = (GridAy(ngpAy) - GridAy(1)) / real(ngpAy - 1)

   do iz=2,(ngpAy-1)
      GridAy(iz) = GridAy(1) + lwidth * real(iz - 1);
   enddo

   GridAy=GridAy+1.0d0

else
   transMatAy=1.0d0
   GridAy(ngpAy)=1.0d0


endif

if(ngpZh .gt. 1) then
   GridZh(ngpZh)  = sqrt(real(ngpZh)-1.0d0)*ph_std
   GridZh(1)  = -GridZh(ngpZh)
   lwidth = (GridZh(ngpZh) - GridZh(1)) / real(ngpZh - 1)

   do iz=2,(ngpZh-1)
      GridZh(iz) = GridZh(1) + lwidth * real(iz - 1);
   enddo


   do iz=1, ngpZh
      do izp=1, ngpZh
         if(izp .eq. 1) then

            call cumnor((GridZh(1) - ph_pers * GridZh(iz) + lwidth / 2.0d0) / ph_std,transMatZh(iz,izp),cctemp)
         elseif( izp .eq. ngpZh) then
            CALL cumnor((GridZh(ngpZh) - ph_pers * GridZh(iz) - lwidth / 2.0d0) / ph_std,cctemp,transMatZh(iz,izp))
         else

            call cumnor((GridZh(izp) - ph_pers * GridZh(iz) + lwidth / 2.0d0) / ph_std,cctemp1,cctemp)
            call cumnor((GridZh(izp) - ph_pers * GridZh(iz) - lwidth / 2.0d0) / ph_std,cctemp2,cctemp)

            transMatZh(iz,izp) = cctemp1-cctemp2

         endif

      enddo
   enddo
   GridZh=exp(GridZh)*zbar

else
   transMatZh=1.0d0
   GridZh=zbar
endif
GridZh(1)=zbar*(0.85d0**((1.0d0-alpha_h)/(5.0d0/8.0d0)))
GridZh(ngpZh)=zbar

print*,GridZh

if(ngpAy .eq. 3) then
   transMatAy(1,1)=0.97d0
   transMatAY(ngpAY,ngpAY)=0.97d0
   transMatAY(ngpAY-1,ngpAY-1)=0.95d0
   transMatAY(1,ngpAY)=1.0d0-transMatAY(1,1)-0.025d0
   transMatAY(ngpAy,1)=1.0d0-transMatAy(ngpAY,ngpAY)-0.025d0
   transMatAY(ngpAY-1,1)=0.025d0
   transMatAY(ngpAY-1,ngpAy)=0.025d0
   transMatAY(ngpAy,ngpAY-1)=0.025d0
   transMatAY(1,ngpAy-1)=0.025d0
elseif(ngpAy .eq. 2) then
   transMatAy(1,1)=0.90d0
   transMatAY(ngpAY,ngpAY)=0.90d0
   transMatAY(1,ngpAY)=1.0d0-transMatAY(1,1)
   transMatAY(ngpAy,1)=1.0d0-transMatAy(ngpAY,ngpAY)
elseif(ngpAY .eq. 1) then
   transMatAY=1.0d0
endif
!99/98

if(DoRf .eq. 1) then

   transMatC(1,1)=0.90d0
   transMatC(ngpC,ngpC)=0.90d0
   transMatC(1,ngpC)=1.0d0-transMatC(1,1)
   transMatC(ngpC,1)=1.0d0-transMatC(ngpC,ngpC)

   transMatAy(1,1)=0.90d0
   transMatAY(ngpAY,ngpAY)=0.90d0
   transMatAY(1,ngpAY)=1.0d0-transMatAY(1,1)
   transMatAY(ngpAy,1)=1.0d0-transMatAy(ngpAY,ngpAY)


else

   transMatC(1,1)=0.99d0
   transMatC(ngpC,ngpC)=0.99d0
   transMatC(1,ngpC)=1.0d0-transMatC(1,1)
   transMatC(ngpC,1)=1.0d0-transMatC(ngpC,ngpC)

endif

if(ngpC .eq. 1) transMatC=1.0d0

transMatZh(1,1)=0.95d0
transMatZh(ngpZh,ngpZh)=0.95d0
transMatZh(1,ngpZh)=1.0d0-transMatZh(1,1)
transMatZh(ngpZh,1)=1.0d0-transMatZh(ngpZh,ngpZh)
if(ngpZh .eq. 1) transMatZh=1.0d0

transMatRf(1,1)=0.90d0
transMatRf(ngpRf,ngpRf)=0.90d0
transMatRf(1,ngpRf)=1.0d0-transMatRf(1,1)
transMatRf(ngpRf,1)=1.0d0-transMatRf(ngpRf,ngpRf)
if(ngpRf .eq. 1) transMatRf=1.0d0


GridPhiH(1)=phi_h/2.0d0
GridPhiH(ngpC)=phi_h
GridPhiH=phi_h


if(ngpHD .eq. 1) then
   transMatHD=1.0d0
   GridDemand=nu
elseif(ngpHD .eq. 2) then
   transMatHD(1,1)=0.98d0
   transMatHD(ngpHD,ngpHD)=0.98d0
   transMatHD(1,ngpHD)=1.0d0-transMatHD(1,1)
   transMatHD(ngpHD,1)=1.0d0-transMatHD(ngpHD,ngpHD)
   GridDemand(ngpHD)=nu
   GridDemand(1)=nu/10.0d0
else


   if(MarkovSwitch .eq. 0) then

      transMatHD(1,1)=0.95d0
      transMatHD(ngpHD,ngpHD)=0.95d0
      transMatHD(ngpHD,ngpHD-1)=1.0d0-transMatHD(ngpHD,ngpHD)-0.01d0
      transMatHD(ngpHD,1)=0.01d0
      transMatHD(ngpHD-1,1)=0.85d0
      transMatHD(ngpHD-1,ngpHD)=0.025d0
      transMatHD(ngpHD-1,ngpHD-1)=0.125d0
      transMatHD(1,ngpHD)=0.01d0
      transMatHD(1,ngpHD-1)=1.0d0-transMatHD(1,1)-transMatHD(1,ngpHD)



   else



      transMatHD(1,1)=0.80d0 !0.95d0 Benchmark
      transMatHD(1,ngpHD-1)=0.025d0 !0.025d0
      transMatHD(1,ngpHD)=0.175d0 !0.025d0

      transMatHD(ngpHD,ngpHD)=0.95d0
      transMatHD(ngpHD,ngpHD-1)=0.04d0
      transMatHD(ngpHD,1)=0.01d0

      transMatHD(ngpHD-1,1)=0.25d0 !0.45d0
      transMatHD(ngpHD-1,ngpHD-1)=0.5d0 !0.5d0
      transMatHD(ngpHD-1,ngpHD)=0.25d0 !0.05d0


   endif



   select case(beliefshock)

   case(1) !Preference


      GridBelief(ngpHD)=nu
      GridBelief(ngpHD-1)=nu
      GridBelief(1) = nu-0.08d0
   case(2) !Construction productivity

      GridBelief(ngpHD)=zbar
      GridBelief(ngpHD-1)=zbar
      GridBelief(1)=zbar*0.675d0




   case(3) ! TFP

      GridBelief(ngpHD)=1.d0
      GridBelief(ngpHD-1)=1.d0
      GridBelief(1)=1.5d0


   end select







endif

do iAgg=1,ngpHD
   print*,transMatHD(iAgg,:)
enddo


iAggsteady=ngpAgg

iAgg=0
transMatAgg=0.0d0
do iAy=1,ngpAy
    do iZh=1,ngpZh
       do iD=1,ngpNu
        do iHD=1,ngpHD
            do iC=1,ngpC
            do iR=1,ngpRf
                iAgg=iAgg+1
                AtoC(iAgg)=iC
                AtoR(iAgg)=iR
                AtoY(iAgg)=iAy
                AtoZ(iAgg)=iZh
                AtoD(iAgg)=iHD
                CHZYtoA(iC,iHD,iZh,iAy)=iAgg
                iAggp=0
                PhiHGrid(iAgg)=GridPhiH(iC)


                ZhGrid(iAgg)=GridZh(iZh)
                AyGrid(iAgg)=GridAy(iAy)
                NuGrid(iAgg)=GridDemand(iD)

                select case(beliefshock)

                case(1)
                   NuGrid(iAgg)=GridBelief(iHD)
                case(2)
                   ZhGrid(iAgg)=GridBelief(iHD)
                case(3)
                   AyGrid(iAgg)=GridBelief(iHD)
                end select

                LTVBase = 0.95d0


                if(doRf .eq. 1) then

                   LTVGrid(iAgg)=LTVBase+dble(ngpAy-iAy)*(LTVmax-LTVBase)
                   LTIGrid(iAgg)=0.25d0+0.25d0*dble(ngpAy-iAy) !
                   Markupgrid(iAgg)=0.01d0+0.0040d0*dble(iAy-ngpAy)
                   WedgeGrid(iAgg)=1.0d0
                   HELOCGrid(iAgg)=0.2d0
                   RfGrid(iAgg) = rf_base+0.0202d0*dble(iC-ngpC)

                   RmGrid(iAgg)=RfGrid(iAgg)*1.33d0

                else

                   RfGrid(iAgg) = rf_base
                   LTIGrid(iAgg)=0.25d0+0.25d0*dble(ngpC-iC) !
                   WedgeGrid(iAgg)=1.0d0
                   LTVGrid(iAgg)=LTVBase+dble(ngpC-iC)*(LTVmax-LTVBase)
                   RmGrid(iAgg)=RfGrid(iAgg)*1.33d0
                   HELOCGrid(iAgg)=0.2d0
                   FCGrid(iAgg)=mortfc+dble(iC-ngpC)*mortfc*0.4d0
                   Markupgrid(iAgg)=0.01d0+0.004d0*dble(iC-ngpC)

                   ! Uncomment for ATM model
                   ! HELOCGrid(iAgg)=0.2d0+0.10d0*dble(ngpC-iC)
                   ! LTIGrid(iAgg)=0.25d0
                   ! FCGrid(iAgg)=mortfc
                   ! LTVGrid(iAgg)=LTVBase
                   ! Markupgrid(iAgg)=0.01d0

                   ! Uncomment for ARM model
                   ! RmGrid(iAgg)=RfGrid(iAgg)*1.33d0+dble(iC-ngpC)*0.005d0
                   ! LTIGrid(iAgg)=0.25d0
                   ! FCGrid(iAgg)=mortfc
                   ! LTVGrid(iAgg)=LTVBase
                   ! Markupgrid(iAgg)=0.01d0
                endif
                

                InterestCap = 0.975d0
                DoInterestCap(iAgg) = 0

                RationInc(iAgg) = 0
                RationIncLevel = 0.5d0

                AdjGrid(iAgg)=adjval+(giC-ngpC)*liquid




                !March Calibration
                RlGrid(iAgg)=RmGrid(iAgg)

                print*,HELOCGrid(iAgg),RmGrid(iAgg)
                do iAyp=1,ngpAy
                do iZhp=1,ngpZh
                do iHDp=1,ngpHD
                do iCp=1,ngpC
                do iRp=1,ngpRf
                    iAggp=iAggp+1
                    transMatAgg(iAgg,iAggp)=transMatAY(iAy,iAyp)*transMatZh(iZh,iZhp)*transMatHD(iHD,iHDp)*transMatC(iC,iCp)*transMatRf(iR,iRp)
                    print*,iAgg,iAggp,transMatAgg(iAgg,iAggp)
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
                print*,iAgg,sum(transMatAgg(iAgg,:))

             enddo
          enddo
       enddo
    enddo
 enddo
print*,'ZhGrid:'
print*,ZhGrid
!pause
print*,'RationInc'
print*,RationInc

print*,'InterestCap'
print*,DoInterestCap


else


   do iAgg=1,ngpAgg
      AtoC(iAgg)=iAgg
      AtoR(iAgg)=iAgg
      AtoY(iAgg)=iAgg
      AtoZ(iAgg)=iAgg
      AtoD(iAgg)=iAgg
   enddo
   call Rouwenhorst(ngpAgg,0.98d0,0.98d0,transMatAgg)
   lwidth=0.02d0
   lxa=1.0d0-lwidth*(dble((ngpAgg-1)/2))
   do iAgg=1,ngpAgg
      GridAy(iAgg)=lxa+lwidth*dble(iAgg-1)
   enddo


   lxa=0.8d0
   lwidth=(LTVMax-lxa)/(dble((ngpAgg-1)))

   do iAgg=1,ngpAgg
      GridC(iAgg)=lxa+lwidth*dble(iAgg-1)
   enddo

   lxa=rf_base
   lwidth=(0.005d0-rf_base)/(dble((ngpAgg-1)))

   do iAgg=1,ngpAgg
      GridRf(iAgg)=lxa+lwidth*dble(iAgg-1)
   enddo


   GridDemand=1.0d0
   GridZh=zbar


   PhgridTemp(ngpPh)  = 0.35d0
   PhgridTemp(1)  = -PhgridTemp(ngpPh)
   lwidth = (PhgridTemp(ngpPh) - PhgridTemp(1)) / real(ngpPh - 1)

   do iz=2,(ngpPh-1)
      PhgridTemp(iz) = PhgridTemp(1) + lwidth * real(iz - 1);
   enddo
   Phgrid=1.0d0+PhGridTemp

   iAggsteady=1






endif






end subroutine AggGrids

!
!   ComputeSteady.f90
!   
!
!   Created by Kurt Mitman on 20/04/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!


subroutine ComputeSteady


    use omp_lib
    use params
    use globals


    implicit none

    integer::iter,iter2,iAgg,iAggp,ccount,steadycomputed
    double precision:: phval,pmax,pmin,pupper,plower,Hsupply,xstol,xsdif,lb,step
    integer::iAggvec(ngpAgg)
    pmax=1.6d0
    pmin=0.4d0
    xstol=2.5d-1
    do iter=1,ngpAgg
       iAggvec(iter)=iter
    enddo
    iAggvec(ngpAgg)=iAggsteady
    iAggvec(iAggsteady)=ngpAgg
    do iter2=1,2 !ngpAgg
       if(iter2 .eq. 1) then
          iter=1
       else
          iter=ngpAgg
       endif
!       iter=iAggvec(iter2)
       Call AggGrids
        pupper=pmax
        plower=pmin
        transMatAgg=0.0d0
        do iAgg=1,ngpAgg
            transMatAgg(iAgg,iAgg)=1.0d0
        enddo
        giRf=AtoR(giAgg)
        rf=GridRf(giRf)

        steadycomputed=0
        phval=1.0d0
        Call ComputeLOM
        Call DecisionsRet
        Call Decisions
        giPh=1
        giAgg=iter
        giAy=AtoY(giAgg)
        giZh=AtoZ(giAgg)

        do giPh=1,ngpPh
            call SimulateSteadyDist

!           Hsupply = (((phgrid(giPh))**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h/GridAY(giAy))**(alpha_h/(1.0d0-alpha_h))))/deltah
           Hsupply = (((phgrid(giPh))**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h))))/deltah
           xsdif = abs(Hsupply-sshdemand)
           if(xsdif .lt. xstol) then
              steadycomputed=1
              exit
           endif
           if(sshdemand .lt. Hsupply) exit

        enddo
        print*,iter,sshdemand,Hsupply
        step=1.0d0

        do while(steadycomputed .eq. 0 .and. step .gt. xstol .and. giPh .le. ngpPh)

           step=(phgrid(giph)-phgrid(giph-1))/real(ngpph-1)
           lb=phgrid(giph-1)
           do giPh=1,ngpPh
              PhGrid(giPh)=lb+real(giPh-1)*step
           enddo

           Call ComputeLOM
           Call DecisionsRet
           Call Decisions
           giPh=1
           giAgg=iter
           giAy=AtoY(giAgg)
           giZh=AtoZ(giAgg)
           giRf=AtoR(giAgg)
           rf=GridRf(giRf)
           do giPh=1,ngpPh
              call SimulateSteadyDist
!              Hsupply = (((phgrid(giPh))**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h/GridAY(giAy))**(alpha_h/(1.0d0-alpha_h))))/deltah
              Hsupply = (((phgrid(giPh))**(1.0d0/(1.0d0-alpha_h)))*((GridZh(giZh))**(1.0d0/(1.0d0-alpha_h)))*((alpha_h)**(alpha_h/(1.0d0-alpha_h))))/deltah
              xsdif = abs(Hsupply-sshdemand)
              if(xsdif .lt. xstol) then
                 steadycomputed=1
                 exit
              endif
              if(sshdemand .lt. Hsupply) exit
              
           enddo
           print*,iter,sshdemand,Hsupply

        end do
        ssph(iter)=phgrid(min(giPh,ngpPh))
        Hsteady=sshdemand
    enddo
    ssph(1:ngpAgg-1)=ssph(1)





end subroutine ComputeSteady

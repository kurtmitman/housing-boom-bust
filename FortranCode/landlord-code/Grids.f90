SUBROUTINE Grids

USE Params
USE Globals
USE Procedures
USE Random
Use Funcs

IMPLICIT NONE

INTEGER                 :: ie,ij,iff,iz,ip,iR,iW,ifslo,iflev,izp,iAy,iAgg,iZh,iHD,iC,iAyp,iZhp,iHDp,iCp,iAggp
INTEGER                 :: hc,mc,lc,mcc,bc,mtemp
double precision         :: lwidth,lxa,lxb,lxc,lfa,lfb,lfc,lm,lfval,lylow,lEz,lSDz,lalpha,lbeta,lEyav,lSDyav,lEpen,slowidth,levwidth
!double precision, EXTERNAL        :: FnTax,FnBondPrice,FnSSBenefit,FnGridPers,GOLDEN
double precision, EXTERNAL  :: Golden
double precision,dimension(ngpfylev)::xlevvals
double precision,dimension(ngpfyslo)::xslovals
double precision    :: curv
double precision    :: GridBneg(max(nbneg,1)),GridBpos(nb-nbneg+1)
double precision    :: cctemp,cctemp1,cctemp2
double precision    :: sigz,rhop
double precision    :: PhGridTemp(ngpPh)
!Moving shock
!psidist(1)=0.9d0
!psidist(2)=1.0d0-psidist(1)
psidist(1)=1.0d0
print*,zbar
!House price shock

do ij=1,Jret
    disutil(ij)=1.0d0*dble(ij)/dble(Jret)
enddo
disutil=0.0d0

!March 16, 2016 GridH(hc)=1.5d0+0.5d0*(real(hc-1)), GridH(hc)=GridH(hc-1)+0.75d0

!DO hc=1,nh-3
!    GridH(hc)=1.5d0+0.5d0*(real(hc-1))
!ENDDO
!DO hc=nh-2,nh
!    GridH(hc)=GridH(hc-1)+0.75d0
!ENDDO
!GridH(nh)=GridH(nh)+0.5d0

if(NoRent .eq. 0) then
  GridH(1)=1.50d0 !*0.75d0 !*1.28d0*1.28d0 !*1.10d0
  !GridH(1)=1.50d0/1.28d0 !*1.10d0 No rent
  do hc=2,nh
     GridH(hc)=1.28d0*GridH(hc-1)
!     GridH(hc)=1.35d0*GridH(hc-1) !Allow for bigger grid
  enddo
else

!  GridH(1) = 0.35d0 !*1.10d0
!  GridH(2) = 0.85d0 !*1.10d0

  !GridH(1)=1.50d0/1.28d0 !*1.10d0 No rent
!  GridH(3) = 1.50d0
!  do hc=4,nh
!     GridH(hc)=1.28d0*GridH(hc-1)
!  do hc = 3, nh
!     GridH(hc) = 1.44d0*GridH(hc-1)
!  enddo
   GridH(1) = 0.5d0
   GridH(2) = 1.0d0
   GridH(3) = 1.5d0
   GridH(4) = 2.0d0
   GridH(5) = 2.75d0
   GridH(6) = 4.0d0
   GridH(nh) = 5.15d0


endif
!   GridH(hc)=1.14d0*GridH(hc-1)


!Calibrated Grids Sep 20 curv=0.6, 1.60,5.0
!curv=0.7d0
!CALL PowerSpacedGrid(nh,curv,1.6d0,5.0d0,GridH)




!CALL PowerSpacedGrid(nh,curv,1.75d0,6.0d0,GridH)

!GridH(nh)=
!GridH(nh-1)=GridH(nh-1)+0.5d0
!GridH(nh)=GridH(nh)+0.75d0
if(nhr .eq. nh) then
   do hc=1,nhr
      GridHR(hc)=GridH(hc)
   enddo

else
   !Calibrated HR(1) Sep 20 =1.2d0
!   GridHR(1)=1.2d0
   !Old GridH(1)*0.65d0
!   GridHR(1)=GridH(1)*0.60d0 !1.0d0

   !March 16, 2016 - HR(1)=GridH(1)*0.75, GridHR(hc)=GridH(hc-1)

!   GridHR(1)=GridH(1)/1.28d0
   ! if(NoRent .eq. 0) then
   !    GridHR(1)=GridH(1)*0.75d0
   !    do hc=2,nhr
   !       GridHR(hc)=GridH(hc-1) !*0.925d0
   !    enddo
   ! else
   !    GridHR = GridH(1)/2.0d0
   ! endif
!   GridHR(nhr)=4.0d0/3.0d0 !0.5d0
endif

print*,'GridHR'
do hc=1,nhr
   Print*,hc,GridHR(hc)
enddo

!GridH = GridH/(1.0d0+dble(BiAnnual))
!GridHR = GridHR/(1.0d0+dble(BiAnnual))

if(nm .GT. 1) then
   mtemp=nm/2
   mtemp=4
    DO mc=1,mtemp
        GridM(mc)=(real(mc)-1.0d0)/(real(mtemp))*(minval(Phgrid)-0.15d0)
    ENDDO
    lwidth=(maxval(Phgrid)-minval(Phgrid)+0.15d0)/real(nm-mtemp-1)

    DO mc=mtemp+1,nm
        GridM(mc)=real(mc-mtemp-1)*lwidth+minval(Phgrid)-0.15d0
    ENDDO


    do mc=1,nm-1
        do mcc=1,msimscale

            GridMsim((mc-1)*msimscale+mcc)=(GridM(mc+1)-GridM(mc))*dble(mcc-1)/dble(msimscale) + GridM(mc)
        ENDDO
    enddo
    GridMsim(nmsim)=GridM(nm)
    do mc=1,nmsim
       print*,mc,GridMsim(mc)

    enddo
    do mc=1,nm
        MtoMsim(mc)=1+(mc-1)*msimscale
    enddo



else

    GridM(1)=0.0d0

endif


if(nl .GT. 1) then

    DO lc=1,nl
    !    GridL(lc)=(real(lc)-1.0d0)/(real(nl)-1.0d0)*HELOCmax*maxval(Phgrid)
        GridL(lc)=(real(lc)-1.0d0)/(real(nl)-1.0d0)*0.15d0
    ENDDO

    !DO lc=nl,nl-ngpPh+1,-1
    !    GridL(lc)=LTVmax*Phgrid(ngpPh+lc-nl)
    !ENDDO
    !DO lc=nl-ngpPh,1,-1
    !   GridL(lc)=(real(lc)-1.0d0)/(real(nl-ngpPh+1)-1.0d0)*LTVmax*Phgrid(1)
    !ENDDO

else
    GridL(1)=0.0d0
endif

!curv=0.4d0
if(nbneg>0) then
   curv=0.7d0
   CALL PowerSpacedGrid(nbneg,curv,bmin,0.0d0,GridBNeg)
   curv=0.4d0
   CALL PowerSpacedGrid(nb-nbneg+1,curv,0.0d0,bmax,GridBpos)
   GridB(1:max(nbneg,1))=GridBneg
   GridB(max(1,nbneg):nb)=GridBpos
else
   curv=0.4d0
   CALL PowerSpacedGrid(nb,curv,0.0d0,bmax,GridB)





endif
    do mc=1,nb-1
        do mcc=1,bsimscale

            GridBsim((mc-1)*bsimscale+mcc)=(GridB(mc+1)-GridB(mc))*dble(mcc-1)/dble(bsimscale) + GridB(mc)
        ENDDO
    enddo
    GridBsim(nbsim)=GridB(nb)

do bc=1,nbsim
   call basefun(GridB,nb,GridBsim(bc),bcvals(:,bc),bcinds(:,bc))
   print*,bc,bcvals(:,bc)
   print*,bcinds(:,bc)
   print*,GridBsim(bc),sum(bcvals(:,bc))
enddo


do bc=1,nbsim
    bmat(bc,:,:,:,:,:)=GridBsim(bc)
    bmatj(bc,:,:,:,:)=GridBsim(bc)
    bmatr(bc,:,:)=GridBsim(bc)
    bmatjr(bc,:)=GridBsim(bc)
ENDDO
do bc=1,nmsim
    mmat(:,:,bc,:,:,:)=GridMsim(bc)
    mmatj(:,:,bc,:,:)=GridMsim(bc)
ENDDO
do bc=1,nh
    hmat(:,:,:,bc,:,:)=GridH(bc)
    hmatj(:,:,:,bc,:)=GridH(bc)
ENDDO
do bc=1,nl
    lmat(:,bc,:,:,:,:)=GridL(bc)
    lmatj(:,bc,:,:,:)=GridL(bc)
ENDDO





!print*,GridB
!pause
!CALL PowerSpacedGrid (nm,curv,0.0d0,0.8d0,GridM)
!CALL PowerSpacedGrid (nm,curv,GridH(1)/10.d0,GridH(nh),GridM)


!Transitory shocks: earnings
IF (ngpey==1) THEN
        eygrid = 0.0
        eydist = 1.0
ELSE
        eygrid(1) = -sqrt((ngpey-1)*Vepsy)
        eygrid(ngpey) = -eygrid(1)
        eydist(1) = bin_prob(ngpey-1, 0.5_4, 0)
        eydist(ngpey) = eydist(1)
        lwidth = (eygrid(ngpey)-eygrid(1))/real(ngpey-1)
                DO ie = 2,ngpey-1
                eygrid(ie) = eygrid(ie-1) + lwidth
                eydist(ie) = bin_prob(ngpey-1, 0.5_4, ie-1)
        END DO
END IF

!Fixed effect: earnings levels
IF(ngpfylev > 1 .AND. ngpfyslo > 1) THEN

    allocate(fylevgrid(ngpfylev*ngpfyslo))
    allocate(fyslogrid(ngpfylev*ngpfyslo))
    allocate(fylevdist(ngpfylev))
    allocate(fyslodist(ngpfyslo))

        xlevvals(1)=-sqrt(Vfey*real(ngpfylev-1))
        xlevvals(ngpfylev)=-xlevvals(1)
        xslovals(1)=-sqrt(Vphy*real(ngpfyslo-1))
        xslovals(ngpfyslo)=-xslovals(1)
        levwidth = (xlevvals(ngpfylev)-xlevvals(1))/real(ngpfylev-1)
        slowidth = (xslovals(ngpfyslo)-xslovals(1))/real(ngpfyslo-1)

        fylevdist(1) = bin_prob(ngpfylev-1, 0.5_4, 0)
        fylevdist(ngpfylev)=fylevdist(1)

        DO iflev = 2,ngpfylev-1
                xlevvals(iflev)=xlevvals(iflev-1)+levwidth
                fylevdist(iflev) = bin_prob(ngpfylev-1, 0.5_4, iflev-1)
        END DO

        fyslodist(1) = bin_prob(ngpfyslo-1, 0.5_4, 0)
        fyslodist(ngpfyslo)=fyslodist(1)

        DO ifslo = 2,ngpfyslo-1
                xslovals(ifslo)=xslovals(ifslo-1)+slowidth
                fyslodist(ifslo) = bin_prob(ngpfyslo-1, 0.5_4, ifslo-1)
        END DO

        iff=0
        DO iflev = 1,ngpfylev
                DO ifslo = 1,ngpfyslo
                        iff = iff+1

                        fyslogrid(iff)=xslovals(ifslo)
                        fylevgrid(iff)=rholevslo*xslovals(ifslo)+sqrt((1.0d0-rholevslo**2))*xlevvals(iflev)

                        fylevind(iff) = iff
                        fysloind(iff) = iff
                        fyind(iflev,ifslo) = iff
                END DO
        END DO


    OPEN(3, FILE = trim(OutputDir) // 'fylevdist.txt', STATUS = 'replace')
    CALL WriteMatrix(3,ngpfylev,1,fylevdist)
    OPEN(3, FILE = trim(OutputDir) // 'fyslodist.txt', STATUS = 'replace')
    CALL WriteMatrix(3,ngpfyslo,1,fyslodist)
    OPEN(3, FILE = trim(OutputDir) // 'fylevgrid.txt', STATUS = 'replace')
    CALL WriteMatrix(3,ngpfy,1,fylevgrid)
    OPEN(3, FILE = trim(OutputDir) // 'fyslogrid.txt', STATUS = 'replace')
    CALL WriteMatrix(3,ngpfy,1,fyslogrid)
    OPEN(3, FILE = trim(OutputDir) // 'fylevind.txt', STATUS = 'replace')
    CALL WriteMatrixInteger(3,ngpfy,1,fylevind)
    OPEN(3, FILE = trim(OutputDir) // 'fysloind.txt', STATUS = 'replace')
    CALL WriteMatrixInteger(3,ngpfy,1,fysloind)


ELSE
    allocate(fylevgrid(ngpfylev))
    allocate(fyslogrid(ngpfyslo))
    allocate(fylevdist(ngpfylev))
    allocate(fyslodist(ngpfyslo))

    IF (ngpfylev==1) THEN
            fylevgrid = 0.0
            fylevdist = 1.0
        ELSE
            fylevgrid(1) = -sqrt((ngpfylev-1)*Vfey)
            fylevgrid(ngpfylev) = -fylevgrid(1)
            fylevdist(1) = bin_prob(ngpfylev-1, 0.5_4, 0)
            fylevdist(ngpfylev) = fylevdist(1)
            lwidth = (fylevgrid(ngpfylev)-fylevgrid(1))/real(ngpfylev-1)
            DO ie = 2,ngpfylev-1
                    fylevgrid(ie) = fylevgrid(ie-1) + lwidth
                    fylevdist(ie) = bin_prob(ngpfylev-1, 0.5_4, ie-1)
            END DO
    END IF

    !Fixed effect: earnings slopes
    IF (ngpfyslo == 1 .or. FlatEarningsProfile==1) THEN
            fyslogrid = 0.0
            fyslodist = 1.0/real(ngpfyslo)
    ELSE

            fyslogrid(1) = -sqrt((ngpfyslo-1)*Vphy)
            fyslogrid(ngpfyslo) = -fyslogrid(1)
            fyslodist(1) = bin_prob(ngpfyslo-1, 0.5_4, 0)
            fyslodist(ngpfyslo) = fyslodist(1)
            lwidth = (fyslogrid(ngpfyslo)-fyslogrid(1))/real(ngpfyslo-1)
            DO ie = 2,ngpfyslo-1
                    fyslogrid(ie) = fyslogrid(ie-1) + lwidth
                    fyslodist(ie) = bin_prob(ngpfyslo-1, 0.5_4, ie-1)
            END DO
    END IF

    iff = 0
    DO iflev = 1,ngpfylev
    DO ifslo = 1,ngpfyslo
            iff = iff+1
            fylevind(iff) = iflev
            fysloind(iff) = ifslo
            fyind(iflev,ifslo) = iff
    END DO
    END DO
!    OPEN(3, FILE = trim(OutputDir) // 'fylevdist.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpfylev,1,fylevdist)
!    OPEN(3, FILE = trim(OutputDir) // 'fyslodist.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpfyslo,1,fyslodist)
!    OPEN(3, FILE = trim(OutputDir) // 'fylevind.txt', STATUS = 'replace')
!    CALL WriteMatrixInteger(3,ngpfy,1,fylevind)
!    OPEN(3, FILE = trim(OutputDir) // 'fysloind.txt', STATUS = 'replace')
!    CALL WriteMatrixInteger(3,ngpfy,1,fysloind)

ENDIF




!Persistent component: earnings. Rouwenhorst works poorly outside of steady state so use modified Tauchen method
IF (ngpzy==1) THEN
        Vzy = 0.0
        zygrid = 0.0
        zytrans = 1.0
        zydist = 1.0
ELSE

        Vzy(1) = Vzy0 + (rhoy**2)*Vetay
        DO ij = 2, Jwork
                Vzy(ij) = (rhoy**2)*Vzy(ij-1) + Vetay
        END DO

        lxa = 1.0e-8
        lxb = 1.0
        if(DoMJ .EQ. 0) then
           CALL MNBRAK(lxa,lxb,lxc,lfa,lfb,lfc,FnGridPers)
           lfval = GOLDEN(lxa,lxb,lxc,FnGridPers,1.0e-6_8,lm)
           lfval = FnGridPers(lm)
        ELSE
           CALL MNBRAK(lxa,lxb,lxc,lfa,lfb,lfc,FnGridPersMJ)
           lfval = GOLDEN(lxa,lxb,lxc,FnGridPersMJ,1.0e-6_8,lm)
           lfval = FnGridPersMJ(lm)
        ENDIF
    OPEN(3, FILE = trim(OutputDir) // 'zygrid.txt', STATUS = 'replace')
    CALL WriteMatrix(3,Jwork,ngpzy,zygrid)
    OPEN(3, FILE = trim(OutputDir) // 'zygridlev.txt', STATUS = 'replace')
    CALL WriteMatrix(3,Jwork,ngpzy,exp(zygrid))
    OPEN(3, FILE = trim(OutputDir) // 'zydist.txt', STATUS = 'replace')
    CALL WriteMatrix(3,Jwork,ngpzy,zydist)
    OPEN(3, FILE = trim(OutputDir) // 'zytrans1.txt', STATUS = 'replace')
    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(1,:,:))
    OPEN(3, FILE = trim(OutputDir) // 'zytrans11.txt', STATUS = 'replace')
    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(Jwork/2+1,:,:))
!    OPEN(3, FILE = trim(OutputDir) // 'zytrans41.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(41,:,:))
!    OPEN(3, FILE = trim(OutputDir) // 'zytrans61.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(61,:,:))
!    OPEN(3, FILE = trim(OutputDir) // 'zytrans81.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(81,:,:))
!    OPEN(3, FILE = trim(OutputDir) // 'zytrans101.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(101,:,:))
!    OPEN(3, FILE = trim(OutputDir) // 'zytrans121.txt', STATUS = 'replace')
!    CALL WriteMatrix(3,ngpzy,ngpzy,zytrans(121,:,:))
END IF
!STOP
!pause
!Earnings, conditional on working
DO ij = 1,Jwork
        DO ie = 1,ngpey
        DO iz = 1,ngpzy
        DO iff = 1,ngpfy
        DO iAy = 1, ngpAy
                iflev = fylevind(iff)
                ifslo = fysloind(iff)
            !    print*,kappay(ij),zygrid(ij,iz),fylevgrid(iflev) + ij*fyslogrid(ifslo), eygrid(ie),GridAY(iAy)
                ygrid(ij,iff,iz,ie,iAy) = exp(kappay(ij)+ fylevgrid(iflev) + ij*fyslogrid(ifslo) + zygrid(ij,iz) + eygrid(ie)+log(GridAY(iAy)))
                !persistent component of earnings
                if(ie==1) ypsgrid(ij,iff,iz,iAy) = exp(kappay(ij)+ fylevgrid(iflev) + ij*fyslogrid(ifslo) + zygrid(ij,iz)+log(GridAY(iAy)))
        END DO
        END DO
        END DO
        END DO
END DO
!ypsgrid=ypsgrid/(DataAvAnnualEarns/numeraire)
!ygrid=ygrid/(DataAvAnnualEarns/numeraire)
!print*,ypsgrid(Jwork,1,:)
!pause

! !Mean gross earnings grid for pension
print*,ypsgrid(:,1,1,1)
print*,ypsgrid(:,1,ngpzy,1)
!pause
CALL SimulateEarnings
print*,ypsgrid(:,1,1,1)
print*,'Hi'
print*,ypsgrid(:,1,5,1)

print*,ngpzy
print*,'Hi'

!DO ij = 1,Jwork
! print*,ij,sum(zygrid(ij,:)*ygrid(ij,1,:,1,1)),sum(zygrid(ij,:)*ypsgrid(ij,1,:,1))
!ENDDO
!stop

!STOP

GridBS(1)=-1.0d0*maxval(ysim)-1.d0


lwidth=GridBS(1)/real(nbs-1)

DO iz=1,nbs
    GridBS(iz)=GridBS(1)-real(iz-1)*lwidth
ENDDO


IF (FixedPension==1) THEN
        pgrid(Jwork+1:Jtot,:) = meanpension
        pgrid(1:Jwork,:) = 0.0

ELSE IF(LinearPension==1) THEN
        IF (ngpp .ne. ngpzy*ngpfy) THEN
                write(*,*) 'ERROR: for pension fn of final persistent must have ngpp=ngpzy*ngpfy'
                stop
        END IF
        lEz = sum(ypssim(:,Jwork))/real(nsim)
        lSDz = sqrt(sum((ypssim(:,Jwork))**2)/real(nsim) - lEz**2)
        lbeta = stdvpension/lSDz
        lalpha = meanpension - lbeta*lEz
        ip = 1
        DO iz = 1,ngpzy
        DO iff = 1,ngpfy
                pgrid(Jwork+1:Jtot,ip) = lalpha + lbeta*ypsgrid(Jwork,iff,iz,ngpAy/2+1)
                ip = ip+1
        END DO
        END DO
        pgrid(1:Jwork,:) = 0.0
ELSE IF(ConcavePension==1) THEN
        IF (ngpp/ngppv .ne. ngpzy*ngpfy) THEN
                write(*,*) 'ERROR: for pension fn of final persistent must have ngpp=ngpzy*ngpfy'
                stop
        END IF
        giAY=1 !ngpAy
        gif=1
        !rescale final persistent component so it has same mean and st dev as average accumulated earnings, then feed through sysetem
!        lEz = sum(zydist(Jwork,:)*ypsgrid(Jwork,gif,:,giAY))
!        lSDz = sqrt(sum(zydist(Jwork,:)*ypsgrid(Jwork,gif,:,giAY)**2) - lEz**2)
!        lEyav = sum(zydist(Jwork,:)*min(ypsgrid(Jwork,gif,:,giAY),pencap))
!        print*,lEz,lSDz,lEyav,lSDyav

!        lSDyav = sqrt(sum(zydist(Jwork,:)*min(ypsgrid(Jwork,gif,:,giAY),pencap)**2) - lEyav**2)

        lEz = sum(ypssim(:,Jwork))/real(nsim)
        lSDz = sqrt(sum((ypssim(:,Jwork))**2)/real(nsim) - lEz**2)
        lEyav = sum(yavsimforpen(:,Jwork))/real(nsim)
!        write(*,*) yavsimforpen(:,Jwork)

        print*,lEz,lEyav
!        pause

        lSDyav = sqrt(sum((yavsimforpen(:,Jwork))**2)/real(nsim) - lEyav**2)
        ip = 1
        DO iz = 1,ngpzy
        DO iff = 1,ngpfy
                pgrid(Jwork+1:Jtot,ip) = min(pencap,FnSSBenefit((ypsgrid(Jwork,iff,iz,giAy)-lEz)*lSDyav/lSDz +lEyav))
                ip = ip+1
        END DO
        END DO

        pgrid(1:Jwork,:) = 0.0
END IF
write(*,*) pgrid(1,1)   !for some reason, without writing an element of pgrid to the screen compilation gives wrong answers when compiling with -openmp

!finally re-scaler pension benefits to have correct mean: assumes no age effects in pension
IF(LinearPension==1 .or. ConcavePension==1) THEN
        lEpen = 0.0
        ip = 1
        IF( ngpfylev > 1 .AND. ngpfyslo > 1) THEN
                DO iz = 1,ngpzy
                DO iflev = 1,ngpfylev
                DO ifslo = 1,ngpfyslo
                        lEpen = lEpen +pgrid(Jwork+1,ip)*zydist(Jwork,iz)*fylevdist(iflev)*fyslodist(ifslo)
                        ip = ip+1
                END DO
                END DO
                END DO
                pgrid = pgrid*meanpension/lEpen
        ELSE
                DO iz = 1,ngpzy
                DO iff = 1,ngpfy
                        iflev = fylevind(iff)
                        ifslo = fysloind(iff)
                        lEpen = lEpen +pgrid(Jwork+1,ip)*zydist(Jwork,iz)*fylevdist(iflev)*fyslodist(ifslo)
                        ip = ip+1
                END DO
                END DO

                pgrid = pgrid*meanpension/lEpen
                lEpen = 0.0d0
                lEz = 0.0d0
                ip = 0
                DO iz = 1,ngpzy
                   if(pgrid(Jwork+1,iz) .le. pencap) then
                      ip = iz
                      lEpen = lEpen + pgrid(Jwork+1,iz)*zydist(Jwork,iz)
                      lEz = lEz + zydist(Jwork,iz)
                    endif
                ENDDO

                lEyav=(meanpension-(1.0d0-lEz)*pencap)/lEpen
                pgrid(Jwork+1:Jtot,1:ip)=pgrid(Jwork+1:Jtot,1:ip)*lEyav
                if(ip+1 .LE. ngpzy) pgrid(Jwork+1:Jtot,ip+1:ngpzy)=pencap

        ENDIF

!        pgrid = (1.0d0+dble(BiAnnual))*pgrid*meanpension/lEpen




END IF

do ip=1,ngpzy
print*,pgrid(Jwork+1,ip),zydist(Jwork,ip)
enddo

iW = 1
DO iz = 1,ngpzy
DO iff = 1,ngpfy
        fWind(iW) = iff
        zWind(iW) = iz
        Wind(iff,iz) = iW
        iW = iW+1
END DO
END DO

!Total state: retired
iR = 1
DO ip = 1,ngpp
        pRind(iR) = ip
        Rind(ip) = iR
        iR = iR+1
END DO


!DO iff = 1,ngpfy
!        lylow = (1.0-maxval(dgrid))*ygrid(Jwork,iff,1,1)-FnTax(ygrid(Jwork,iff,1,1))
!        nbl(Jwork,iff) = -lylow + 1.0e-4
!        nbl(Jwork,iff) = min(nbl(Jwork,iff),0.0)
!        DO ij = Jwork-1,1,-1
!                lylow = (1.0-maxval(dgrid))*ygrid(ij,iff,1,1)-FnTax(ygrid(ij,iff,1,1))
!                nbl(ij,iff) = nbl(ij+1,iff)*FnQb(nbl(ij+1,iff),0.0d0)
!                nbl(ij,iff) = min(nbl(ij,iff),0.0)
!        END DO
!END DO

delt=1.0d0


OPEN(1, FILE = InputDir // 'initw.txt')
DO ij=1,ngpzy
    READ(1,*) initadist(ij,1),initadist(ij,2),initashare(ij)
    initadist(ij,2)=1.0d0-initadist(ij,1)
END DO
CLOSE(1)

!initashare=initashare/sum(initashare)
iff=1
giAy=1
do ij=1,Jwork
   do iz=1,ngpzy
      ymat(:,:,:,:,iz,ij)=ypsgrid(ij,iff,iz,giAy)
      ymatr(:,iz,ij)=ypsgrid(ij,iff,iz,giAy)
   enddo
enddo
do ij=Jwork+1,Jtot
   do iz=1,ngpzy
      ymat(:,:,:,:,iz,ij)=pgrid(ij,iz)
      ymatr(:,iz,ij)=pgrid(ij,iz)
   enddo
enddo




END SUBROUTINE Grids

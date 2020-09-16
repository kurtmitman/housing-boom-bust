SUBROUTINE SaveOutput

USE params
USE globals
USE Procedures

integer::it
double precision,allocatable::larray(:,:)

    OPEN(3, FILE = trim(OutputDir) // 'Jwork.txt', STATUS = 'replace');WRITE(3,*) Jwork;CLOSE(3)
    OPEN(3, FILE = trim(OutputDir) // 'Jret.txt', STATUS = 'replace');WRITE(3,*) Jret;CLOSE(3) 
    OPEN(3, FILE = trim(OutputDir)// 'nsim.txt', STATUS = 'replace');WRITE(3,*) nsim;CLOSE(3)
    OPEN(3, FILE = trim(OutputDir)// 'bppcoef.txt', STATUS = 'replace');WRITE(3,*) bppcoef;CLOSE(3)




!    OPEN(3, FILE = trim(OutputDirSims)// 'ysim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jwork,ysim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'psim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,psim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'hsim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,hsim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'hconssim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,hconssim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'bsim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,bsim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'lsim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,lsim)


!    OPEN(3, FILE = trim(OutputDirSims)// 'msim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,msim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'sellsim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,sellsim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'buysim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,buysim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'foresim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,foresim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'csim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,conssim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'rsim.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,refisim)







OPEN(3, FILE = trim(OutputDir) // 'a0.txt', STATUS = 'replace')
CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa0)

OPEN(3, FILE = trim(OutputDir) // 'a1.txt', STATUS = 'replace')
CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa1)

if(ngpAgg .eq. 12) then
   OPEN(3, FILE = trim(OutputDir) // 'qm3.txt', STATUS = 'replace')
   CALL WriteMatrix3(3,nm,ngpPh,ngpAgg*ngpW,qm(nbneg,1,:,3,:,:,2))
   !OPEN(3, FILE = trim(OutputDir) // 'qm12.txt', STATUS = 'replace')
   !CALL WriteMatrix2(3,nm,ngpPh,qm(nbneg,1,:,3,12,:,2))
endif

OPEN(3, FILE = trim(OutputDir) // 'agepathEage.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEage)

OPEN(3, FILE = trim(OutputDir) // 'agepathEown.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEown)

OPEN(3, FILE = trim(OutputDir) // 'agepathEdh.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEdh)

OPEN(3, FILE = trim(OutputDir) // 'agepathEdp.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEdp)

OPEN(3, FILE = trim(OutputDir) // 'agepathEnw.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEnw)

OPEN(3, FILE = trim(OutputDir) // 'agepathEnwown.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEnwown)

OPEN(3, FILE = trim(OutputDir) // 'agepathEnwrent.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEnwrent)

OPEN(3, FILE = trim(OutputDir) // 'agepathErent.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathErent)

OPEN(3, FILE = trim(OutputDir) // 'agepathEinc.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEinc)

OPEN(3, FILE = trim(OutputDir) // 'agepathEincrent.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEincrent)

OPEN(3, FILE = trim(OutputDir) // 'agepathEincown.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEincown)

OPEN(3, FILE = trim(OutputDir) // 'agepathEhnw.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhnw)

OPEN(3, FILE = trim(OutputDir) // 'agepathEhnwshare.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhnwshare)

OPEN(3, FILE = trim(OutputDir) // 'agepathEloc.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEloc)

OPEN(3, FILE = trim(OutputDir) // 'agepathEpen.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEpen)

OPEN(3, FILE = trim(OutputDir) // 'agepathEcon.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEcon)

OPEN(3, FILE = trim(OutputDir) // 'agepathEhcon.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhcon)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconrent.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconrent)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconown.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconown)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconinchigh.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconinchigh)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconinclow.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconinclow)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconhighltv.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconhighltv)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconlowltv.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconlowltv)








OPEN(3, FILE = trim(OutputDir) // 'agepathEass.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEass)

OPEN(3, FILE = trim(OutputDir) // 'agepathEmort.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEmort)

OPEN(3, FILE = trim(OutputDir) // 'agepathEhouse.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhouse)

OPEN(3, FILE = trim(OutputDir) // 'agepathEbuy.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEbuy)

OPEN(3, FILE = trim(OutputDir) // 'agepathEsell.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEsell)

OPEN(3, FILE = trim(OutputDir) // 'agepathEmortpos.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEmortpos)

OPEN(3, FILE = trim(OutputDir) // 'agepathEmlpos.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEmlpos)

OPEN(3, FILE = trim(OutputDir) // 'agepathElocpos.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathElocpos)

OPEN(3, FILE = trim(OutputDir) // 'agepathElev.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathElev)

OPEN(3, FILE = trim(OutputDir) // 'agepathEfore.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEfore)

OPEN(3, FILE = trim(OutputDir) // 'agepathEforem.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEforem)

OPEN(3, FILE = trim(OutputDir) // 'agepathErefi.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathErefi)

OPEN(3, FILE = trim(OutputDir) // 'agepathEtax.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEtax)


OPEN(3, FILE = trim(OutputDir) // 'agepathEhnw.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhnwsim)
OPEN(3, FILE = trim(OutputDir) // 'agepathEdelhnw.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEdelhnwsim)

OPEN(3, FILE = trim(OutputDir) // 'pathPh.txt', STATUS = 'replace')
CALL WriteVector(3,1,Tsim,Phpath(1:Tsim))
OPEN(3, FILE = trim(OutputDir) // 'pathPr.txt', STATUS = 'replace')
CALL WriteVector(3,1,Tsim,Prpath(1:Tsim))
OPEN(3, FILE = trim(OutputDir) // 'pathHt.txt', STATUS = 'replace')
CALL WriteVector(3,1,Tsim,Htpath(1:Tsim))
OPEN(3, FILE = trim(OutputDir) // 'iAggpath.txt', STATUS = 'replace')
CALL WriteVectorInteger(3,1,Tsim,iAggpath(1:Tsim))

OPEN(3, FILE = trim(OutputDir) // 'nwperc.txt', STATUS = 'replace')
CALL WriteVector(3,1,5,nwperc)
OPEN(3, FILE = trim(OutputDir) // 'hnwperc.txt', STATUS = 'replace')
CALL WriteVector(3,1,5,hnwperc)
OPEN(3, FILE = trim(OutputDir) // 'hnwshareperc.txt', STATUS = 'replace')
CALL WriteVector(3,1,5,hnwshareperc)
OPEN(3, FILE = trim(OutputDir) // 'liqperc.txt', STATUS = 'replace')
CALL WriteVector(3,1,5,liqperc)
OPEN(3, FILE = trim(OutputDir) // 'goldmine.txt', STATUS = 'replace')
CALL WriteVector(3,1,7,Goldmine)


OPEN(3, FILE = trim(OutputDir) // 'Econ.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Econ)
OPEN(3, FILE = trim(OutputDir) // 'Eown.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Eown)
OPEN(3, FILE = trim(OutputDir) // 'Eass.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Eass)
OPEN(3, FILE = trim(OutputDir) // 'Emort.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Emort)
OPEN(3, FILE = trim(OutputDir) // 'Ehouse.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Ehouse)
OPEN(3, FILE = trim(OutputDir) // 'Esell.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Esell)
OPEN(3, FILE = trim(OutputDir) // 'Elev.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Elev)
OPEN(3, FILE = trim(OutputDir) // 'Emortpos.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Emortpos)
OPEN(3, FILE = trim(OutputDir) // 'Efore.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Efore)







if(DoSims .eq. 1) then


IF (ALLOCATED(larray)) DEALLOCATE(larray)
ALLOCATE(larray(nsimtrans,tend-tstart+1))

DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%csim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'csimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%hconssim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'hconssimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%csim2(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'csim2TR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)

DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%mpcsim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'mpcsimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)



DO it = 1,tend-tstart+1
   larray(:,it) = Phpath(tstart+it-1)*simtrans(tstart+it-1)%hsim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'hsimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%msim(:)*simtrans(tstart+it-1)%hsim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'msimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%ownsim(:)*1.0d0
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'ownsimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%foresim(:)*1.0d0
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'foresimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%jsim(:)*1.0d0
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'jsimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%ysim(:)+simtrans(tstart+it-1)%psim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'ysimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%bsim(:)
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'bsimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)
DO it = 1,tend-tstart+1
   larray(:,it) = simtrans(tstart+it-1)%sellsim(:)*1.0d0
END DO
OPEN(3, FILE = trim(OutputDirSims) // 'sellsimTR.txt', STATUS = 'replace')
CALL WriteMatrixCSVExpon(3,nsimtrans,tend-tstart+1,larray)


endif 



END SUBROUTINE SaveOutput

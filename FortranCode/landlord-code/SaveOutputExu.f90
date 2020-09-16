SUBROUTINE SaveOutputExu

USE params
USE globals
USE Procedures

integer::it
double precision,allocatable::larray(:,:)

    OPEN(3, FILE = trim(OutputDir) // 'Jwork_exu.txt', STATUS = 'replace');WRITE(3,*) Jwork;CLOSE(3)
    OPEN(3, FILE = trim(OutputDir) // 'Jret_exu.txt', STATUS = 'replace');WRITE(3,*) Jret;CLOSE(3) 
    OPEN(3, FILE = trim(OutputDir)// 'nsim_exu.txt', STATUS = 'replace');WRITE(3,*) nsim;CLOSE(3)

OPEN(3, FILE = trim(OutputDir) // 'goldmine_exu.txt', STATUS = 'replace')
CALL WriteVector(3,1,7,Goldmine)
!    OPEN(3, FILE = trim(OutputDirSims)// 'ysim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jwork,ysim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'psim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,psim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'hsim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,hsim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'hconssim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,hconssim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'bsim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,bsim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'lsim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,lsim)


!    OPEN(3, FILE = trim(OutputDirSims)// 'msim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,msim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'sellsim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,sellsim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'buysim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,buysim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'foresim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,foresim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'csim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSV(3,nsim,Jtot,conssim)

!    OPEN(3, FILE = trim(OutputDirSims)// 'rsim_exu.txt', STATUS = 'replace')
!    CALL WriteMatrixCSVInteger(3,nsim,Jtot,refisim)







OPEN(3, FILE = trim(OutputDir) // 'a0_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa0)

OPEN(3, FILE = trim(OutputDir) // 'a1_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,ngpAgg,ngpAgg,aa1)


OPEN(3, FILE = trim(OutputDir) // 'agepathEage_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEage)

OPEN(3, FILE = trim(OutputDir) // 'agepathEown_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEown)


OPEN(3, FILE = trim(OutputDir) // 'agepathEinc_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEinc)

OPEN(3, FILE = trim(OutputDir) // 'agepathEloc_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEloc)

OPEN(3, FILE = trim(OutputDir) // 'agepathEpen_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEpen)

OPEN(3, FILE = trim(OutputDir) // 'agepathEcon_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEcon)

OPEN(3, FILE = trim(OutputDir) // 'agepathEass_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEass)

OPEN(3, FILE = trim(OutputDir) // 'agepathEmort_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEmort)

OPEN(3, FILE = trim(OutputDir) // 'agepathEhouse_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhouse)

OPEN(3, FILE = trim(OutputDir) // 'agepathEbuy_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEbuy)

OPEN(3, FILE = trim(OutputDir) // 'agepathEsell_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEsell)

OPEN(3, FILE = trim(OutputDir) // 'agepathEmortpos_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEmortpos)

OPEN(3, FILE = trim(OutputDir) // 'agepathEmlpos_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEmlpos)

OPEN(3, FILE = trim(OutputDir) // 'agepathElocpos_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathElocpos)

OPEN(3, FILE = trim(OutputDir) // 'agepathElev_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathElev)

OPEN(3, FILE = trim(OutputDir) // 'agepathEfore_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEfore)

OPEN(3, FILE = trim(OutputDir) // 'agepathEforem_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEforem)

OPEN(3, FILE = trim(OutputDir) // 'agepathErefi_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathErefi)

OPEN(3, FILE = trim(OutputDir) // 'agepathEhnw_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEhnwsim)
OPEN(3, FILE = trim(OutputDir) // 'agepathEdelhnw_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEdelhnwsim)

OPEN(3, FILE = trim(OutputDir) // 'pathPh_exu.txt', STATUS = 'replace')
CALL WriteVector(3,1,Tsim,Phpath(1:Tsim))
OPEN(3, FILE = trim(OutputDir) // 'pathPr_exu.txt', STATUS = 'replace')
CALL WriteVector(3,1,Tsim,Prpath(1:Tsim))
OPEN(3, FILE = trim(OutputDir) // 'pathHt_exu.txt', STATUS = 'replace')
CALL WriteVector(3,1,Tsim,Htpath(1:Tsim))
OPEN(3, FILE = trim(OutputDir) // 'iAggpath_exu.txt', STATUS = 'replace')
CALL WriteVectorInteger(3,1,Tsim,iAggpath(1:Tsim))



OPEN(3, FILE = trim(OutputDir) // 'agepathEconrent_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconrent)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconown_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconown)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconinchigh_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconinchigh)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconinclow_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconinclow)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconhighltv_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconhighltv)

OPEN(3, FILE = trim(OutputDir) // 'agepathEconlowltv_exu.txt', STATUS = 'replace')
CALL WriteMatrix2(3,Tsim,Jtot,agepathEconlowltv)






OPEN(3, FILE = trim(OutputDir) // 'Econ_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Econ)
OPEN(3, FILE = trim(OutputDir) // 'Eown_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Eown)
OPEN(3, FILE = trim(OutputDir) // 'Eass_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Eass)
OPEN(3, FILE = trim(OutputDir) // 'Emort_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Emort)
OPEN(3, FILE = trim(OutputDir) // 'Ehouse_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Ehouse)
OPEN(3, FILE = trim(OutputDir) // 'Esell_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Esell)
OPEN(3, FILE = trim(OutputDir) // 'Elev_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Elev)
OPEN(3, FILE = trim(OutputDir) // 'Emortpos_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Emortpos)
OPEN(3, FILE = trim(OutputDir) // 'Efore_exu.txt', STATUS = 'replace')
CALL WriteMatrix(3,Jtot,1,Efore)




END SUBROUTINE SaveOutputExu

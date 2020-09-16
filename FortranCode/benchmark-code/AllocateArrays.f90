subroutine AllocateArrays

use params
use globals

implicit none


allocate(VownStay(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))



allocate(VownMove(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))

allocate(EEVown(nb,nl,nm,nh,Jwork,ngpExo,ngpPh))
allocate(Vown(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))


allocate(VownRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(EEVownRet(nb,nl,nm,nh,Jret,ngpExo,ngpPh))



allocate(qmret(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(qm(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1))
allocate(qmret_e(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(qm_e(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1))
allocate(qlret(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(ql(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1))

allocate(bankgain_mort(nb,nl,nm,nh,ngpExo,ngpPh))
allocate(bankgain_mort_move(nb,nl,nm,nh,ngpExo,ngpPh))
allocate(bankgain_loc(nb,nl,nm,nh,ngpExo,ngpPh))
allocate(bankgain_loc_move(nb,nl,nm,nh,ngpExo,ngpPh))


allocate(mRpol(nb,ngpExo,ngpPh,Jtot))
allocate(hRpol(nb,ngpExo,ngpPh,Jtot))
allocate(bRpol(nb,ngpExo,ngpPh,Jtot))
allocate(hconsRpol(nb,ngpExo,ngpPh,Jtot))
allocate(consRpol(nb,ngpExo,ngpPh,Jtot))
allocate(Wrent(nb,ngpExo,ngpPh,Jtot))




ALLOCATE(zyrand(nsim,-Jtot:Tsim))
ALLOCATE(eyrand(nsim,-Jtot:Tsim))
ALLOCATE(thetarand(nsim,-Jtot:Tsim))

allocate(psimss(nsimss,Jtot))
allocate(hsimss(nsimss,Jtot))
allocate(msimss(nsimss,Jtot))
allocate(lsimss(nsimss,Jtot))
allocate(bsimss(nsimss,Jtot))
allocate(hconssimss(nsimss,Jtot))
allocate(csimss(nsimss,Jtot))
allocate(beqsimss(nsimss,Jtot))
allocate(hsimIss(nsimss,Jtot))
allocate(lsimIss(nsimss,Jtot))
allocate(buysimss(nsimss,Jtot))
allocate(ownsimss(nsimss,Jtot))
allocate(refisimss(nsimss,Jtot))
allocate(foresimss(nsimss,Jtot))
allocate(sellsimss(nsimss,Jtot))









allocate(Wsell(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(buySpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(consSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hconsSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(bSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(mSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(Wfore(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(consFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hconsFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(bFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(mFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(Wrefin(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(consNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hconsNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(bNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(mNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(lNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(Wpay(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(consPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hconsPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(bPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(hPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(mPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(lPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))

allocate(WsellRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(buySpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(consSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hconsSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(bSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(mSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(WforeRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(consFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hconsFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(bFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(mFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(WrefinRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(consNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hconsNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(bNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(mNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(lNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(WpayRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(consPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hconsPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(bPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(hPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(mPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(lPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!intergers
allocate(mINpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(lINpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(lIPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))

allocate(mINpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(lINpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
allocate(lIPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))








allocate(distsso(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(distssoexu(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(consdisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(dpconsdisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(hconsdisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(hdemdisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(bdisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(buydisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(foredisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(refidisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(selldisto(nbsim,nl,nmsim,nh,ngpW,Jtot))
allocate(dhconsdisto(nbsim,nl,nmsim,nh,ngpW,Jtot))






end subroutine AllocateArrays

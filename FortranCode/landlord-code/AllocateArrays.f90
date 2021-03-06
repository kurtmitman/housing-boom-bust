subroutine AllocateArrays

use params
use globals

implicit none


allocate(VownStay(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(foreOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(sellOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(buyOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(consOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(hconsOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(bOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(mOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(hOwnStayPol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))


allocate(VownMove(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(foreOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(sellOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(buyOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(consOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(hconsOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(bOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(mOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(hOwnMovePol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))

!allocate(EVown(nb,nl,nm,nh,Jwork))
allocate(EEVown(nb,nl,nm,nh,Jwork,ngpExo,ngpPh))
allocate(Vown(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))


allocate(VownRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(EVownRet(nb,nl,nm,nh,Jret))
allocate(EEVownRet(nb,nl,nm,nh,Jret,ngpExo,ngpPh))
!allocate(foreOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(sellOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(buyOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(consOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(hconsOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(bOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(mOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(hOwnPolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))


allocate(qmret(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(qm(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1))
allocate(qmret_e(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(qmret_e_exo(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
allocate(qm_e(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1))
allocate(qm_e_exo(nb,nl,nm,nh,ngpExo,ngpPh,Jwork+1))
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

!allocate(ex_mRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(ex_hRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(ex_bRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(ex_hconsRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(ex_consRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(ex_Wrent(nb,ngpExo,ngpPh,Jtot))


!allocate(base_mRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(base_hRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(base_bRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(base_hconsRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(base_consRpol(nb,ngpExo,ngpPh,Jtot))
!allocate(base_Wrent(nb,ngpExo,ngpPh,Jtot))



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







!allocate(ex_Wsell(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_buySpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_consSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hconsSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_bSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_mSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_Wfore(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_consFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hconsFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_bFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_mFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_Wrefin(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_consNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hconsNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_bNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_mNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_lNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_Wpay(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_consPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hconsPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_bPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_hPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_mPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_lPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))

!allocate(ex_WsellRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_buySpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_consSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hconsSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_bSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_mSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_WforeRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_consFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hconsFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_bFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_mFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_WrefinRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_consNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hconsNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_bNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_mNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_lNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_WpayRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_consPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hconsPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_bPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_hPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_mPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_lPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))


!allocate(ex_mINpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_lINpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(ex_lIPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))

!allocate(ex_mINpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_lINpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(ex_lIPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))





!allocate(base_Wsell(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_buySpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_consSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hconsSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_bSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_mSpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_Wfore(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_consFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hconsFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_bFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_mFpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_Wrefin(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_consNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hconsNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_bNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_mNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_lNpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_Wpay(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_consPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hconsPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_bPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_hPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_mPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_lPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))

!allocate(base_WsellRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_buySpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_consSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hconsSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_bSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_mSpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_WforeRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_consFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hconsFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_bFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_mFpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_WrefinRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_consNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hconsNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_bNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_mNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_lNpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_WpayRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_consPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hconsPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_bPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_hPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_mPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_lPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))

!allocate(base_mINpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_lINpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))
!allocate(base_lIPpolRet(nb,nl,nm,nh,ngpExo,ngpPh,Jret))

!allocate(base_mINpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_lINpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))
!allocate(base_lIPpol(nb,nl,nm,nh,ngpExo,ngpPh,Jwork))



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



!allocate((nb,ngpExo,ngpPh,Jtot))
!allocate((nb,ngpExo,ngpPh,Jtot))
!allocate((nb,ngpExo,ngpPh,Jtot))
!allocate((nb,ngpExo,ngpPh,Jtot))
!allocate((nb,ngpExo,ngpPh,Jtot))
!allocate((nb,ngpExo,ngpPh,Jtot))
!allocate((nb,ngpExo,ngpPh,Jtot))



end subroutine AllocateArrays

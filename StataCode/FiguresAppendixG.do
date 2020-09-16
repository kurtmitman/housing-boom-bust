cd ~/GitHub/housing-boom-bust/scratch
use ~/Dropbox/Housing-Boom-Bust/Benchmark/SimulationPanelDataExp.dta, clear

keep if time<=50
capture drop _merge
merge m:1 time age inc using perminc.dta
drop _merge


merge m:1 age shock using creditscore.dta
drop _merge

global filename="Draft"
global savedirec="~/GitHub/housing-boom-bust/Figures/"

rename house houseval
gen house=houseval/ph

gen equity=houseval-mort

capture drop heloc locmort dllm4
gen heloc=0
replace heloc=-ass if ass<0
gen locmort=heloc+mort

bys id time: keep if _n==1
capture drop d2own

xtset id time
gen buy=0
gen refi=0
gen newmort=0
gen newmortval=0
gen termmort=0
gen termmortval=0
gen purmort=0
gen purmortval=0


sort id time
by id: gen dhouse=abs(house-F.house)
by id: replace buy=1 if dhouse>0.1 & F.house>0 & F.age~=1
by id: replace sell=1 if dhouse>0.1 & house>0 & fore==0 & F.age~=1
by id: replace newmort=1 if (F.mort>=mort | buy==1) & F.mort>0  & F.age~=1
by id: replace newmortval=F.mort if (F.mort>=mort | buy==1) & F.mort>0  & F.age~=1



gen origcredit = creditscore if newmort ==1
by id: carryforward origcredit, gen(neworigcredit)

gen origquint=.
forvalues x=41/48{
xtile origquart`x'=neworigcredit if time==`x', nq(4)
replace origquint=origquart`x' if time==`x'
}



gen incquint=.
gen wagequint=.
gen totincquint=.
forvalues x=43/46{
xtile incquint`x'=inc if time==`x', nq(5)
replace incquint=incquint`x' if time==`x'
drop incquint`x'
xtile wagequint`x'=inc if time==`x' & age>1 & age<=22, nq(5)
replace wagequint=wagequint`x' if time==`x'
drop wagequint`x'
xtile totincquint`x'=inc+0.06*ass if time==`x' & age>1, nq(5)
replace totincquint=totincquint`x' if time==`x'
drop totincquint`x'


}







gen hasmort=0
replace hasmort=1 if mort>0

bys time: egen totmorti=sum(mort)

bys time incquint: egen totmort_iq=sum(mort)
bys time incquint: egen tothasmort_iq=sum(hasmort)
bys time incquint: egen totown_iq=sum(own)
bys time incquint: egen totfore_iq=sum(fore)
gen forerate_iq = totfore_iq/tothasmort_iq
gen mortshare_iq = totmort_iq/totmorti




preserve

bys time: egen totmortti=sum(mort) if age>1
bys time: egen totforeti=sum(fore) if age>1
bys time totincquint: egen totmort_tiq=sum(mort)
bys time totincquint: egen tothasmort_tiq=sum(hasmort)
bys time totincquint: egen totown_tiq=sum(own)
bys time totincquint: egen totfore_tiq=sum(fore)
gen forerate_tiq = totfore_tiq/tothasmort_tiq
gen foreshare_tiq = totfore_tiq/totforeti
gen mortshare_tiq = totmort_tiq/totmortti


bys time totincquint: keep if _n==1

keep forerate_tiq foreshare_tiq mortshare_tiq totmort_tiq totown_tiq tothasmort_tiq totincquint time

reshape wide *_tiq, i(totincquint) j(time)
drop if totincquint ==.
graph bar (asis) mortshare_tiq43 mortshare_tiq46, over(totincquint) ytitle(Share of Debt, size(large)) caption("                   Income Quintile of Household", justification(center) size(large))title(Shares of Mortgage Debt) legend(order(1 "2001" 2 "2007") position(10) ring(0) size(large)) graphregion(color(white)) bgcolor(white)

graph export "${savedirec}ShareDebtByTotIncQ_01_07.eps", as(eps) preview(off) replace
graph export "${savedirec}ShareDebtByTotIncQ_01_07.pdf", as(pdf) replace

graph save Graph "${savedirec}ShareDebtByTotIncQ_01_07.gph", replace



restore





preserve
capture drop *_tiq
capture drop totmorti
bys time: egen totmorti=sum(mort) if age<=22
bys time: egen totforei=sum(fore)

bys time origquint: egen totmort_tiq=sum(mort)
bys time origquint: egen tothasmort_tiq=sum(hasmort)
bys time origquint: egen totown_tiq=sum(own)
bys time origquint: egen totfore_tiq=sum(fore)
gen forerate_tiq = totfore_tiq/tothasmort_tiq
gen mortshare_tiq = totmort_tiq/totmorti
gen foreshare_tiq = totfore_tiq / totforei

bys time origquint: keep if _n==1
gen time2=(time-43)*2+2003
binscatter foreshare_tiq  time2 if time>=43 & time<=51, by(origquint) line(connect) ytitle(Share, size(large)) title(Shares of Foreclosures) legend(order(1 "FICO Q1" 2 "FICO Q2" 3 "FICO Q3" 4 "FICO Q4")  size(large)) graphregion(color(white)) bgcolor(white) xtitle(Year)

graph export "${savedirec}ShareForeByFICO.eps", as(eps) preview(off) replace
graph export "${savedirec}ShareForeByFICO.pdf", as(pdf) replace

graph save Graph "${savedirec}ShareForeByFICO.gph", replace

restore





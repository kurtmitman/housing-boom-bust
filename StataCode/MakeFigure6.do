insheet using "~/Github/housing-boom-bust/scratch/PermInc.csv", comma clear
merge 1:m age shock using "~/Github/housing-boom-bust/scratch/perminc.dta"
drop _merge
merge 1:m time age inc using ~/Dropbox/Housing-Boom-Bust/Benchmark/SimulationPanelDataExp.dta

capture drop _merge
global filename="Draft"
global savedirec="~/GitHub/housing-boom-bust/Figures/"

global timeperiod=48



bys id time: keep if _n==1


xtset id time
global alpha_h=0.6

rename house houseval
gen house=houseval/ph

gen equity=houseval-mort

drop if cons<0.000001

sort id time
by id: gen dlc4=log(cons[_n])-log(cons[_n-2])


capture drop heloc locmort dllm4
gen heloc=0
replace heloc=-ass if ass<0

 

by id: gen d2own=own[_n-2]

by id: gen d2lev8=(houseval[_n-2] - mort[_n-2] - heloc[_n-2]) / (houseval[_n-2] + ass[_n-2] + perminc[_n-2] -mort[_n-2])


su dlc4  if time==$timeperiod & age>2, d
capture drop r_dlc4
gen r_dlc4 = inrange(dlc4, `r(p1)', `r(p99)') if time==$timeperiod & age>2


binscatter dlc4 d2lev8 if time==$timeperiod & r_dlc4,line(none) by(d2own) ytitle(Change in Log Consumption 2007-2011) xtitle(Housing Share of Total Wealth) nq(20) legend(order(1 "Renters" 2 "Owners") position(2) ring(0)) yscale(range(-0.35 0.1))
graph export "${savedirec}NewChangeConsbyWealth_${filename}.eps", as(eps) preview(off) replace
graph export "${savedirec}NewChangeConsbyWealth_${filename}.pdf", as(pdf) replace
graph save "${savedirec}NewChangeConsbyWealth_${filename}.gph", replace










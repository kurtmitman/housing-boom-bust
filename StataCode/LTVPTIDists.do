 use ~/Dropbox/Housing-Boom-Bust/Benchmark/SimulationPanelDataExp.dta, clear
rename house houseval
gen house=houseval/ph
 gen lev = mort/houseval
 keep if time>37 & time<40
 xtset id time
 gen newmort = 0
by id: replace newmort =1 if lev[_n] > lev[_n-1] & age>1 & age<=30


pctile levdist=lev if mort>0 & time==39 & newmort==1, nq(100)

list levdist in 1/100

gen payment = 0.0609*mort
gen pti = payment/inc


su pti if mort>0 & time==39 & newmort==1, d

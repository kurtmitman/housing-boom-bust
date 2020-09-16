 use ~/Dropbox/Housing-Boom-Bust/Benchmark/SimulationPanelData.dta, clear

xtset, clear
xtset id time
drop if cons<0.00001
drop if time<30
sort id time

gen rhoy=0.9409
by id: gen gy=rhoy*(log(inc)-rhoy*log(L.inc))+(rhoy^2)*(log(L.inc)-rhoy*log(L2.inc))+(log(F.inc)-rhoy*log(inc)) if age>2 & age<21
by id: gen dc=log(cons)-log(L.cons)  if age>2 & age<21
by id: gen dy=log(inc)-rhoy*log(L.inc)  if age>2 & age<21

corr gy dc dy, c

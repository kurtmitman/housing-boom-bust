capture log close
log using Table6.log, replace
use ~/Dropbox/Housing-Boom-Bust/Benchmark/SimulationPanelDataExp.dta, clear


keep if time>=20 & time<=60

sort id time


xtset id time

by id: gen owner_occ=F.own
by id: gen owner_occL=L.own
gen rent_to_own=0
gen own_to_rent=0
gen own_to_own=0
gen rent_to_rent=0
by id: replace own_to_own = 1 if own==1 & sell==1 & owner_occ==1
by id: replace hcons=F.hcons if hcons==0
by id: replace rent_to_own = 1 if own==0 & owner_occ ==1
by id: replace rent_to_rent = 1 if own==0 & owner_occ ==0
by id: replace own_to_rent = 1 if own==1 & owner_occ ==0
by id: replace rent_to_rent = 1 if own==0 & owner_occ ==0 & owner_occL==0
by id: gen past_rent = L.hcons if rent_to_own==1 | rent_to_rent==1
by id: gen past_house = L.hcons if own_to_rent==1
gen size_otr = ln(hcons)-ln(past_house) if own_to_rent==1
gen size_rto = ln(hcons)-ln(past_rent) if rent_to_own==1
gen size_rtr = ln(hcons)-ln(past_rent) if rent_to_rent==1
by id: gen size_oto = ln(hcons)-ln(L.hcons) if own==1 & owner_occ==1

su size_rto if time>=30 & time<40 & age>2
su size_rtr if time>=30 & time<40 & age>2
su size_otr if time>=30 & time<40 & age>2
su size_oto if time>=30 & time<40  & size_oto~=0 & age>2

log close


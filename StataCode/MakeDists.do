clear all

global filename="Benchmark"
global direc="~/Dropbox/Housing-Boom-Bust/"
global savedirec="${direc}${filename}"

cd ${savedirec}
global alphavals="0.6"

tempfile Robust

foreach aav of global alphavals{

insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/csimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v cons

save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/hconssimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v hcons

merge 1:1 id time using "`Robust'"
drop _merge
save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/mpcsimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v mpc
*keep if time>370

merge 1:1 id time using "`Robust'"
drop _merge
save "`Robust'", replace



insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/hsimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v house
*keep if time>370

merge 1:1 id time using "`Robust'"
drop _merge
save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/foresimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v fore

merge 1:1 id time using "`Robust'"
drop _merge
save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/jsimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v age

merge 1:1 id time using "`Robust'"

drop _merge
save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/msimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v mort
*keep if time>370

merge 1:1 id time using "`Robust'"

drop _merge
save "`Robust'", replace



insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/sellsimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v sell
*keep if time>430

merge 1:1 id time using "`Robust'"

drop _merge
save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/ownsimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v own

merge 1:1 id time using "`Robust'"

drop _merge
save "`Robust'", replace





insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/bsimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v ass

merge 1:1 id time using "`Robust'"

drop _merge
save "`Robust'", replace



insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/sims/ysimTR.txt", clear
gen id=_n
reshape long v, i(id) j(time)
rename v inc
*keep if time>430

merge 1:1 id time using "`Robust'"

drop _merge
save "`Robust'", replace


insheet using "${direc}${filename}/Results/${filename}_alpha_`aav'/pathPh.txt", clear
gen time=_n
keep if time>=60 
rename v1 ph

replace time=time-59

merge 1:m time using "`Robust'"
drop _merge




}

save ${savedirec}SimulationPanelData, replace


keep if time<250 & time>25

save ${savedirec}SimulationPanelDataExp, replace








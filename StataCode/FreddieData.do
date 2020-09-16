cd ~/GitHub/housing-boom-bust/Data/
insheet using historical_data1_Q11999.txt, delimiter("|") clear
pctile cltv=v9 if v9~=., nq(100)
// List combined LTV percentiles
list cltv in 1/100
destring v10, replace
su v10, d
pctile pti=v10, nq(100)
//List PTI percentiles
list pti in 1/100

# delimit ;
******************************************************
********* CHANGE OF ROOMS AMONG MOVERS ***************
******************************************************;
cd "$data_final_dir";
use housing_data.dta, clear;
keep if int_numb68<=3000;
keep if year <= 1996;

su lrooms_change if owner_to_renter == 1, detail;
local mean_otor = r(mean);
local med_otor = r(p50);

su lrooms_change if renter_to_owner == 1, detail;
local mean_rtoo = r(mean);
local med_rtoo = r(p50);

su lrooms_change if owner_to_owner == 1, detail;
local mean_otoo = r(mean);
local med_otoo = r(p50);

su lrooms_change if renter_to_renter == 1, detail;
local mean_rtor = r(mean);
local med_rtor = r(p50);

capture file close myfile;
file open myfile using "Log-Room Changes 1968 to 1996.txt", text write replace;

file write myfile %9s "Owner to Renter. Mean: " %7.2f (`mean_otor')  %9s ". Median: " %7.2f (`med_otor')  _n;
file write myfile %9s "Renter to Owner. Mean: " %7.2f (`mean_rtoo')  %9s ". Median: " %7.2f (`med_rtoo')  _n;
file write myfile %9s "Owner to Owner. Mean: " %7.2f (`mean_otoo')  %9s ". Median: " %7.2f (`med_otoo')  _n;
file write myfile %9s "Renter to Renter. Mean: " %7.2f (`mean_rtor')  %9s ". Median: " %7.2f (`med_rtor')  _n;

capture file close myfile;


keep if year >= 1976;

su lrooms_change if owner_to_renter == 1, detail;
local mean_otor = r(mean);
local med_otor = r(p50);

su lrooms_change if renter_to_owner == 1, detail;
local mean_rtoo = r(mean);
local med_rtoo = r(p50);

su lrooms_change if owner_to_owner == 1, detail;
local mean_otoo = r(mean);
local med_otoo = r(p50);

su lrooms_change if renter_to_renter == 1, detail;
local mean_rtor = r(mean);
local med_rtor = r(p50);

capture file close myfile;
file open myfile using "Log-Room Changes 1976 to 1996.txt", text write replace;

file write myfile %9s "Owner to Renter. Mean: " %7.2f (`mean_otor')  %9s ". Median: " %7.2f (`med_otor')  _n;
file write myfile %9s "Renter to Owner. Mean: " %7.2f (`mean_rtoo')  %9s ". Median: " %7.2f (`med_rtoo')  _n;
file write myfile %9s "Owner to Owner. Mean: " %7.2f (`mean_otoo')  %9s ". Median: " %7.2f (`med_otoo')  _n;
file write myfile %9s "Renter to Renter. Mean: " %7.2f (`mean_rtor')  %9s ". Median: " %7.2f (`med_rtor')  _n;

capture file close myfile;

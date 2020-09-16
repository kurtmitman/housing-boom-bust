# delimit ;
**************************************************************
********* Change in Demand of Rooms of House Owners **************
**************************************************************;
cd "$data_final_dir";
use housing_data.dta, clear;
tsset id year;
keep if year <= 1996;

capture file close myfile;
file open myfile using "Observations_Cleaning_1968-1996.txt", text write replace;

by id, sort: gen nvals = _n == 1;
count if nvals;
local nn = r(N);
count if head == 1; 
local oo = r(N);
di "Individuals who are heads at least once, with age < 24 or > 63: " `nn';
file write myfile %9s "Individuals who are heads at least once, with age < 24 or > 63 : " %7.0f (`nn')  %9s ". Observations: " %7.0f (`oo')  _n;

keep if to_keep == 1;
drop nvals;
by id, sort: gen nvals = _n == 1;
count if nvals;
local nn = r(N);
count if head == 1; 
local oo = r(N);
di "Keeping only heads with housing information in one period at least: " `nn';
file write myfile %9s "Keeping only heads with housing information in one period at least: " %7.0f (`nn')  %9s ". Observations: " %7.0f (`oo')  _n;


keep if renter_to_renter == 1 | renter_to_owner == 1 | owner_to_owner == 1 | owner_to_renter == 1;
drop nvals;
by id, sort: gen nvals = _n == 1;
count if nvals;
local nn = r(N);
count if head == 1; 
local oo = r(N);
di "Total individuals with transitions: " `nn';
file write myfile %9s "Total individuals with transitions: " %7.0f (`nn')  %9s ". Total transitions: " %7.0f (`oo')  _n;

capture file close myfile;

cd "$data_final_dir";
use housing_data.dta, clear;
tsset id year;
keep if year <= 1996;
keep if year >= 1976;

capture file close myfile;
file open myfile using "Observations_Cleaning_1976-1996.txt", text write replace;
by id, sort: gen nvals = _n == 1;
count if nvals;
local nn = r(N);
count if head == 1; 
local oo = r(N);
di "Individuals who are heads at least once, with age < 24 or > 63: " `nn';
file write myfile %9s "Individuals who are heads at least once, with age < 24 or > 63 : " %7.0f (`nn')  %9s ". Observations: " %7.0f (`oo')  _n;

keep if to_keep == 1;
drop nvals;
by id, sort: gen nvals = _n == 1;
count if nvals;
local nn = r(N);
count if head == 1; 
local oo = r(N);
di "Keeping only heads with housing information in one period at least: " `nn';
file write myfile %9s "Keeping only heads with housing information in one period at least: " %7.0f (`nn')  %9s ". Observations: " %7.0f (`oo')  _n;


keep if renter_to_renter == 1 | renter_to_owner == 1 | owner_to_owner == 1 | owner_to_renter == 1;
drop nvals;
by id, sort: gen nvals = _n == 1;
count if nvals;
local nn = r(N);
count if head == 1; 
local oo = r(N);
di "Total individuals with transitions: " `nn';
file write myfile %9s "Total individuals with transitions: " %7.0f (`nn')  %9s ". Total transitions: " %7.0f (`oo')  _n;

capture file close myfile;

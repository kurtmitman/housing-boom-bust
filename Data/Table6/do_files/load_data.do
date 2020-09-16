# delimit ;

**************************************************************************;
*Read-in data;

*Go to Data Center/Cross-year index/Family Public Data Index

*accessed: Feb 27, 2017: J206827;
***************************************************************************;

cd "$data_dir";
do J223657.do;
do J223657_label_define.do;

compress;
cd "$scratch_dir";
save temp_housingdata_raw.dta, replace; 

***************************************************************************;
*Cross Year File;
*accessed: March 02, 2016: ind2013er;
***************************************************************************;

cd "$data_dir";
do IND2013ER.do;

compress;
cd "$scratch_dir";
save temp_cross_year.dta, replace;

**********************************************************;
*pseudo balanced panel--creates column of years;
**********************************************************;

clear;
set obs 1;
gen id = 1;
forvalues Y = 65(1)113 {;
gen blah`Y' = .;
};
reshape long blah@, i(id) j(year);
drop blah id;
compress;
save temp_pseudo_panel.dta, replace;

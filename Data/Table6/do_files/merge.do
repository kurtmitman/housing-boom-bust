# delimit ;
cd "$scratch_dir";

**********************************************************;
*PART 1: Merge individual level datasets:
**demographic (cross year), housing data;		
**Note: housing data is at household int_numb68 level;
**********************************************************;

*1) cross year file;
use temp_demographics.dta, clear;

*2) Housing data;
save temp.dta, replace;

* Merge using int_number of each year;
foreach Y of numlist 68(1)97 99(2)113 {;
	*local X = `Y' - 1;
	local X = `Y';

	use temp_housing.dta, clear;
	keep int_numb`Y' age_head`Y' sex_head`Y' family_size`Y' house_owner`Y' rooms`Y' house_value`Y' moved`Y';
	keep if int_numb`Y' ~= . & int_numb`Y' ~= 0; *0 is family non-response;
	sort int_numb`Y';

	save temp1.dta, replace;

	use temp.dta, clear;
	sort int_numb`Y';
	merge int_numb`Y' using temp1.dta;
	tab _merge;

	*keep demographics data and matched hours/income data;
	keep if _merge == 1 | _merge == 3;
	drop _merge;

	save temp.dta, replace;

};


*drop irrelevant variables;
drop seq*;

keep int_numb68 per_numb68 
relate68-relate113
age_head68-age_head113
sex_head68-sex_head113
family_size68-family_size113
house_owner68-house_owner113
house_value68-house_value113
moved68-moved113
rooms68-rooms113
years_school
ybirth;
compress;
sort int_numb68 per_numb68;

save temp.dta, replace;
*3) make merged data a panel;
**a) time invariant variables;
use temp.dta, clear;
keep int_numb68 per_numb68 years_school ybirth;
cross using temp_pseudo_panel.dta;
sort int_numb68 per_numb68 year;
save temp1.dta, replace;

use temp.dta, clear;
**b) time varying variables;
keep int_numb68 per_numb68 relate* sex_head* age_head* family_size* house_owner* house_value* moved* rooms*;
reshape long relate@ age_head@ sex_head@ family_size@ house_owner@ house_value@ moved@ rooms@, i(int_numb68 per_numb68) j(year);
sort int_numb68 per_numb68 year;

merge int_numb68 per_numb68 year using temp1.dta;
tab _merge;
drop _merge;
keep if relate != 0;

compress;
save temp_merged.dta, replace;



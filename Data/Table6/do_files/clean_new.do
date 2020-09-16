# delimit ;

cd "$scratch_dir";
use temp_merged.dta, clear;


gen head = 0;
replace head = 1 if relate == 1 & year <= 82;
replace head = 1 if relate == 10 & year > 82;

gen spouse = 0;
replace spouse = 1 if relate == 2 & year <= 82; 
replace spouse = 1 if relate == 20 & year > 82; 
replace spouse = 1 if relate == 22 & year > 82; 
replace spouse = 1 if relate == 88 & year > 82; 
replace spouse = 1 if relate == 90 & year > 82; 

gen child = 0;
replace child = 1 if relate == 3 & year <= 82; 
replace child = 1 if relate == 30 & year > 82; 
replace child = 1 if relate == 33 & year > 82; 
replace child = 1 if relate == 35 & year > 82; 
replace child = 1 if relate == 38 & year > 82;

gen brother_sister = 0;
replace brother_sister = 1 if relate == 4 & year <= 82; 
replace brother_sister = 1 if relate == 40 & year > 82; 
replace brother_sister = 1 if relate == 47 & year > 82; 
replace brother_sister = 1 if relate == 48 & year > 82; 

gen grandchild = 0;
replace grandchild = 1 if relate == 6 & year <= 82; 
replace grandchild = 1 if relate == 60 & year > 82; 

gen parent = 0;
replace parent = 1 if relate == 5 & year <= 82; 
replace parent = 1 if relate == 50 & year > 82; 
replace parent = 1 if relate == 57 & year > 82; 
replace parent = 1 if relate == 58 & year > 82; 

keep if int_numb68<=3000;

egen id = group(int_numb68 per_numb68);
xtset id year;
bys int_numb68 per_numb68: egen min_year = min(year) if !missing(relate);
gen flag_first_year = 0;
replace flag_first_year = 1 if year == min_year;
bys int_numb68 per_numb68: egen max_year = max(year) if !missing(relate);
gen flag_last_year = 0;
replace flag_last_year = 1 if year == max_year;
tsfill, full;

bys id: egen int68 = max(int_numb68);
replace int_numb68 = int68 if missing(int_numb68);
bys id: egen per68 = max(per_numb68);
replace per_numb68 = per68 if missing(per_numb68);
drop int68 per68;

gen l_relate = .;
replace l_relate = 0 if  year <= 97 & !missing(L.relate);
replace l_relate = 0 if  year >=99 & !missing(L2.relate);

replace l_relate = 1 if L.head  == 1 & year <= 97;
replace l_relate = 1 if L2.head == 1 & year >=99;

replace l_relate = 2 if L.spouse  == 1 & year <= 97;
replace l_relate = 2 if L2.spouse == 1 & year >=99;

replace l_relate = 3 if L.child  == 1 & year <= 97;
replace l_relate = 3 if L2.child == 1 & year >=99;

replace l_relate = 4 if L.grandchild  == 1 & year <= 97;
replace l_relate = 4 if L2.grandchild == 1 & year >=99;

replace l_relate = 5 if L.brother_sister  == 1 & year <= 97;
replace l_relate = 5 if L2.brother_sister == 1 & year >=99;

replace l_relate = 6 if L.parent  == 1 & year <= 97;
replace l_relate = 6 if L2.parent == 1 & year >=99;

replace l_relate = 7 if missing(l_relate);
drop spouse child grandchild brother_sister parent;

gen lrel = .;
replace lrel = L.relate 	if  year <= 97;
replace lrel = L2.relate 	if  year >=99;

bys int_numb68 per_numb68: egen ever_head = max(head);
sort int_numb68 per_numb68 year;
keep if ever_head == 1;
drop ever_head;

bys int_numb68 per_numb68: egen count_after97 = count(house_owner) if year>= 97;
*tab cc;
*keep if cc >= 9;

sort id year;
replace house_owner = . if head != 1;
replace rooms 		= . if head != 1;

*collapse (first) per_numb68 family_size age_head sex_head rooms house_owner house_value moved years_school ybirth, by(int_numb68 head year);
gen female 		= 1 if sex_head == 2;
replace female 	= 0 if sex_head == 1;

*drop if age_head == 0; *Creates problem with 1901.175
*drop if age_head == 999; *Creates problem with 1538.1
*drop if age_head < 20;

gen flag_room_capped = 0;
replace flag_room_capped = 1 if rooms == 8 & year<85;
replace flag_room_capped = 1 if rooms == 20 & year>=103;

replace house_owner = 0 if house_owner == 5;
replace house_owner = . if house_owner!= 1 & house_owner!=0;
*drop if house_owner == .;

replace rooms = . if rooms == 0;
replace rooms = . if rooms == 9 & year<85;
replace rooms = . if rooms == 99 & year>=85;
replace rooms = . if rooms == 98 & year >=94;

*MAKE SURE DATA IS CONSISTENT: Otherwise changes are not computed correctly;
*replace rooms = 0 if rooms == .; 
replace house_owner = . if rooms == .;
replace rooms 		= . if house_owner == .;
*drop if rooms == .;

replace family_size = . if family_size == 99;
replace moved = 0 if moved == 5;
replace moved = . if moved >= 8;

*MAKE SURE DATA IS CONSISTENT: Otherwise changes are not computed correctly (e.g., 337.2 or 627.1);
gen to_fix = 0;
replace to_fix = 1 if moved == . & house_owner == 1 & L.house_owner == 1 & year <= 97; *Cannot identify if moved or not;
replace to_fix = 1 if moved == . & house_owner == 1 & L2.house_owner == 1 & year > 97; *Cannot identify if moved or not;
replace to_fix = 1 if moved == . & house_owner == 0 & L.house_owner == 0 & year <= 97; *Cannot identify if moved or not;
replace to_fix = 1 if moved == . & house_owner == 0 & L2.house_owner == 0 & year > 97; *Cannot identify if moved or not;
replace house_owner = . if to_fix == 1;
replace rooms 		= . if to_fix == 1;

gen room_change 		= rooms - L.rooms if year <= 97;
replace room_change 	= rooms - L2.rooms if year >=99;

gen lrooms           	= ln(rooms);
gen lrooms_change 		= lrooms - L.lrooms if year <= 97;
replace lrooms_change 	= lrooms - L2.lrooms if year >=99;

gen rooms_perc_change 		= (rooms - L.rooms)/L.rooms if year <= 97;
replace rooms_perc_change 	= (rooms - L2.rooms)/L2.rooms if year >=99;

gen renter_to_owner 		= 0;
replace renter_to_owner 	= 1 if L.house_owner == 0 & house_owner == 1 & year  <= 97;
replace renter_to_owner 	= 1 if L2.house_owner == 0 & house_owner == 1 & year >=99;

gen renter_to_renter 		= 0;
replace renter_to_renter 	= 1 if L.house_owner == 0 & house_owner == 0 & moved ==1 & year <= 97;
replace renter_to_renter 	= 1 if L2.house_owner == 0 & house_owner == 0 & moved ==1 & year >=99;

gen renter_to_renter_SH  	= 0;
replace renter_to_renter_SH = 1 if L.house_owner == 0 & house_owner == 0 & moved ==0 & year <= 97;
replace renter_to_renter_SH = 1 if L2.house_owner == 0 & house_owner == 0 & moved ==0 & year >=99;

gen renter_to_nothing 		= 0;
replace renter_to_nothing 	= 1 if L.house_owner == 0 & house_owner == . & year  <= 97;
replace renter_to_nothing 	= 1 if L2.house_owner == 0 & house_owner == . & year >=99;

gen owner_to_renter 		= 0;
replace owner_to_renter 	= 1 if L.house_owner == 1 & house_owner == 0 & year  <= 97;
replace owner_to_renter 	= 1 if L2.house_owner == 1 & house_owner == 0 & year >=99;

gen owner_to_owner 			= 0;
replace owner_to_owner 		= 1 if L.house_owner == 1 & house_owner == 1 & moved ==1 & year  <= 97;
replace owner_to_owner 		= 1 if L2.house_owner == 1 & house_owner == 1 & moved ==1 & year >=99;

gen owner_to_owner_SH		= 0;
replace owner_to_owner_SH 	= 1 if L.house_owner == 1 & house_owner == 1 & moved ==0 & year  <= 97;
replace owner_to_owner_SH 	= 1 if L2.house_owner == 1 & house_owner == 1 & moved ==0 & year >=99;

gen owner_to_nothing 		= 0;
replace owner_to_nothing 	= 1 if L.house_owner == 1 & house_owner == . & year  <= 97;
replace owner_to_nothing 	= 1 if L2.house_owner == 1 & house_owner == . & year >=99;

gen nothing_to_owner     = 0;
replace nothing_to_owner = 1 if L.house_owner == .  & house_owner == 1 & year <= 97;
replace nothing_to_owner = 1 if L2.house_owner == . & house_owner == 1 & year >=99;

gen nothing_to_renter     = 0;
replace nothing_to_renter = 1 if L.house_owner == . & house_owner == 0 & year  <= 97;
replace nothing_to_renter = 1 if L2.house_owner == . & house_owner == 0 & year >=99;

gen nothing_to_nothing 		= 0;
replace nothing_to_nothing 	= 1 if L.house_owner == . & house_owner == . & year  <= 97;
replace nothing_to_nothing 	= 1 if L2.house_owner == . & house_owner == . & year >=99;

* Fix information after defining groups;
replace house_owner         = . if renter_to_nothing == 1;
replace rooms               = 0 if renter_to_nothing == 1;
replace lrooms              = . if renter_to_nothing == 1;
replace lrooms_change 	 	= . if renter_to_nothing == 1;
replace room_change      	= - L.rooms if renter_to_nothing == 1 & year  <= 97;
replace room_change      	= - L2.rooms if renter_to_nothing == 1 & year >=99;
replace rooms_perc_change   = -1 if renter_to_nothing == 1;
replace int_numb68          = L.int_numb68 if renter_to_nothing == 1 & year <= 97;
replace int_numb68          = L2.int_numb68 if renter_to_nothing == 1 & year >=99;

replace house_owner         = . if owner_to_nothing == 1;
replace rooms               = 0 if owner_to_nothing == 1;
replace lrooms              = . if owner_to_nothing == 1;
replace room_change      	= - L.rooms if owner_to_nothing == 1 & year  <= 97;
replace room_change      	= - L2.rooms if owner_to_nothing == 1 & year >=99;
replace rooms_perc_change   = -1 if owner_to_nothing == 1;
replace lrooms_change 	 	= . if owner_to_nothing == 1;
replace int_numb68          = L.int_numb68 if owner_to_nothing == 1 & year  <= 97;
replace int_numb68          = L2.int_numb68 if owner_to_nothing == 1 & year >=99;

replace room_change      	= rooms if nothing_to_owner == 1;
replace lrooms_change 		= . if nothing_to_owner == 1;
replace rooms_perc_change   = . if nothing_to_owner == 1;

replace room_change      	= rooms if nothing_to_renter == 1;
replace lrooms_change 	 	= . if nothing_to_renter == 1;
replace rooms_perc_change   = . if nothing_to_renter == 1;

replace house_owner         = . if nothing_to_nothing == 1;
replace rooms               = 0 if nothing_to_nothing == 1;
replace lrooms              = . if nothing_to_nothing == 1;
replace room_change      	= 0 if nothing_to_nothing == 1;
replace rooms_perc_change  	= 0 if nothing_to_nothing == 1;
replace lrooms_change 	 	= 0 if nothing_to_nothing == 1;

*drop if nothing_to_nothing == 1;
*replace house_owner 		= 0 if house_owner == .;
*replace rooms       		= 0 if rooms ==.;
*replace room_change 		= 0 if room_change==.;
*replace rooms_perc_change 	= 0 if rooms_perc_change==.;

drop if year == 98 | year == 100 | year == 102 | year == 104 | year == 106 | year == 108 | year == 110 | year == 112;

gen to_keep = 0;
replace to_keep = 1 if head == 1;
replace to_keep = 1 if owner_to_nothing == 1 & L.head == 1 & year  <= 97; *To keep the heads that leave the sample;
replace to_keep = 1 if owner_to_nothing == 1 & L2.head == 1 & year  > 97; *To keep the heads that leave the sample;
replace to_keep = 1 if renter_to_nothing == 1 & L.head == 1 & year  <= 97; *To keep the heads that leave the sample;
replace to_keep = 1 if renter_to_nothing == 1 & L2.head == 1 & year  > 97; *To keep the heads that leave the sample;
*keep if to_keep == 1;
*drop to_keep;

cd "$data_final_dir";
replace year = year + 1900;
sort int_numb68 per_numb68 year;
*drop if L.house_owner == .  & year < 1999;
*drop if L2.house_owner == . & year >= 1999;
*keep if year >= 1985;
compress;
save housing_data.dta, replace;



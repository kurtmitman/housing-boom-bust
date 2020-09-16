# delimit ;
set more 1;
clear all;

*upper level directory;
global upper_dir   = "XXX"; * Add Master Directory here;
global scratch_dir = "XXX"; *any user created files, not synced online;

global dofiles_dir = "$upper_dir\do_files"; *all dofiles;
global data_dir = "$upper_dir\data"; *all original data;
global paper_dir = "$upper_dir\paper"; *paper directory;
global data_final_dir = "$upper_dir\scratch"; *Final Data to be used in analysis;

*STEP 1: Load all original PSID data;

cd "$dofiles_dir";
do load_data.do;

*STEP 2: Housing;
cd "$dofiles_dir";
do housing_variable_names.do;

*STEP 3: Demographics;
cd "$dofiles_dir";
do demographics.do;

*STEP 4: Merge datasets and create panel;
cd "$dofiles_dir";
do merge.do;

*STEP 5: CLEAN DATA;
cd "$dofiles_dir";
do clean_new.do;

*STEP 6: Analysis;
cd "$dofiles_dir";
do analysis_home.do;




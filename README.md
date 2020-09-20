# housing-boom-bust
 Replication materials for Kaplan, Mitman and Violante (2020): "The Housing Boom and Bust: Model Meets Evidence" published in the Journal of Political Economy
 
 The replication kit is generally divided into three parts:
 
 1.    Code folders: These folders contains the fortran source code for solving and simulating the model.  They also contains matlab code and stata code to generate the plots in the figures in the paper. 
 2.    Data folder: This folder contains the data to replicate the empirical results from the tables in the paper.
 3.    Model Output Folder: This folder contains the output from running the model. Note that this is not stored on GitHub because it contains roughly 22 GiB worth of files. Those files can be downloaded here: https://www.dropbox.com/sh/j981wml5sims1y8/AADnl6PXUIz0kJqlQqmOBo7ga
 
 
 Licenses:
 
 The license for this software is in the “license” document. We have tried to abide by copyright notices and licenses in the use of other’s software. If you find some of the source code is missing the proper notices or is being improperly used, please notify us and we’ll try to rectify the situation. 
 
 Model Replication
 
 Step 1: Fortran Code
 
 
 The model code is written in Fortran90.  All of the files are included in FortranCode directory.  The parameters are set to those from the calibrated model as reported in Table 1.  Instructions for running alternative parameters and policy experiments are included below.
 
 The code was developed on the New York University High Performance Computing Cluster Prince, as well as the Swedish National Infrastructure for Computing (SNIC) high performance clusters Rackham at UPPMAX at Uppsala University and Kebnekaise at HPC2N at Umea University.  The code was compiled with the Intel Fortran 17.0.1 compiler. The code is designed to run in a threaded environment using OpenMP (it can also be run serially).  The program requires a significant amount of memory.  The benchmark code was run on machines with 256 GB of RAM and the Landlord model was run on machines with 3TB of RAM.  The included Makefiles can be updated to match the specifications on your system. Note: the makefile builds one version of the code, so you will either need to copy the makefile into the sub-directory associated with that particular version of the code or edit it so that it finds the source files in that directory.
 
 Benchmark Results
 
 The code in FortranCode/benchmark-code/ is used to generate the results in all of the tables as well as Figures 1-6, 8, 9, 12, F4, and H1.
 
 Some parameters are set in the input file, which must be located in a folder Input/ in the same directory as the executable code. For example, in the folder ModelOuput, there is a folder Benchmark, which contains the model output for the benchmark model. In the folder Input/ is the input folder “input_Benchmark_alpha_0.6.txt” which includes the relevant parameter values for the benchmark model. To execute the code from the command line, one would run the command:
 
 ./ConsHouse input_Benchmark_alpha_0.6.txt
 
 The code will write the output to the folders specified in the first two lines of the input file (in this case, the subdirectories 
 
 Results/Benchmark_alpha_0.6/
 
 and
 
 Results/Benchmark_alpha_0.6/sims/
 
 The output of the program was piped into the file output.out, which includes relevant statistics for the model.
 
 To generate the results for Figure 12 (in the folder ModelOuput/Modify), one needs to change the variable Modify on line 42 of ConsHouse.f90 to take a value of 1.
 
 To generate the results in Figures F4 and H1, one needs to modify the AggGrids.f90 file. For the Houses as ATMs model uncomment lines 309-313 in AggGrids.f90. For the Adjustable Rate Mortgages model uncomment lines 316-320 in AggGrids.f90. For the Movements in the Risk-free Rate model, change DoRf on line 24 of SetParameters.f90 to have a value of 1. Those use the input files in ModelOutput/ATM, ModelOutput/ARM and ModelOutput/RfOnly respectively.
 
 Sensitivities
 
 To generate the results in in Figures 7, 10, 11, F3, F5-F9, F11 one has to run the code in FortranCode/sensitivity-code.
 
 To generate the results in Figure 7 one has to modify SetParameters.f90 lines 18 and 19. Setting NoBankBelief = 1 for the No Bank model (using input files in ModelOutput/NoBank/). Setting OnlyBankBelief = 1 for the Only Bank model (using input files in ModelOutput/OnlyBank/).
 
 To generate the results in Figure F3 one has to modify SetParameters.f90 line 20. Setting NoRentalBelief = 1 for the No Bank model (using input files in ModelOutput/NoRentalBelief/).
 
 To generate the results in Figure 10 one has to modify SetParameters.f90 line 21. Setting beliefshock = 2 for the Land belief model (using input files in ModelOutput/Land/).
 
 To generate the results in Figure F5 one has to modify params.f90 line 29. Setting nh = 7 for the No segmentation model (using input files in ModelOutput/NoSegmentation/).
 
 To generate the results in Figure F6 for the Partial segmentation 1 model use input files in ModelOutput/PartialSegmentation1/).
 
 To generate the results in Figure F7 one has to modify params.f90 line 29. Setting nh = 5 for the Partial segmentation 2 model (using input files in ModelOutput/ PartialSegmentation2/).
 
 To generate the results in Figure F8 one has to modify params.f90 line 29. Setting nh = 4 for the Full segmentation model (using input files in ModelOutput/FullSegmentation/).
 
 To generate the results in Figure F9 one has to modify params.f90 line 29. Setting nh = 4 for the Full segmentation model (using input files in ModelOutput/FullSegmentation/).
 
 To generate the results in Figure F11 one has to modify params.f90 lines 14 and 15. Setting ShortTerm = 1 and NoRent =1 for the Credit Affects house prices model (using input files in ModelOutput/CreditModel/). Also, uncommenting lines 77 and 81 of SetParameters.f90.
 
 Rental Company Sensitivities
 
 To generate the results in Figure 11 one has to run the code in FortranCode/rental-wedge-code using the input files in ModelOutput/Conversion, ModelOutput/Leverage and ModelOutput/SDF respectively.
 
 Landlord Model
 
 To generate the results for the household-landlord model in Figure F10 use the code in FortranCode/landlord-code/ with the input files in ModelOutput/LandlordModel
 
 
 
 Step 2: Matlab Code
 
 1.    To produce figures run MakeMatlabFigures.
 
 Step 3: Stata Code
 
 1.    Run MakeDists.do to generate the simulation panel
 2.    Run Table6.do to generate the model results in Table 6
 3.    Run BuildCreditScoreIncShock.do to generate the creditscore
 4.    Run FiguresAppendixG.do to generate the Figures in Appendix G
 5.    Run MakeFigure6.do to generate the right panel of Figure 6
 6.    Run LTVPTIDists.do to generate the distributions of LTV and PTI at origination
 7.    Run FreddieData.do to to generate LTV and PTI values from the Freddie Mac Data
 8.    Run ComputeBPP.do to compute the BPP coefficient
     
 
 Data Replication
 
 
 American Housing Survey (Table 5):
 
 The data in Table 5 was generated from the 2015 American Housing Survey from the U.S. Census (accessed 16 March 2018). The file can be found in Data, Table5.xls. Size Class 1 corresponds to Less than 500 sq ft to 999 sq ft. Size class 2 is 1,000-1,499 sq ft. Size class 3 is 1,500 to 1,999 sq ft. Size class 4 is 2,000 to 2,499 sq ft. Size class 5 is 2,500 to 2,999 sq ft. Size class 6 is 3,000 to 3,999 sq ft. Size class 7 is 4,000 or more square feet. Units where the square footage is not recorded are dropped from the calculations.
 
 Panel Survey of Income Dynamics (Table 6):
 
 To replicate the values in Table 6 for log-changes in house size in the data, see the README.txt file in the folder Table6. 


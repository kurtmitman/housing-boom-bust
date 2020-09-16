cd ~/Dropbox/Housing-Boom-Bust/Benchmark/Results/Benchmark_alpha_0.6/
import delimited "creditscore.txt", delimiter(space, collapse) encoding(ISO-8859-1)clear
drop v1
rename v2 age
rename v3 shock
rename v4 creditscore
cd ~/Github/housing-boom-bust/scratch
save creditscore.dta, replace






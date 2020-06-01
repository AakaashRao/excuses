mkdir data/working
mkdir output
mkdir output/figures
mkdir output/tables
export PATH=$PATH:/Applications/Stata/StataMP.app/Contents/MacOS
#stata-mp -b do code/cleaning/covid-clean.do &
stata-mp -b do code/cleaning/exp1-clean.do & 
stata-mp -b do code/cleaning/exp2-clean.do &
python3 code/analysis/exp1-motive.py 
python3 code/analysis/purpose-prediction.py
Rscript code/analysis/exp1-analysis.R 
Rscript code/analysis/exp2-analysis.R 
Rscript code/analysis/covid-analysis.R 
python3 code/analysis/exp-demand-text-analysis.py
stata-mp -b do code/analysis/representativeness-tables.do &
stata-mp -b do code/analysis/m-representativeness-table.do &
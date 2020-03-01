mkdir data/working
mkdir output
mkdir output/figures
mkdir output/tables
stata-mp -b do code/exp1-clean.do & 
stata-mp -b do code/exp2-clean.do &
python3 code/exp1-motive.py 
python3 code/purpose-prediction.py
Rscript code/exp1-analysis.R 
Rscript code/exp2-analysis.R 
stata-mp -b do code/representativeness-tables.do &
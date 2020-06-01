* Import the SPSS file to Stata (this step requires Stata 16 or newer)

import spss using "data/raw/covid.sav",  case(lower) clear 

**************************************
********* Start data cleaning  *******
**************************************

* Drop those with missing city

drop if city==""

* Renaming 

rename q43 strong_rep

* Set seed for replication purposes 

set seed 123

* Destring all 

destring *, replace

* Get all variables with value labels

ds, has(vallabel)
local vars `r(varlist)'

* Modify label names 

foreach var of local vars {
	* get the name of the value label for variable `var'
	local labname : value label `var'
	
	* create a copy with name prefix + oldname
	label copy `labname' `var'_lab, replace
	
	* assign that copy to variable `var'
	label value `var' `var'_lab
}


* Drop those with missing observations

drop if responseid==""

* Main outcome 

order support_ban excuse 

gen date = dofc(recordeddate)


* Save the data

saveold "$user\Dropbox\Excuses - Social Image\data\processed\lucid_motivation_covid.dta", replace version(13)

/*

mean support_ban, over(excuse) 
local level=(1 - 2*ttail(e(df_r), 1))*100

ttest support_ban, by(excuse)
local pval = string(r(p),"%9.3f")

coefplot, citop ciopts(recast(rcap)) recast(bar) coeflabel(,labsize(medlarge)) vertical barwidth(0.6) finten(40) fcolor(*.5)  ///
ylab(0(0.1)0.6,gmin gmax) ytitle("Mean {&plusmn} s.e.m.",size(medlarge)) levels(`level') note("N=`=e(N)'. P-value (equal means) = `pval'",size(med))  xlab(1 "No excuse" 2 "Excuse") 


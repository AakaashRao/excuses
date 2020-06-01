************************************
****** Prepare the data set  *******
************************************

// The corresponding Qualtrics file for this data collection is: "Motivation Survey"
* Clear the data frame

clear all

* Set the color scheme 
set scheme plotplainblind, permanent

* Some further options 

global pm = char(177)
set more off
set cformat %5.3f

* Set the current directory

if c(os) == "MacOSX" gl user "/Users/`c(username)'"
else if c(os) == "Windows" gl user "C:\Users\\`c(username)'"
else if c(os) == "Unix" gl user "/usr/`c(username)'"

di "$user 

cd "$user/Dropbox/Excuses - Social Image"


* Get the data

use "data/processed/lucid_motivation_covid.dta", clear 

**************************************
********* Create a figure ************
**************************************


mean support_ban, over(excuse) 
local level=(1 - 2*ttail(e(df_r), 1))*100

ttest support_ban, by(excuse)
local pval = string(r(p),"%9.3f")

coefplot, citop ciopts(recast(rcap)) recast(bar) coeflabel(,labsize(medlarge)) vertical barwidth(0.6) finten(40) fcolor(*.5)  ///
ylab(0(0.1)0.6,gmin gmax) ytitle("Mean {&plusmn} s.e.m.",size(medlarge)) levels(`level') note("N=`=e(N)'. P-value (equal means) = `pval'",size(med))  xlab(1 "No excuse" 2 "Excuse") 
graph export "data/processed/covidfigure.pdf"

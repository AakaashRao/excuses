clear all
set more off

global raw "data/raw"
global data "data/working"
global output "output/tables"

import delimited "$raw/pew-atp-w39.csv", clear 

* age
gen age = .
	replace age = (18+29)/2 if f_agecat == "18-29"
	replace age = (30+49)/2 if f_agecat == "30-49"
	replace age = (50+64)/2 if f_agecat == "50-64"
	replace age = 65 		if f_agecat == "65+"
	
* education 
gen lt_high_school 	  = f_educcat2 == "Less than high school"
gen high_school       = f_educcat2 == "High school graduate"
gen college_no_degree = f_educcat2 == "Some college, no degree"
gen associate 		  = f_educcat2 == "Associate's degree"
gen bachelor 		  = f_educcat2 == "College graduate/some post grad"
gen post_bachelor 	  = f_educcat2 == "Postgraduate"
gen bachelor_more 	  = f_educcat2 == "College graduate/some post grad" | f_educcat2 == "Postgraduate"

foreach v of varlist lt_high_school-bachelor_more {
	replace `v' = . if f_educcat2 == "Don't know/Refused"
}

* gender
gen male = f_sex == "Male"
	replace male = . if f_sex == "Refused"
	
* political party 
gen political_party = . 
	replace political_party = 1 if f_party_final == "Republican"
	replace political_party = 2 if f_party_final == "Democrat"
	replace political_party = 3 if f_party_final == "Independent"
	
* race 
gen hisp = f_hisp == "Yes"
	replace hisp = . if f_hisp == "Refused"
	
gen asian = f_racecmb == "Asian or Asian-American" & f_hisp != "Yes"
	replace asian = . if f_racecmb == "Refused"
	
gen black = f_racecmb == "Black or African American" & f_hisp != "Yes"
	replace black = . if f_racecmb == "Refused"
	
gen white = f_racecmb == "White" & f_hisp != "Yes"
	replace white = . if f_racecmb == "Refused"

gen type = "pew"

keep age-type weight_w39_final

rename weight_w39_final weight

save "$data/pew", replace
	
********************************************************
* DATA PREPARATION - COVID-SAMPLE
********************************************************

use "$raw/covid.dta", clear

* Sample restriction 
	* republican 
	keep if party == 1
	
	* no attritors
	drop if excuse == . | support_ban == . 
	
* Political party
gen political_party = 1 // for republican
	
* Race
gen black = race == 1 if !missing(race)
gen asian = race == 2 if !missing(race)
gen white = race == 3 if !missing(race)
replace white = 0 if hispanic == 1 	
gen hisp = hispanic == 1 if !missing(hispanic)
gen other_race = black != 1 & white != 1 & asian != 1 & hisp != 1 

* Education 
gen lt_high_school 		= education == 1 if !missing(education)
gen high_school 		= education == 2 if !missing(education)
gen college_no_degree 	= education == 3 if !missing(education)
gen associate 			= education == 4 if !missing(education)
gen bachelor 			= education == 5 if !missing(education)
gen post_bachelor 		= education == 6 | edu == 7 | edu == 8 if !missing(education)
gen bachelor_more 		= bachelor == 1 | post_bachelor == 1 

* Gender
gen male = sex == 1 if !missing(sex) 

* Age
decode year, gen(birth_year)
destring birth_year, replace

gen age = 2020 - birth_year

gen type = "covid"

keep age male white black asian hisp other_race lt_high_school high_school ///
	 bachelor post_bachelor bachelor_more type college_no_degree associate political_party
	 
append using "$data/pew"

replace weight = 1 if weight == . & type != "pew"

********************************************************
* SUMMARY STATISTICS
********************************************************
local sumvar age black asian white hisp male college_no_degree bachelor 

local age_label "Age"
local black_label "Black"
local asian_label "Asian"
local white_label "White"
local hisp_label "Hispanic"
local male_label "Male"
local lt_high_school_label "Less than high school"
local high_school_label "High school diploma"
local bachelor_label "Bachelors degree"
local college_no_degree_label "Some college, no degree"
local associate_label "Associate degree"
local post_bachelor_label "Post-bachelor degree"
local bachelor_more_label "Bachelors degree or higher"

foreach var of varlist `sumvar' {
	
	// republican characteristics 

	local list1 "pew covid"
	
	foreach i in `list1' {

		sum `var' [aw = weight] if type == "`i'" & political_party == 1 
		local `var'_`i'_1: di %3.2f `r(mean)'
		local `var'_`i'_1_sd : di %3.2f `r(sd)'
			
		count if type == "`i'" & political_party == 1
		local count_`i'_1 `r(N)'
			
	} 
	
	
	* RECEIVER SAMPLE
	
	cap file close table 
	file open table using "$output/m-representativeness.tex", write replace

	local fwt "file write table"
		
	local caption "Motivating survey: Sample representativeness"
	local cols 2
	local header "\begin{table}[H] \centering \caption{`caption'} \label{t:m-representativeness} \begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular} \end{table}"

	`fwt' "`header'" _n
	`fwt' "& Survey & Pew \\" _n
	`fwt' "\cmidrule(lr){2-2} \cmidrule(lr){3-3} " _n
	`fwt' "Variables: & (1) & (2) \\" _n 
	`fwt' "\midrule" _n
	`fwt' "\midrule" _n

	foreach var in age {
		`fwt' "``var'_label' & ``var'_covid_1' & ``var'_pew_1' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
		
	foreach var in black asian white hisp {
		`fwt' "``var'_label' & ``var'_covid_1' & ``var'_pew_1' \\" _n
	}
	
	`fwt' "\addlinespace" _n 
	
	foreach var in male {
		`fwt' "``var'_label' & ``var'_covid_1' & ``var'_pew_1' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
	
	foreach var in college_no_degree bachelor  {
		`fwt' "``var'_label' & ``var'_covid_1' & ``var'_pew_1' \\" _n
	}
	
	`fwt' "\midrule" _n
	
	`fwt' "Observations  & `count_covid_1' & `count_pew_1'  \\" _n
		

	`fwt' "\bottomrule" _n
	`fwt' "\bottomrule" _n
	`fwt' "\multicolumn{`++cols'}{p{10cm}}{\footnotesize \textit{Notes:} Mean of respondent characteristics in the motivating study and the 2018 Pew Research Center's American Trends Panel Wave 39. Attriters dropped from sample.}" _n
	`fwt' "`footer'" _n
		
	file close table
}

exit, clear

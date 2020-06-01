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
* DATA PREPARATION - RECEIVER
********************************************************

use "$data/exp1.dta", clear

gen black = race == "African American/Black"
gen asian = race == "Asian/Asian American"
replace white = 0 if hisp == 1 
gen other_race = black != 1 & white != 1 & asian != 1 & hisp != 1 

encode education, gen(edu)

gen lt_high_school =    edu == 5 
gen high_school = 		edu == 4
gen college_no_degree = edu == 8
gen associate = 		edu == 1
gen bachelor = 			edu == 2
gen post_bachelor = 	edu == 3 | edu == 6 | edu == 7 
gen bachelor_more =     bachelor == 1 | post_bachelor == 1 

gen type = "receiver"

gen political_party = 2 // = DEMOCRAT 

drop if attrit == 1 

keep age age2 male white black asian hisp other_race lt_high_school high_school ///
	 bachelor post_bachelor bachelor_more political_party type college_no_degree associate
	 
tempfile receiver_sum
save `receiver_sum'

********************************************************
* DATA PREPARATION - SENDER
********************************************************

use "$data/exp2.dta", clear

gen black = race == "African American/Black"
gen asian = race == "Asian/Asian American"
replace white = 0 if hisp == 1 
gen other_race = black != 1 & white != 1 & asian != 1 & hisp != 1 

encode education, gen(edu)

gen lt_high_school =    edu == 5 
gen high_school = 		edu == 4
gen college_no_degree = edu == 8
gen associate = 		edu == 1
gen bachelor = 			edu == 2
gen post_bachelor = 	edu == 3 | edu == 6 | edu == 7 
gen bachelor_more =     bachelor == 1 | post_bachelor == 1 

gen type = "sender"

gen political_party = .
	replace political_party = 1 if partisan >= 2 // = REPUBLICAN
	replace political_party = 3 if partisan <= 1 // = INDEPENDENT
	
drop if attrit == 1 

keep age age2 male white black asian hisp other_race lt_high_school high_school ///
	 bachelor post_bachelor bachelor_more political_party type college_no_degree associate
	 
append using `receiver_sum'
append using "$data/pew"

replace weight = 1 if weight == . & type != "pew"


********************************************************
* SUMMARY STATISTICS
********************************************************
local sumvar age black asian white hisp male bachelor_more // lt_high_school high_school college_no_degree associate bachelor post_bachelor 

local age_label "Age"
*local age2_label "Age Squared"
local black_label "Black"
local asian_label "Asian"
local white_label "White"
*local other_race_label "Other Race"
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
	// republican & indepdent characteristics 
	preserve 
	
	keep if type != "receiver"
	local list1 "pew sender"
	local numlist1 "1 3"
	
	foreach i in `list1' {
		foreach c in `numlist1' {
			sum `var' [aw = weight] if type == "`i'" & political_party == `c' 
			local `var'_`i'_`c': di %3.2f `r(mean)'
			local `var'_`i'_`c'_sd : di %3.2f `r(sd)'
			
			count if type == "`i'" & political_party == `c'
			local count_`i'_`c' `r(N)'
			
		}
	} 
	
	restore
	
	// democratic characteristics
	
	preserve 
	
	keep if type != "sender"
	local list1 "pew receiver"
	
	foreach i in `list1' {
		sum `var' [aw = weight] if type == "`i'" & political_party == 2
		local `var'_`i'_2: di %3.2f `r(mean)'
		local `var'_`i'_2_sd : di %3.2f `r(sd)'
		
		count if type == "`i'" & political_party == 2
		local count_`i'_2 `r(N)'
	} 
	

	restore 
	
	
	* RECEIVER SAMPLE
	
	cap file close table 
	file open table using "$output/t1-representativeness.tex", write replace

	local fwt "file write table"
		
	local caption "Experiment 2: Sample representativeness"
	local cols 2
	local header "\begin{table}[H] \centering \caption{`caption'} \label{t:1-representativeness} \begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular} \end{table}"

	`fwt' "`header'" _n
	`fwt' "& Experiment 2 & Pew \\" _n
	`fwt' "\cmidrule(lr){2-2} \cmidrule(lr){3-3} " _n
	`fwt' "Variables: & (1) & (2) \\" _n 
	`fwt' "\midrule" _n
	`fwt' "\midrule" _n

	foreach var in age {
		`fwt' "``var'_label' & ``var'_receiver_2' & ``var'_pew_2' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
		
	foreach var in black asian white hisp {
		`fwt' "``var'_label' & ``var'_receiver_2' & ``var'_pew_2' \\" _n
	}
	
	`fwt' "\addlinespace" _n 
	
	foreach var in male {
		`fwt' "``var'_label' & ``var'_receiver_2' & ``var'_pew_2' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
	
	foreach var in bachelor_more {
		`fwt' "``var'_label' & ``var'_receiver_2' & ``var'_pew_2' \\" _n
	}
	
	`fwt' "\midrule" _n
	
	`fwt' "Observations  & `count_receiver_2' & `count_pew_2'  \\" _n
		

	`fwt' "\bottomrule" _n
	`fwt' "\bottomrule" _n
	`fwt' "\multicolumn{`++cols'}{p{10cm}}{\footnotesize \textit{Notes:} Mean of respondent characteristics in experiment 2 and the 2018 Pew Research Center's American Trends Panel Wave 39. Attriters dropped from sample.}" _n
	`fwt' "`footer'" _n
		
	file close table
	
	
	
	* SENDER SAMPLE
		
	cap file close table 
	file open table using "$output/t2-representativeness.tex", write replace

	local fwt "file write table"
	
	local caption "Experiment 1: Sample representativeness"
	local cols 2
	local header "\begin{table}[H] \centering \caption{`caption'} \label{t:2-representativeness}  \begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular} \end{table}"

	`fwt' "`header'" _n
	`fwt' "& Experiment 1 & Pew \\" _n
	`fwt' "\cmidrule(lr){2-2} \cmidrule(lr){3-3} " _n
	`fwt' " & (1) & (2) \\" _n 
	`fwt' "\midrule" _n
	`fwt' "\multicolumn{3}{@{}l}{\textbf{Panel A}: \textit{Republican}} \\" _n
	`fwt' "\midrule" _n
	

	foreach var in age {
		`fwt' "``var'_label' & ``var'_sender_1' & ``var'_pew_1' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
		
	foreach var in black asian white hisp {
		`fwt' "``var'_label' & ``var'_sender_1' & ``var'_pew_1' \\" _n
	}
	
	`fwt' "\addlinespace" _n 
	
	foreach var in male {
		`fwt' "``var'_label' & ``var'_sender_1' & ``var'_pew_1' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
	
	foreach var in bachelor_more {
		`fwt' "``var'_label' & ``var'_sender_1' & ``var'_pew_1' \\" _n
	}
	
	`fwt' "\midrule" _n
	
	`fwt' "Observations  & `count_sender_1' & `count_pew_1'  \\" _n
		
		
	`fwt' "\midrule" _n
	`fwt' "\midrule" _n
	`fwt' "\multicolumn{3}{@{}l}{\textbf{Panel B}: \textit{Independent}} \\" _n
	file close table
	
	file open table using "$output/t2-representativeness.tex", write append 
	
	`fwt' "\midrule" _n


	foreach var in age {
		`fwt' "``var'_label' & ``var'_sender_3' & ``var'_pew_3' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
		
	foreach var in black asian white hisp {
		`fwt' "``var'_label' & ``var'_sender_3' & ``var'_pew_3' \\" _n
	}
	
	`fwt' "\addlinespace" _n 
	
	foreach var in male {
		`fwt' "``var'_label' & ``var'_sender_3' & ``var'_pew_3' \\" _n
		}
		
	`fwt' "\addlinespace" _n 
	
	foreach var in bachelor_more {
		`fwt' "``var'_label' & ``var'_sender_3' & ``var'_pew_3' \\" _n
	}
	
	`fwt' "\midrule" _n
	
	`fwt' "Observations  & `count_sender_3' & `count_pew_3'  \\" _n
		
	
	`fwt' "\bottomrule" _n
	`fwt' "\bottomrule" _n
	`fwt' "\multicolumn{`++cols'}{p{10cm}}{\footnotesize \textit{Notes:} Mean of respondent characteristics in experiment 1 and the 2018 Pew Research Center's American Trends Panel Wave 39. Attriters dropped from sample.}" _n
	`fwt' "`footer'" _n
		
	file close table
	
	}	
exit,clear
	

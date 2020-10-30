use "data/raw/dictator-deidentified", clear 

* Drop pre-randomization attritters and flag post-randomization attriters
gen attrit = donation=="" | missing(donation)
lab var attrit "Respondent attritted post-randomization"

* Attention check and consent
gen consent2 = consent=="Yes"
drop consent
rename consent2 consent
lab var consent "Consented to survey"
gen correct_attentioncheck = attention=="Extremely interested,Not at all interested"
drop attention
lab var correct_attention "Correctly answered attention check"

* Generate outcome and treatment variables
gen donated = strpos(donation, "Yes") > 0
lab var donated "Donated to Fund the Wall"
drop donation

gen gullible = condition=="gullible"
lab var gullible "Gullible condition"
gen intolerant = condition=="intolerant"
lab var intolerant "Intolerant condition"
drop condition

* Generate demographic and control variables
gen rep = party=="Republican"
lab var rep "Republican"
drop party

gen partisan = 0
replace partisan = -2 if democratstrength == "Weakly support"
replace partisan = -3 if democratstrength == "Strongly support"
label define partisanvalues -2 "Weak Dem" -3 "Strong Dem"
label values partisan partisanvalues
lab var partisan "Partisan affiliation"
drop democratstrength

destring year, replace
gen age = 2020-year
lab var age "Age"
drop year

gen age2 = age^2
lab var age2 "Age squared"
gen hisp = hispanic=="Yes"
lab var hisp "Spanish, Hispanic, or Latino"
drop hispanic
gen male = sex == "Male"
lab var male "Male"
drop sex

lab var education "Education level"

gen white = race=="Caucasian/White"
lab var white "White"
lab var race "Race"

* Remaining labels

lab var purpose "If you had to guess, what would you say is the purpose of this study?"
lab var feedback "If you have any feedback on our survey, please leave it below."

lab var startdate "Survey start date"
lab var enddate "Survey end date"
lab var responseid "Response ID"

save "data/working/dictator.dta", replace
exit,clear

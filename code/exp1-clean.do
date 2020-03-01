use "data/raw/exp1-pilot-deidentified.dta", replace
gen main=0
append using "data/raw/exp1-mainexp-deidentified.dta"
replace main=1 if missing(main)
lab var main "Part of main experiment (1=main exp, 0=pilot)"

* Drop pre-randomization attritters and flag post-randomization attriters
drop if !inlist(condition, "excuse", "noexcuse")
gen attrit = (gullibility_score=="" & cultural_tolerance=="") | (missing(gullibility_score) & missing(cultural_tolerance))
lab var attrit "Respondent attritted post-randomization"

* Respondents who claim to have taken a previous online survey mentioning Lott's study
gen previous_lott = 0
replace previous_lott = previous=="Yes"
lab var previous_lott "Reports taking a previous online survey mentioning Lott's study"
drop previous

* Attention check and consent
gen first_consent = consent=="Yes"
drop consent
lab var first_consent "Consented on first consent screen"
gen correct_attentioncheck = attention=="Extremely interested,Not at all interested"
drop attention
lab var correct_attention "Correctly answered attention check"

gen excuse = condition=="excuse"
lab var excuse "Excuse condition"
gen noexcuse = condition=="noexcuse"
lab var noexcuse "No excuse condition"
drop condition

gen cultural_score = substr(cultural_tolerance, 15, 2)
destring cultural_score, replace
replace cultural_score = cultural_score+5
lab var cultural_score "Guess about partner's score of cultural tolerance (midpoint of range)"
drop cultural_tolerance

gen gullibility_score = substr(gullibility_score10, 15, 2)
destring gullibility_score, replace
replace gullibility_score = gullibility_score+5
lab var gullibility_score "Guess about partner's score of gullibility (midpoint of range)"
drop gullibility_score10

* Generate demographic and control variables
drop party

gen partisan = 0
replace partisan = -2 if democratstrength == "Strongly support"
replace partisan = -1 if democratstrength == "Weakly support"
label define partisanvalues -2 "Strong Dem" -1 "Weak Dem" 
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
lab var bias "Bias condition"
lab var gullibility "Gullibility condition"
lab var openendedmotives "If you had to guess, why did your matched respondent donate to Fund the Wall?"
lab var previous_lott "Reports taking a previous online survey mentioning Lott's study"

*lab var purpose "If you had to guess, what would you say is the purpose of this study?"
lab var feedback "If you have any feedback on our survey, please leave it below."

lab var startdate "Survey start date"
lab var enddate "Survey end date"
lab var responseid "Response ID"

save "data/working/exp1.dta", replace
exit, clear

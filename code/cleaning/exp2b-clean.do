use "data/raw/exp2b-deidentified", clear 

* Drop pre-randomization attritters and flag post-randomization attriters
drop if !inlist(condition, "excuse", "noexcuse")
gen attrit = donate=="" | missing(donate)
lab var attrit "Respondent attritted post-randomization"

* Drop people with missing city (pre-registered)
drop if city==""

* Respondents who claim to have taken a previous online survey mentioning Lott's study
gen previous_lott = previous=="Yes"
lab var previous_lott "Reports taking a previous online survey mentioning Lott's study"
drop previous

* Attention check and consent
gen first_consent = consent=="Yes"
drop consent
lab var first_consent "Consented on first consent screen"
gen second_consent = reconsent=="Yes"
drop reconsent
lab var second_consent "Consented on second consent screen"
gen correct_attentioncheck = attention=="Extremely interested,Not at all interested"
drop attention
lab var correct_attention "Correctly answered attention check"

* Generate outcome and treatment variables
gen donated = strpos(donate, "Yes") > 0
lab var donated "Donated to Fund the Wall"
drop donate

gen excuse = condition=="excuse"
lab var excuse "Excuse condition"
gen noexcuse = condition=="noexcuse"
lab var noexcuse "No excuse condition"
drop condition
gen control = 0
lab var control "Control condition"

* Generate demographic and control variables
gen rep = party=="Republican"
lab var rep "Republican"
drop party

gen partisan = 0
replace partisan = -1 if partylean == "Lean toward the Democratic Party"
replace partisan = 1 if partylean == "Lean toward the Republican Party"
replace partisan = 2 if republicanstrength == "Weakly support"
replace partisan = 3 if republicanstrength == "Strongly support"
label define partisanvalues -1 "Dem-leaning Ind" 1 "Rep-leaning Ind" 2 "Weak Rep" 3 "Strong Rep"
label values partisan partisanvalues
lab var partisan "Partisan affiliation"
drop partylean republicanstrength

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

gen main = 0
lab var main "Main experiment"
gen replication = 1
lab var replication "Replication experiment"

* Remaining labels

lab var lott_published "Will Lott's study will be widely discussed when published"

lab var purpose "If you had to guess, what would you say is the purpose of this study?"
lab var feedback "If you have any feedback on our survey, please leave it below."
lab var city "City of respondent's IP address (displayed throughout survey)"

lab var startdate "Survey start date"
lab var enddate "Survey end date"
lab var responseid "Response ID"


save "data/working/exp2b.dta", replace
exit,clear

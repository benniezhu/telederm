log using "F:/projects/telederm/output/summary_fyc_analysis_12_24_2024.log", replace 

use "F:/projects/telederm/data/ml_ready/derm_summary_FYC.dta", clear 

keep if total_visits != .
svyset VARPSU [pweight = perwt], strata(VARSTR) vce(linearized) singleunit(certainty)

*make variable labels

label define insurance_lbl 1 "private" 2 "medicaid" 3 "medicare" 4 "medicare advantage" 5 "uninsured 1 month" 6 "other"
label define sex_lbl 1 "female" 2 "male" 
label define poverty_lbl 1 "<100% FPL" 2 "100%-200% FPL" 3 "200%-400% FPL" 4 ">400% FPL"
label define region_lbl 1 "Northeast" 2 "Midwest" 3 "South" 4 "West"
label define race_lbl 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
label define edu_lbl 1 "<HS" 2 "HS Diploma" 3 "Bachelor or more"


label values insurance insurance_lbl
label values SEX sex_lbl
label values poverty poverty_lbl
label values region region_lbl 
label values edu edu_lbl
label values race race_lbl


gen telemed = 0
replace telemed = 1 if total_telehealth_visits > 0

gen age2 = age * age 

gen age40 = 0
replace age40 = 1 if age >= 40

svy: mean total_visits total_telehealth_visits oop total_spend acne_dx acne_dx eczema_dx psoriasis_dx hair_loss_dx seb_dermatitis_dx pruritus_dx unspecified_dermatitis_dx dermatitis_dx nail_dx pigmentation_dx atrophic_dx rash_dx i.SEX i.race i.insurance i.edu i.region i.poverty charlson_score 

svy: mean $idvars

*svy: logit telemed age age2 i.year i.race i.insurance i.region i.poverty , or

*svy: logit telemed age age2 i.year i.race i.insurance i.region i.poverty acne_dx eczema_dx psoriasis_dx hair_loss_dx seb_dermatitis_dx pruritus_dx unspecified_dermatitis_dx nail_dx pigmentation_dx atrophic_dx rash_dx, or

global id_vars "age age2 i.year i.SEX i.race i.edu i.region i.poverty i.insurance i.acne_dx i.psoriasis_dx i.hair_loss_dx i.pruritus_dx i.dermatitis_dx charlson_score"

* list of covaritates that eddie reccomended 

* acne, eczema and psoriasis are well suited from both a patient and dermatologist perspective. Skin exams, examination of pigmented lesions and hair loss are conditions that are

svy: logit telemed age40  i.year i.SEX i.race i.edu i.region i.poverty acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx charlson_score

svy: logit telemed $id_vars , or

svy: twopm tele_oop $id_vars ,  f(probit) s(glm, link(log) family(gamma))

margins insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx 
margins, dydx(year insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx charlson_score)

svy: twopm tele_total_spend $id_vars ,  f(probit) s(glm, link(log) family(gamma))
margins insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx 
margins, dydx(year insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx charlson_score)

svy: glm tele_oop $id_vars , family(gamma) link(log)

margins insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx
margins, dydx(year insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx charlson_score)


svy: glm tele_total_spend $id_vars , family(gamma) link(log)

margins insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx 
margins, dydx(year insurance edu poverty race SEX region acne_dx psoriasis_dx hair_loss_dx pruritus_dx dermatitis_dx charlson_score)

*table 1 code 

global vars "total_visits total_telehealth_visits oop total_spend tele_oop tele_total_spend acne_dx acne_dx eczema_dx psoriasis_dx hair_loss_dx seb_dermatitis_dx pruritus_dx unspecified_dermatitis_dx dermatitis_dx nail_dx pigmentation_dx atrophic_dx rash_dx age i.SEX i.race i.insurance i.edu i.region i.poverty charlson_score "

eststo: svy: mean $vars 

eststo: svy: mean $vars if telemed == 0

eststo: svy: mean $vars if telemed == 1

esttab using "F:\projects\telederm\output\table1.rtf" , b(2) ci(2) label replace nostar 


eststo: svy: mean $percentage_vars 

eststo: svy: mean $percentage_vars if telemed == 0

eststo: svy: mean $percentage_vars if telemed == 1

global percentage_vars "acne_dx acne_dx eczema_dx psoriasis_dx hair_loss_dx seb_dermatitis_dx pruritus_dx unspecified_dermatitis_dx dermatitis_dx nail_dx pigmentation_dx atrophic_dx rash_dx i.SEX i.race i.insurance i.edu i.region i.poverty"


log close 

/*
margins insurance edu poverty race, vce(uncond) noestimcheck
margins, dydx(race insurance edu poverty) vce(uncond) noestimcheck
margins insurance edu poverty race, vce(uncond) noestimcheck
margins, dydx(race insurance edu poverty) vce(uncond) noestimcheck
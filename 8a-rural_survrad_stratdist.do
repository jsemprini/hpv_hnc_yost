***Descriptive Analysis***

clear all 

cd "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\results\update"

set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" { 
	log close 
	}
log using Semprini_ASCO5, replace text


use "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\stata_data\cleaned_data.dta"

order year site malignant hpv_pos1 hpv_pos2 allrural allurban mostrur mosturb rural catrural ses1 ses2 ses3 ses4 ses5 catses rurses male age2 age31 age32 age0024 age2544 age4564 age6584 age85 hisp nh_aian nh_api nh_b nh_unk nh_w medicaid uninsured p_insured hosp_clinic married stage_1 stage_2 stage_3 stage_4 stage_5 stage_6 stage_7 stage_8 insitu localized regional distant mo_to_trt2 y_chemo y_rad mo_trt_same mo_trt_next mo_trt_sec mo_trt_3plus alive dead die_oc despair surv2 surv_yr_one surv_yr_two surv_yr_five auto_dc_only

drop if auto_dc_only==1

global hpv_strata hpv_pos1 hpv_pos2

global census_strata ses1 ses2 ses3 ses4 ses5

tab catses, missing

gen flag_ses=1 if catses==""

tab distant if flag_ses==1

rename rurses rur4ses5

global prime_strata rurses

gen stringrur="rural" if rural==1
replace stringrur="urban" if rural==0

gen rur2ses5=stringrur+"_"+catses

gen flag_r2s5=1 if rur2ses5=="_" | rur2ses5=="rural_" | rur2ses5=="urban_"

global demo_male male 

gen medicare_elig=.
replace medicare_elig=0 if age0024==1 | age2544==1 | age4564==1
replace medicare_elig=1 if age65==1 | age85==1

gen not_elig=.
replace not_elig=0 if medicare_elig==1
replace not_elig=1 if medicare_elig==0

global demo_age medicare_elig not_elig

gen old=.
replace old=0 if age2544==1
replace old=1 if age6584==1

global demo_race hisp nh_aian nh_api nh_b nh_unk nh_w 

gen w_b=.
replace w_b=0 if nh_b==1
replace w_b=1 if nh_w==1

gen w_h=.
replace w_h=0 if hisp==1
replace w_h=1 if nh_w==1

gen w_o=.
replace w_o=0 if nh_w==0 & nh_b==0 & hisp==0
replace w_o=1 if nh_w==1
 

global demo_ins medicaid uninsured p_insured 

gen p_not=.
replace p_not=0 if uninsured==0
replace p_not=1 if p_insured==1

gen med_not=.
replace med_not=0 if uninsured==1
replace med_not=1 if medicaid==1

gen p_med=.
replace p_med=0 if medicaid==1
replace p_med=1 if p_insured==1

global demo_other married 

global stage_outcomes insitu localized regional distant

gen mo_trt_2plus=.
replace mo_trt_2plus=0 if mo_trt_same==1 | mo_trt_next==1
replace mo_trt_2plus=1 if mo_trt_sec==1 | mo_trt_3plus==1

global treat_outcomes y_chemo y_rad mo_trt_same mo_trt_next mo_trt_2plus

global mortality_outcomes alive dead die_oc despair 

global survival_outcomes surv_yr_one surv_yr_two surv_yr_five 

global cont_outcomes mo_to_trt2 surv2

drop if flag_ses==1




**********demographic groups**********
**********setup male, rural, married****
gen female=.
replace female=0 if male==1
replace female=1 if male==0

global sex male female

gen urban=.
replace urban=0 if rural==1
replace urban=1 if rural==0

global geo rural urban

gen not_married=.
replace not_married=0 if married==1
replace not_married=1 if married==0

global family married no_married





****geo******



foreach s of numlist 0/1{
foreach n of numlist 1 { 
foreach z of numlist 0/1 {
foreach y in y_rad y_chemo surv_yr_two{


eststo: reg `y' 1.($geo)#1.($census_strata)  if hpv_pos`n'==`z' & distant==`s' , nocons

	***test across ses****

foreach x in $geo {
test 1.`x'#1.ses1 = 1.`x'#1.ses2 = 1.`x'#1.ses3 = 1.`x'#1.ses4 = 1.`x'#1.ses5
estadd scalar joint_`x'p=r(p)

test 1.`x'#1.ses1 = 1.`x'#1.ses2 = 1.`x'#1.ses3 = 1.`x'#1.ses4 = 1.`x'#1.ses5, mtest(b)
estadd scalar mjoint_`x'p=r(p)

test 1.`x'#1.ses1 = 1.`x'#1.ses2 
estadd scalar s12_`x'p=r(p)

test 1.`x'#1.ses1 = 1.`x'#1.ses3
estadd scalar s13_`x'p=r(p)

test 1.`x'#1.ses1 = 1.`x'#1.ses4 
estadd scalar s14_`x'p=r(p)

test 1.`x'#1.ses1 = 1.`x'#1.ses5
estadd scalar s15_`x'p=r(p)
}

*****test within ses****

foreach x of numlist 1/5{

test 1.rural#1.ses`x' = 1.urban#1.ses`x'
estadd scalar ru_s`x'p=r(p)


}

reg `y' 1.($census_strata) if rural==1 & hpv_pos`n'==`z' & distant==`s' , nocons
estimates store rural1

reg `y' 1.($census_strata) if urban==1 & hpv_pos`n'==`z' & distant==`s' , nocons
estimates store urban1


coefplot rural1 urban1, vertical  xtitle("SES Group") ytitle("Pr(`y')") title("HPV `z' HNC (`y' outcomes)", size(small)) recast(bar) barwidth(0.3) fcolor(*.5) ciopts(recast(rcap)) citop nolabels rename(1.ses1 = "1" 1.ses2 = "2" 1.ses3 = "3" 1.ses4 = "4" 1.ses5 = "5")
graph save g`s'_`y'_hpv`n'_`z'.gph, replace
graph export g`s'_`y'_hpv`n'_`z'.pdf, replace


}
}
}


esttab using geo_dis`s'.csv, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_ruralp joint_urbanp mjoint_ruralp mjoint_urbanp s12_ruralp s13_ruralp s14_ruralp s15_ruralp s12_urbanp s13_urbanp s14_urbanp s15_urbanp ru_s1p ru_s2p ru_s3p ru_s4p ru_s5p) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

estimates clear
}



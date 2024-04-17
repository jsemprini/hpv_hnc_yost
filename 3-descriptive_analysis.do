***Descriptive Analysis***

clear all 

set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" { 
	log close 
	}
log using Semprini_ASCO1, replace text


use "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\stata_data\cleaned_data.dta"

order year site malignant hpv_pos1 hpv_pos2 allrural allurban mostrur mosturb rural catrural ses1 ses2 ses3 ses4 ses5 catses rurses male age2 age31 age32 age0024 age2544 age4564 age6584 age85 hisp nh_aian nh_api nh_b nh_unk nh_w medicaid uninsured p_insured hosp_clinic married stage_1 stage_2 stage_3 stage_4 stage_5 stage_6 stage_7 stage_8 insitu localized regional distant mo_to_trt2 y_chemo y_rad mo_trt_same mo_trt_next mo_trt_sec mo_trt_3plus alive dead die_oc despair surv2 surv_yr_one surv_yr_two surv_yr_five auto_dc_only

drop if auto_dc_only==1

global hpv_strata hpv_pos1 hpv_pos2

global census_strata allrural allurban mostrur mosturb rural catrural ses1 ses2 ses3 ses4 ses5 catses

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

global demo_age age0024 age2544 age4564 age6584 age85

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

global treat_outcomes y_chemo y_rad mo_trt_same mo_trt_next mo_trt_sec mo_trt_3plus 

global mortality_outcomes alive dead die_oc despair 

global survival_outcomes surv_yr_one surv_yr_two surv_yr_five 

global cont_outcomes mo_to_trt2 surv2

drop if flag_ses==1

****proportion test - within rural/ses strata****
global prop_demo rural male old nh_w w_b w_h w_o uninsured p_not med_not p_med married
foreach y in $stage_outcomes $treat_outcomes $mortality_outcomes $survival_outcomes{
foreach i in  $prop_demo {
foreach x in $prop_demo { 
foreach z of numlist 0/1 {
foreach n of numlist 1/2 { 

by `x', sort : prtest `y' if hpv_pos`n'==`z' , by(`i')

}
}
}
}
}

*****logit models****

global logit_demo rural male demo_age demo_race demo_ins married
foreach y in $stage_outcomes $treat_outcomes $mortality_outcomes $survival_outcomes{
foreach i in  $prop_demo {
foreach x in rurses { 
foreach z of numlist 0/1 {
foreach n of numlist 1/2 { 

by `x', sort : prtest `y' if hpv_pos`n'==`z' , by(`i')

estimates store `y'_`i'_`x'_`z'_`n'

}
}
}
}
}
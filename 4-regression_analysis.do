***Descriptive Analysis by Linear Probability Regression***

clear all 

cd "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\results\full"

set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" { 
	log close 
	}
log using Semprini_ASCO4, replace text


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

gen mo_trt_2plus=.
replace mo_trt_2plus=0 if mo_trt_same==1 | mo_trt_next==1
replace mo_trt_2plus=1 if mo_trt_sec==1 | mo_trt_3plus==1

global treat_outcomes y_chemo y_rad mo_trt_same mo_trt_next mo_trt_2plus

global mortality_outcomes alive dead die_oc despair 

global survival_outcomes surv_yr_one surv_yr_two surv_yr_five 

global cont_outcomes mo_to_trt2 surv2

drop if flag_ses==1



*****linear probability models****
foreach n of numlist 1/2 { 
foreach z of numlist 0/1 {
foreach y in $stage_outcomes $treat_outcomes $mortality_outcomes $survival_outcomes {


eststo: reg `y' i.($census_strata)  if hpv_pos`n'==`z' , nocons

test 1.ses1 = 1.ses2 = 1.ses3 = 1.ses4 = 1.ses5
estadd scalar joint_p=r(p)

test 1.ses1 = 1.ses2 = 1.ses3 = 1.ses4 = 1.ses5, mtest(b)
estadd scalar mjoint_p=r(p)

test 1.ses1 = 1.ses2 
estadd scalar s12_p=r(p)

test 1.ses1 = 1.ses3
estadd scalar s13_p=r(p)

test 1.ses1 = 1.ses4 
estadd scalar s14_p=r(p)

test 1.ses1 = 1.ses5
estadd scalar s15_p=r(p)


coefplot, vertical xtitle("SES Group") ytitle("Pr(`y')") title("HPV `z' HNC (`y' outcomes)", size(small)) recast(bar) barwidth(0.3) fcolor(*.5) ciopts(recast(rcap)) citop nolabels rename(1.ses1 = "1" 1.ses2 = "2" 1.ses3 = "3" 1.ses4 = "4" 1.ses5 = "5")
graph save `y'_hpv`n'_`z'.gph, replace
graph export `y'_hpv`n'_`z'.pdf, replace
graph export  `y'_hpv`n'_`z'.svg, replace
graph export  `y'_hpv`n'_`z'.png, replace



}
}
esttab using test_hpv`n'`z'.txt, replace b(4) se(3) keep(1.ses*) nostar sca(joint_p  mjoint_p s12_p s13_p s14_p s15_p) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

esttab using test_hpv`n'`z'.csv, replace b(4) se(3) keep(1.ses*) nostar sca(joint_p  mjoint_p s12_p s13_p s14_p s15_p) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))


estimates clear
}

*****add laytext for poster/paper****
*****add coefplot, vertical*****
*****see PoliSci repolication****


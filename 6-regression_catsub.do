***Descriptive Analysis***

clear all 

cd "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\results\cat"

set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" { 
	log close 
	}
log using Semprini_ASCO6, replace text


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

gen age4554=.
replace age4554=0 if age!="45-49 years" & age!="50-54 years"
replace age4554=1 if age=="45-49 years" | age=="50-54 years"

gen age5564=.
replace age5564=0 if age!="55-59 years" & age!="60-64 years"
replace age5564=1 if age=="55-59 years" | age=="60-64 years"

gen age6574=.
replace age6574=0 if age!="65-69 years" & age!="70-74 years"
replace age6574=1 if age=="65-69 years" | age=="70-74 years"

gen age7584=.
replace age7584=0 if age!="75-79 years" & age!="80-84 years"
replace age7584=1 if age=="75-79 years" | age=="80-84 years"


global demo_age age4554 age5564 age6574 age7584

gen old=.
replace old=0 if age2544==1
replace old=1 if age6584==1

gen nh_o=.
replace nh_o=0 if hisp==1 | nh_b==1 | nh_w==1
replace nh_o=1 if nh_aian==1 | nh_api==1 | nh_unk==1
global demo_race nh_w nh_b hisp nh_o


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


*****race********


foreach n of numlist 1/2 { 
foreach z of numlist 0/1 {
foreach y in $stage_outcomes $treat_outcomes $mortality_outcomes $survival_outcomes {


eststo: reg `y' 1.($demo_race)#1.($census_strata)  if hpv_pos`n'==`z' , nocons

	***test across ses****

foreach x in $demo_race {
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

test 1.nh_w#1.ses`x' = 1.nh_b#1.ses`x'
estadd scalar wb_s`x'p=r(p)

test 1.nh_w#1.ses`x' = 1.hisp#1.ses`x'
estadd scalar wh_s`x'p=r(p)

test 1.nh_w#1.ses`x' = 1.nh_o#1.ses`x'
estadd scalar wo_s`x'p=r(p)

test 1.nh_w#1.ses`x' = 1.nh_b#1.ses`x' = 1.hisp#1.ses`x' = 1.nh_o#1.ses`x' 
estadd scalar jtses`x'p=r(p)

test 1.nh_w#1.ses`x' = 1.nh_b#1.ses`x' = 1.hisp#1.ses`x' = 1.nh_o#1.ses`x' , mtest(b)
estadd scalar mjtses`x'p=r(p)







}

reg `y' 1.($census_strata) if nh_w==1 & hpv_pos`n'==`z', nocons
estimates store white1

reg `y' 1.($census_strata) if nh_b==1 & hpv_pos`n'==`z' , nocons
estimates store black1

reg `y' 1.($census_strata) if hisp==1 & hpv_pos`n'==`z', nocons
estimates store hisp1

reg `y' 1.($census_strata) if nh_o==1 & hpv_pos`n'==`z' , nocons
estimates store other1


coefplot white1 black1 hisp1 other1, vertical  xtitle("SES Group") ytitle("Pr(`y')") title("HPV `z' HNC (`y' outcomes)", size(small)) recast(bar) barwidth(0.3) fcolor(*.5) ciopts(recast(rcap)) citop nolabels rename(1.ses1 = "1" 1.ses2 = "2" 1.ses3 = "3" 1.ses4 = "4" 1.ses5 = "5")
graph save r_`y'_hpv`n'_`z'.gph, replace
graph export r_`y'_hpv`n'_`z'.pdf, replace
graph export  r_`y'_hpv`n'_`z'.svg, replace
graph export  r_`y'_hpv`n'_`z'.png, replace





}
}
esttab using race_hpv`n'.txt, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_nh_wp joint_nh_bp mjoint_nh_wp mjoint_nh_bp s12_nh_wp s13_nh_wp s14_nh_wp s15_nh_wp s12_nh_bp s13_nh_bp s14_nh_bp s15_nh_bp joint_hispp joint_nh_op mjoint_hispp mjoint_nh_op s12_hispp s13_hispp s14_hispp s15_hispp s12_nh_op s13_nh_op s14_nh_op s15_nh_op wb_s1p wb_s2p wb_s3p wb_s4p wb_s5p wh_s1p wh_s2p wh_s3p wh_s4p wh_s5p wo_s1p wo_s2p wo_s3p wo_s4p wo_s5p jtses1p jtses2p jtses3p jtses4p mjtses5p mjtses1p mjtses2p mjtses3p mjtses4p mjtses5p ) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))


esttab using race_hpv`n'.csv, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_nh_wp joint_nh_bp mjoint_nh_wp mjoint_nh_bp s12_nh_wp s13_nh_wp s14_nh_wp s15_nh_wp s12_nh_bp s13_nh_bp s14_nh_bp s15_nh_bp joint_hispp joint_nh_op mjoint_hispp mjoint_nh_op s12_hispp s13_hispp s14_hispp s15_hisp s12_nh_op s13_nh_op s14_nh_op s15_nh_op wb_s1p wb_s2p wb_s3p wb_s4p wb_s5p wh_s1p wh_s2p wh_s3p wh_s4p wh_s5p wo_s1p wo_s2p wo_s3p wo_s4p wo_s5p jtses1p jtses2p jtses3p jtses4p mjtses5p mjtses1p mjtses2p mjtses3p mjtses4p mjtses5p ) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

estimates clear
}













****insurance******


estimates clear

foreach n of numlist 1/2 { 
foreach z of numlist 0/1 {
foreach y in $stage_outcomes $treat_outcomes $mortality_outcomes $survival_outcomes {


eststo: reg `y' 1.($demo_ins)#1.($census_strata)  if hpv_pos`n'==`z' , nocons

	***test across ses****

foreach x in $demo_ins {
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

test 1.p_insured#1.ses`x' = 1.uninsured#1.ses`x'
estadd scalar pu_s`x'p=r(p)

test 1.p_insured#1.ses`x' = 1.medicaid#1.ses`x'
estadd scalar pm_s`x'p=r(p)

test 1.medicaid#1.ses`x' = 1.uninsured#1.ses`x'
estadd scalar mu_s`x'p=r(p)


}

reg `y' 1.($census_strata) if medicaid==1 & hpv_pos`n'==`z' , nocons
estimates store medicaid1

reg `y' 1.($census_strata) if uninsured==1  & hpv_pos`n'==`z' , nocons
estimates store uninsured1


reg `y' 1.($census_strata) if p_insured==1 & hpv_pos`n'==`z'  , nocons
estimates store private1


coefplot medicaid1 uninsured1 private1, vertical  xtitle("SES Group") ytitle("Pr(`y')") title("HPV `z' HNC (`y' outcomes)", size(small)) recast(bar) barwidth(0.3) fcolor(*.5) ciopts(recast(rcap)) citop nolabels rename(1.ses1 = "1" 1.ses2 = "2" 1.ses3 = "3" 1.ses4 = "4" 1.ses5 = "5")
graph save i_`y'_hpv`n'_`z'.gph, replace
graph export i_`y'_hpv`n'_`z'.pdf, replace
graph export  i_`y'_hpv`n'_`z'.svg, replace
graph export  i_`y'_hpv`n'_`z'.png, replace



}
}
esttab using ins_hpv`n'.txt, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_p_insuredp joint_medicaidp mjoint_p_insuredp mjoint_medicaidp s12_p_insuredp s13_p_insuredp s14_p_insuredp s15_p_insuredp s12_medicaidp s13_medicaidp s14_medicaidp s15_medicaidp joint_uninsuredp mjoint_uninsuredp s12_medicaidp s13_medicaidp s14_medicaidp s15_medicaidp pu_s1p pu_s2p pu_s3p pu_s4p pu_s5p pm_s1p pm_s2p pm_s3p pm_s4p pm_s5p mu_s1p mu_s2p mu_s3p mu_s4p mu_s5p jtses1p jtses2p jtses3p jtses4p mjtses5p mjtses1p mjtses2p mjtses3p mjtses4p mjtses5p ) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))


esttab using ins_hpv`n'.csv, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_p_insuredp joint_medicaidp mjoint_p_insuredp mjoint_medicaidp s12_p_insuredp s13_p_insuredp s14_p_insuredp s15_p_insuredp s12_medicaidp s13_medicaidp s14_medicaidp s15_medicaidp joint_uninsuredp mjoint_uninsuredp s12_medicaidp s13_medicaidp s14_medicaidp s15_medicaidp pu_s1p pu_s2p pu_s3p pu_s4p pu_s5p pm_s1p pm_s2p pm_s3p pm_s4p pm_s5p mu_s1p mu_s2p mu_s3p mu_s4p mu_s5p jtses1p jtses2p jtses3p jtses4p mjtses5p mjtses1p mjtses2p mjtses3p mjtses4p mjtses5p ) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

estimates clear
}

estimates clear

****age******

keep if age4564==1 | age6584==1



foreach n of numlist 1/2 { 
foreach z of numlist 0/1 {
foreach y in $stage_outcomes $treat_outcomes $mortality_outcomes $survival_outcomes {


eststo: reg `y' 1.($demo_age)#1.($census_strata)  if hpv_pos`n'==`z' , nocons

	***test across ses****

foreach x in $demo_age {
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

test 1.age4554#1.ses`x' = 1.age5564#1.ses`x'
estadd scalar ym_s`x'p=r(p)

test 1.age4554#1.ses`x' = 1.age6574#1.ses`x'
estadd scalar yo_s`x'p=r(p)

test 1.age4554#1.ses`x' = 1.age7584#1.ses`x'
estadd scalar ye_s`x'p=r(p)

test 1.age4554#1.ses`x' = 1.age6574#1.ses`x' = 1.age7584#1.ses`x' 
estadd scalar sj_s`x'p=r(p)

test 1.age4554#1.ses`x' = 1.age6574#1.ses`x' = 1.age7584#1.ses`x' , mtest(b)
estadd scalar msj_s`x'p=r(p)


}

reg `y' 1.($census_strata) if age4554==1 & hpv_pos`n'==`z', nocons
estimates store age4554a

reg `y' 1.($census_strata) if age5564==1 & hpv_pos`n'==`z', nocons
estimates store age5564a

reg `y' 1.($census_strata) if age6574==1 & hpv_pos`n'==`z', nocons
estimates store age6574b

reg `y' 1.($census_strata) if age7584==1 & hpv_pos`n'==`z', nocons
estimates store age7584b

coefplot age4554a age5564a age6574b age7584b, vertical  xtitle("SES Group") ytitle("Pr(`y')") title("HPV `z' HNC (`y' outcomes)", size(small)) recast(bar) barwidth(0.3) fcolor(*.5) ciopts(recast(rcap)) citop nolabels rename(1.ses1 = "1" 1.ses2 = "2" 1.ses3 = "3" 1.ses4 = "4" 1.ses5 = "5")
graph save a_`y'_hpv`n'_`z'.gph, replace
graph export a_`y'_hpv`n'_`z'.pdf, replace
graph export  a_`y'_hpv`n'_`z'.svg, replace
graph export  a`y'_hpv`n'_`z'.png, replace





}
}
esttab using age_hpv`n'.txt, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_age4554p joint_age5564p joint_age6574p joint_age7584p mjoint_age4554p mjoint_age5564p mjoint_age6574p mjoint_age7584p s12_age4554p s13_age4554p s14_age4554p s15_age4554p s12_age5564p s13_age5564p s14_age5564p s15_age5564p  s12_age6574p s13_age6574p s14_age6574p s15_age6574p  s12_age7584p s13_age7584p s14_age7584p s15_age7584p ym_s1p yo_s1p ye_s1p  ym_s2p yo_s2p ye_s2p ym_s3p yo_s3p ye_s3p ym_s4p yo_s4p ye_s4p ym_s5p yo_s5p ye_s5p  sj_s1p msj_s1p sj_s2p msj_s2p sj_s3p msj_s3p sj_s4p msj_s4p sj_s5p msj_s5p) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))


esttab using age_hpv`n'.csv, replace b(4) se(3) keep(*1.ses*) nostar sca(joint_age4554p joint_age5564p joint_age6574p joint_age7584p mjoint_age4554p mjoint_age5564p mjoint_age6574p mjoint_age7584p s12_age4554p s13_age4554p s14_age4554p s15_age4554p s12_age5564p s13_age5564p s14_age5564p s15_age5564p  s12_age6574p s13_age6574p s14_age6574p s15_age6574p  s12_age7584p s13_age7584p s14_age7584p s15_age7584p ym_s1p yo_s1p ye_s1p  ym_s2p yo_s2p ye_s2p ym_s3p yo_s3p ye_s3p ym_s4p yo_s4p ye_s4p ym_s5p yo_s5p ye_s5p  sj_s1p msj_s1p sj_s2p msj_s2p sj_s3p msj_s3p sj_s4p msj_s4p sj_s5p msj_s5p) mgroups("negative" "positive", pattern(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

estimates clear
}








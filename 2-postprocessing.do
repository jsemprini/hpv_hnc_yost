*****clean and code data for analysis****

clear all

use "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\stata_data\processed_data.dta"

order hpvrecode ruc4 yost5 year site behavior sex age raceethn insurance  repsource marital stage1 stage2 stage3 mo2tx chemo radiate surg1 surg2 surg3 surg4 cod1 cod2 surv_mo

foreach i in  cod1 cod2 surv_mo{
	
	tab `i', missing
}

***code hpv***
gen hpv_pos1=.
replace hpv_pos1=0 if hpvrecode=="HPV Negative"
replace hpv_pos1=1 if hpvrecode=="HPV Positive"

gen hpv_pos2=hpv_pos1
replace hpv_pos2=0 if hpvrecode=="Unknown/NA"

***cod rurality***
gen allrural=.
replace allrural=1 if ruc4=="All Rural"
replace allrural=0 if ruc4!="All Rural" & ruc4!="Tract in SEER, no match in Census data" & ruc4!="Unable to Calculate"

gen allurban=. 
replace allurban=1 if ruc4=="All Urban"
replace allurban=0 if ruc4!="All Urban" & ruc4!="Tract in SEER, no match in Census data" & ruc4!="Unable to Calculate"

gen mostrur=.
replace mostrur=1 if ruc4=="Mostly Rural"
replace mostrur=0 if ruc4!="Mostly Rural" & ruc4!="Tract in SEER, no match in Census data" & ruc4!="Unable to Calculate"

gen mosturb=.
replace mosturb=1 if ruc4=="Mostly Urban"
replace mosturb=0 if ruc4!="Mostly Urban" & ruc4!="Tract in SEER, no match in Census data" & ruc4!="Unable to Calculate"

gen rural=.
replace rural=1 if mostrur==1 | allrural==1 
replace rural=0 if mosturb==1 | allurban==1 

gen catrural=ruc4 if ruc4!="Tract in SEER, no match in Census data" & ruc4!="Unable to Calculate"


***code ses***
gen ses1=.
replace ses1=0 if yost5!="Group 1" & yost5!="Missing (unable to calculate)" & yost5!="Tract in SEER, no match in Census data"
replace ses1=1 if yost5=="Group 1" 

gen ses2=.
replace ses2=0 if yost5!="Group 2" & yost5!="Missing (unable to calculate)" & yost5!="Tract in SEER, no match in Census data"
replace ses2=1 if yost5=="Group 2"

gen ses3=.
replace ses3=0 if yost5!="Group 3" & yost5!="Missing (unable to calculate)" & yost5!="Tract in SEER, no match in Census data"
replace ses3=1 if yost5=="Group 3"


gen ses4=.
replace ses4=0 if yost5!="Group 4" & yost5!="Missing (unable to calculate)" & yost5!="Tract in SEER, no match in Census data"
replace ses4=1 if yost5=="Group 4"


gen ses5=.
replace ses5=0 if yost5!="Group 5" & yost5!="Missing (unable to calculate)" & yost5!="Tract in SEER, no match in Census data"
replace ses5=1 if yost5=="Group 5"

gen catses=yost5 if yost5!="Missing (unable to calculate)" & yost5!="Tract in SEER, no match in Census data"




***gen rural-ses****

gen rurses=catrural+"_"+catses if catrural!="" & catses!=""



***behavior***

gen malignant=.
replace malignant=0 if behavior=="In situ"
replace malignant=1 if behavior=="Malignant"

***disparity***
**male**

gen male=.
replace male=0 if sex=="Female"
replace male=1 if sex=="Male"

*age-twenty year cat*
gen age2=age

split age2, parse("-") gen(age3)
replace age31="00" if age31=="00 years"
replace age31="85" if age31=="85+ years"
tab age31
destring(age31), force replace


gen age0024=.
replace age0024=0 if age31>=25
replace age0024=1 if age31<25

gen age2544=.
replace age2544=0 if age31<25 
replace age2544=0 if age31>=45
replace age2544=1 if age31>=25 & age31<45

gen age4564=.
replace age4564=0 if age31<45
replace age4564=0 if age31>=65
replace age4564=1 if age31>=45 & age31<65

gen age6584=.
replace age6584=0 if age31<64
replace age6584=0 if age31>=85
replace age6584=1 if age31>=65 & age31<85

gen age85=.
replace age85=0 if age31<85
replace age85=1 if age31==85

foreach i in age0024 age2544 age4564 age6584 age85{
	
	tab `i' age31, missing
}



*race*

tab raceethn, gen(race_)

rename (race_1 race_2 race_3 race_4 race_5 race_6) (hisp nh_aian nh_api nh_b nh_unk nh_w)

*insurance*
gen medicaid=.
replace medicaid=0 if insurance!="Any Medicaid" & insurance!="Insurance status unknown"
replace medicaid=1 if insurance=="Any Medicaid"

gen uninsured=.
replace uninsured=0 if insurance!="Uninsured" & insurance!="Insurance status unknown" 
replace uninsured=1 if insurance=="Uninsured"

gen p_insured=.
replace p_insured=0 if medicaid==1 | uninsured==1
replace p_insured=1 if insurance=="Insured" | insurance=="Insured/No specifics"

gen auto_dc_only=1 if repsource=="Autopsy only" | repsource=="Death certificate only"

gen hosp_clinic=.
replace hosp_clinic=0 if repsource!="Hospital inpatient/outpatient or clinic"
replace hosp_clinic=1 if repsource=="Hospital inpatient/outpatient or clinic"

gen married=.
replace married=0 if marital!="Married (including common law)"
replace married=1 if marital=="Married (including common law)"

***outcomes***
tab stage2, gen(stage_)

gen insitu=. 
replace insitu=0 if stage2!="In situ"
replace insitu=1 if stage2=="In situ"

gen localized=.
replace localized=0 if stage2!="Localized only"
replace localized=1 if stage2=="Localized only"

gen regional=.
replace regional=0 if insitu==1 | localized==1 | stage2=="Distant site(s)/node(s) involved" | stage2=="Unknown/unstaged/unspecified/DCO" 
replace regional=1 if regional!=0

gen distant=. 
replace distant=0 if stage2!="Distant site(s)/node(s) involved"
replace distant=1 if stage2=="Distant site(s)/node(s) involved"

***correct stage4, mo2tx, chemo, radiate***


rename mo2tx stage4
rename chemo mo_to_trt
rename radiate chemo
rename surg1 radiate

gen mo_to_trt2=mo_to_trt
destring(mo_to_trt2), force replace

tab chemo
tab radiate

**treatment***

gen y_chemo=.
replace y_chemo=0 if chemo=="No/Unknown"
replace y_chemo=1 if chem=="Yes"

gen y_rad=.
replace y_rad=0 if radiate=="Refused (1988+)" | radiate=="Recommended, unknown if administered" | radiate=="None/Unknown"
replace y_rad=1 if y_rad!=0


gen mo_trt_same=.
replace mo_trt_same=0 if mo_to_trt2>0
replace mo_trt_same=1 if mo_to_trt2==0

gen mo_trt_next=.
replace mo_trt_next=0 if mo_to_trt2<1 | mo_to_trt2>1
replace mo_trt_next=1 if mo_to_trt2==1

gen mo_trt_sec=.
replace mo_trt_sec=0 if mo_to_trt2<1 & mo_to_trt2>2
replace mo_trt_sec=1 if mo_to_trt2==2

gen mo_trt_3plus=.
replace mo_trt_3plus=0 if mo_to_trt2<3
replace mo_trt_3plus=1 if mo_to_trt2>=3

***mortality & survival***
gen alive=.
replace alive=0 if cod1!="Alive"
replace alive=1 if cod1=="Alive"

gen dead=.
replace dead=0 if alive==1
replace dead=1 if alive==0

gen die_oc=.
replace die_oc=0 if cod2!="Dead (attributable to this cancer dx)" & cod1!="Esophagus"  &  cod1!="Floor of Mouth"  &  cod1!="Gum and Other Mouth"  &  cod1!="Hypopharynx"  &  cod1!="Larynx"  &  cod1!="Lip"  &  cod1!="Nasopharynx"  &  cod1!="Other Oral Cavity and Pharynx"  &  cod1!="Tongue"  &  cod1!="Tonsil"

replace die_oc=1 if cod2=="Dead (attributable to this cancer dx)" | cod1=="Esophagus" | cod1=="Floor of Mouth" | cod1=="Gum and Other Mouth" | cod1=="Hypopharynx" | cod1=="Larynx" | cod1=="Lip" | cod1=="Nasopharynx" | cod1=="Other Oral Cavity and Pharynx" | cod1=="Tongue" | cod1=="Tonsil"

gen despair=.
replace despair=0 if cod1!="Suicide and Self-Inflicted Injury" & cod1!="Accidents and Adverse Effects" & cod1!="Chronic Liver Disease and Cirrhosis"

replace despair=1 if cod1=="Suicide and Self-Inflicted Injury" | cod1=="Accidents and Adverse Effects" | cod1=="Chronic Liver Disease and Cirrhosis"

tab surv*, missing

gen surv2=surv_mo

destring(surv2), force replace

gen surv_yr_one=.
replace surv_yr_one=0 if surv2<12 & surv2!=.
replace surv_yr_one=1 if surv2>=12 & surv2!=.

gen surv_yr_two=.
replace surv_yr_two=0 if surv2<24 & surv2!=.
replace surv_yr_two=1 if surv2>=24 & surv2!=.

gen surv_yr_five=.
replace surv_yr_five=0 if surv2<60 & surv2!=.
replace surv_yr_five=1 if surv2>=60 & surv2!=.

save "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\stata_data\cleaned_data.dta", replace
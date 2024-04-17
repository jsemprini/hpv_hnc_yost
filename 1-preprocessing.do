****Import & Prep HNC-Census Data for Cleaning****

clear all 

import delimited "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\seer_data\export2.txt"

****strata variables***
rename v1 hpvrecode
rename v2 ruc4
rename v3 ruca2
rename v4 yost3
rename v5 yost5
rename v6 ry3
rename v7 ry5

***trend variables***
rename v8 year
rename v11 site
rename v12 behavior



***disparity variables***
rename v9 age
rename v10 raceethn
rename v27 repsource
rename v28 insurance
rename v29 marital
rename v30 id
rename v31 sex

***outcome variables***
*stage*
rename v13 stage1
rename v14 stage2
rename v15 stage3

*treatment*
rename v16 mo2tx
rename v17 chemo
rename v18 radiate
rename v19 surg1
rename v20 surg2
rename v21 surg3
rename v22 surg4

*mort*
rename v23 cod1
rename v24 cod2
rename v25 surv_mo
rename v26 surv_flag

save "C:\Users\jsemprini\OneDrive - University of Iowa\3-Preliminary\3-ASCO\stata_data\processed_data.dta", replace
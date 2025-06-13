/*

* Do-file: Preparation of data 
* Paper: Democratic values among immigrants in Europe. Socialization and adaptation processes
* Authors: Fabian Gülzau, Marc Helbling and Sandra Morgenstern

*/


clear all 

**************************
* Preparing ESS data 
**************************

* Load ESS data
use  "C:\Users\innom\Desktop\MarcSVRdaten\Dropbox\ESS-Data-Wizard-subset-2023-06-01\ESS-Data-Wizard-subset-2023-06-01.dta"


**Recodings: Demographics
* age
tab agea, m //15-90, missings:71
gen agea2 = agea*agea
* gender
tab gndr, m // 23k male, 25k female, missings: 167
* educ
tab edulvlb, m nol
replace edulvlb=. if edulvlb==5555

*First generation. Respondent not born in country, nor father or mother
gen first_gen=. //because of 271 missings in brncntr variable
recode first_gen (.=0) if brncntr==1
recode first_gen (.=1) if brncntr==2 & (facntr==2 | mocntr==2) // First gen N=4,717 
*All generations: 
gen generati=.
recode generati (.=2) if brncntr==1 & (facntr==2 | mocntr==2)
recode generati (.=1) if brncntr==2 & (facntr==2 | mocntr==2) 
recode generati (.=0) if brncntr==1

*Migration background. Respondent or father or mother not born in country
gen mig_back=.
recode mig_back (.=0) if brncntr==1
recode mig_back (.=1) if brncntr==2 | facntr==2 | mocntr==2
*Muslims
gen muslim=.
recode muslim (.=1) if rlgdnm==6
recode muslim (.=0) if rlgdnm==1 | rlgdnm==2  | rlgdnm==3  | rlgdnm==4  | rlgdnm==5  | rlgdnm==7  | rlgdnm==8  |  rlgdnm==.a // Kontrollgruppe = any other religion (1-7 außer 6) or no religion (.a); misisngs sind somit nur refusal und no answer, da diese trotzdem muslimisch sein könnten

*Religiosity
gen religiosity=(rlgatnd*-1)+7 

** discrimination
tab dscrgrp
gen discri = .
recode discri (.=0) if (dscrgrp==2)
recode discri (.=1) if (dscrgrp==1)

*Time in destination country (in years)
gen timedest=2022-livecnta 
*Time in origin country (in years)
gen timeorig=livecnta - yrbrn 

** Recodings: DV Democracy measures
*Factor analysis
fac cttresa dfprtal fairelc gptpelc grdfinc gvctzpv medcrgv rghmgpr viepol keydec votedir wpestop, pcf
rotate
*Generate index with first factor items
alpha fairelc dfprtal medcrgv rghmgpr cttresa, gen(demo1)
tab demo, m

label var fairelc "Fair Elections"



** Recodings: Heterogenous var

*Importance to live in democracy
gen dem_imp=.
recode dem_imp (.=0) if (implvdm==0) | (implvdm==1) | (implvdm==2) | (implvdm==3) | (implvdm==4)
recode dem_imp (.=1) if (implvdm==5) | (implvdm==6) | (implvdm==7) | (implvdm==8) | (implvdm==9) | (implvdm==10)
*Satisfaction with way democracy works in country of residence
tab stfdem, m // Level 0-10 
gen sat_cntr_dem=.
recode sat_cntr_dem (.=0) if (stfdem==0) | (stfdem==1) | (stfdem==2) | (stfdem==3) | (stfdem==4)
recode sat_cntr_dem (.=1) if (stfdem==5) | (stfdem==6) | (stfdem==7) | (stfdem==8) | (stfdem==9) | (stfdem==10)
*emotionally attached to country of residence
tab atchctr, m // Level 0-10 
gen atach_cntr=.
recode atach_cntr (.=0) if (atchctr==0) | (atchctr==1) | (atchctr==2) | (atchctr==3) | (atchctr==4)
recode atach_cntr (.=1) if (atchctr==5) | (atchctr==6) | (atchctr==7) | (atchctr==8) | (atchctr==9) | (atchctr==10)




** Prep for merge with Vdem data

*Country: Creating iso3 country codes as in vdem
tab brncntr, nol m // 1= yes, if yes no question about birthcountry (cntbrthd)
tab cntbrthd, m // country of birth
tab cntry, m // country survey conducted
destring cntry, gen(cntry_n) //country numeric

kountry cntbrthd, from(iso2c) to(iso3c) // ISO3 

replace _ISO3C_ = "DDR" if cntbrthd=="1000" //1000=DDR
replace _ISO3C_ = "SUN" if cntbrthd=="2000" //2000=USSR
replace _ISO3C_ = "CSK" if cntbrthd=="3000" //3000=Czechoslovakia
replace _ISO3C_ = "YUG" if cntbrthd=="4000" //4000=Yuguslavia 
replace _ISO3C_ = "TLS" if cntbrthd=="5000" //5000=EastTimor
replace _ISO3C_ = "SCG" if cntbrthd=="6000" //6000=SerbiaMontenegro 

tab _ISO3C_ , m

tab first_gen if  !missing(cntbrthd) // First gen N= 4717
tab first_gen if  !missing(_ISO3C_) // First gen N= 3937




*Country for first gen and non-migrants: Creating iso3 country codes as in vdem





*Generate country code variable that corresponds to vdem variable
gen country_text_id=_ISO3C_

* save dataset 
save "C:\Users\innom\Desktop\MarcSVRdaten\1_analysen_feb2024\data_prep\mergeddataset14_16.dta", replace






					
					
**************************
* Merge with V-dem data, by country of origin & year
**************************

/* Merge vdem data with the year of age==14 
and delete people who migrated before the age of 16, i.e. <=15;

--> there will be variations of merging year and cut-off for the appendix*/
gen year = . 
replace year=yrbrn+14 // year = year born + 14 years 
drop if timeorig<=15 //delte if person migrated before the age of 16
									

*Merge ESS and VDEM
merge m:1 country_text_id year using"C:\Users\innom\Desktop\MarcSVRdaten\Dropbox\Vdem-Data\vdem.dta", generate(mergeddataset)
drop if essround==. /*Drop cases that are not in ESS, i.e. mergedataset==2*/ 
drop if country_id==. /*Drop cases that are not in vdem*/

tab first_gen // First gen N= 2,229 --> TEXT
keep if first_gen==1 //first gen : hier werden nocheinmal 12 obs deleted, da diese wohl nur selbst im Ausland geboren sind aber nicht ihre Eltern (Definition von first gen: selbst & eltern)

* save dataset 
save "C:\Users\innom\Desktop\MarcSVRdaten\1_analysen_feb2024\data_prep\mergeddataset14_16.dta", replace





**************************
* Merge with V-indoc data, by country of origin & year
**************************

/*
** now load data / merge data for analysis
clear all
use "C:\Users\innom\Desktop\MarcSVRdaten\Dropbox\ESS-Data_Vdem\ess10_fb.dta"

** load varieties data: merge 
*/

*Merge ESS-VDEM with V-indoc
drop historical_date
merge m:1 country_text_id year using "C:\Users\innom\Desktop\MarcSVRdaten\Dropbox\vindoc_cy_dta\vindoc_cy.dta", generate(mergeddatasetV_ESS)
drop if mergeddatasetV_ESS==1 /*Drop cases that are not in Varieties-data, i.e. mergedataset==2*/ 
drop if mergeddatasetV_ESS==2


** correlations
pwcorr v2x_polyarchy v2x_libdem v2x_partipdem v2xed_ed_dmcon v2xed_ed_ptcon v2xed_ptcon, sig star (.05) //für TEXT


** Recodings: 
*vdem quartiles
tab v2x_polyarchy
gen v2x_polyarchy_4 =.
recode v2x_polyarchy_4 (.=1) if (v2x_polyarchy>0.00) & (v2x_polyarchy<=0.25)
recode v2x_polyarchy_4 (.=2) if (v2x_polyarchy>0.25) & (v2x_polyarchy<=0.50) 
recode v2x_polyarchy_4 (.=3) if (v2x_polyarchy>0.50) & (v2x_polyarchy<=0.75) 
recode v2x_polyarchy_4 (.=4) if (v2x_polyarchy>0.75) & (v2x_polyarchy<=1.00)

*indoc quantiles
tab v2xed_ed_dmcon 
gen v2xed_ed_dmcon_4 =.
recode v2xed_ed_dmcon_4 (.=1) if (v2xed_ed_dmcon>0.00) & (v2xed_ed_dmcon<=0.25)
recode v2xed_ed_dmcon_4 (.=2) if (v2xed_ed_dmcon>0.25) & (v2xed_ed_dmcon<=0.50) 
recode v2xed_ed_dmcon_4 (.=3) if (v2xed_ed_dmcon>0.50) & (v2xed_ed_dmcon<=0.75) 
recode v2xed_ed_dmcon_4 (.=4) if (v2xed_ed_dmcon>0.75) & (v2xed_ed_dmcon<=1.00)



* save dataset in prep-folder
save "C:\Users\innom\Desktop\MarcSVRdaten\1_analysen_feb2024\data_prep\mergeddataset_V_ESS_14_16.dta" , replace 
* save dataset in analysis-folder
save "C:\Users\innom\Desktop\MarcSVRdaten\1_analysen_feb2024\mergeddataset_V_ESS_14_16.dta" , replace








*******************************************************************************

* add native data again
append using "C:\Users\innom\Desktop\MarcSVRdaten\1_analysen_feb2024\data_prep\mergeddataset_natives.dta", generate(newv)
tab newv, m nol
label define newv_ll 1 "Non-Migrant" 0 "First-generation Migrant" 
label values newv newv_ll

tab v2x_polyarchy_4
tab v2x_polyarchy_4, m nol
gen v2x_polyarchy_4nat = v2x_polyarchy_4
recode v2x_polyarchy_4nat (.=0) if (newv==1) 
tab v2x_polyarchy_4nat

label define v2x_polyarchy_4nat_l 0 "Non-Migrant" 1 "Low" 2 "Rather low" 3 "Rather high" 4 "High"
label values v2x_polyarchy_4nat v2x_polyarchy_4nat_l


tab v2xed_ed_dmcon_4
tab v2xed_ed_dmcon_4, m nol
gen v2xed_ed_dmcon_4nat = v2xed_ed_dmcon_4
recode v2xed_ed_dmcon_4nat (.=0) if (newv==1) 
tab v2xed_ed_dmcon_4nat

label define v2xed_ed_dmcon_4nat_l 0 "Non-Migrant" 1 "Low" 2 "Rather low" 3 "Rather high" 4 "High"
label values v2xed_ed_dmcon_4nat v2xed_ed_dmcon_4nat_l


* save dataset in analysis-folder
save "C:\Users\innom\Desktop\MarcSVRdaten\1_analysen_feb2024\mergeddataset_V_ESS_14_16_nativesfirstgen.dta" , replace

















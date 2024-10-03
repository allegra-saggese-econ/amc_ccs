

// Formatting //
******************************************************************************************************************************************
set scheme lean2

graph set window fontface "Arial"

// Defining directories //
******************************************************************************************************************************************
if "`c(username)'"=="ignaciobanaressanchez"{
	global projdir "/Users/ignaciobanaressanchez/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs"
}
else if "`c(username)'"=="ibanares"{
	global projdir "/Users/ibanares/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs"
}
else if "`c(username)'"=="BANARESS"{
	global projdir "C:/Users/BANARESS/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs"
}

* import Orbis data:

import delimited "$projdir/data/orbis/orbis_long.csv", varnames(1) clear

keep applicant_name file_date treat_date

* There are some that get treated in 2023 that do not have a treat_date

sort applicant_name file_date

gen f_year = substr(file_date, 1,4)
gen f_month = substr(file_date, 6,2)

destring f_year, replace force
destring f_month, replace force

gen f_modate=ym(f_year,f_month)
format f_modate %tm

drop if f_modate == . // lets first create the span of the panel and then merge with all data to bring in the zeros

gen patent_filed = 1 

collapse (sum) patent_filed, by(applicant_name f_modate) // total number of patents file in each monthly date for each firm

reshape wide patent_filed, i(applicant_name) j(f_modate)

drop patent_filed266 patent_filed318 patent_filed328 patent_filed360 patent_filed366 patent_filed406 patent_filed411 patent_filed423 patent_filed456 patent_filed458 patent_filed461 patent_filed462 patent_filed468 patent_filed474 patent_filed476 patent_filed477 patent_filed479 patent_filed481 patent_filed486 patent_filed487 patent_filed501 patent_filed505 patent_filed509 patent_filed515 patent_filed522 patent_filed530 patent_filed561


foreach monthdate of numlist 564(1)764 {

gen patent_f_`monthdate' = .
capture replace patent_f_`monthdate' = patent_filed`monthdate'
replace patent_f_`monthdate' = 0 if patent_f_`monthdate' == .

}

drop patent_filed*

* Now we have full panel 

reshape long patent_f_, i(applicant_name) j(f_modate)

gen f_modate_num = f_modate

format f_modate %tm

preserve

collapse (sum) patent_f_ (first) f_modate_num, by(f_modate)

label var patent_f_ "Sum of Applicants' Patents"

twoway line patent_f_ f_modate, 	///
									xlabel(564(10)764, labsize(vsmall) angle(45)) xtitle("") ///
									ylabel(,labsize(small))

gr export "$projdir/output/sum_patents.pdf", replace

restore

save "$projdir/data/temp/applicants_patents_panel", replace

* Generate data with treatment date variable

import delimited "$projdir/data/Google_patents/firm_level_data.csv", varnames(1) clear

keep applicant_name year quarter applied awarded

foreach year of numlist 2020(1)2023 {

gen applied_`year'_Q1 = 0
gen applied_`year'_Q2 = 0
gen applied_`year'_Q3 = 0
gen applied_`year'_Q4 = 0

gen awarded_`year'_Q1 = 0
gen awarded_`year'_Q2 = 0
gen awarded_`year'_Q3 = 0
gen awarded_`year'_Q4 = 0

replace applied_`year'_Q1 = 1 if year == `year' & quarter == 1 & applied == 1 
replace applied_`year'_Q2 = 1 if year == `year' & quarter == 2 & applied == 1
replace applied_`year'_Q3 = 1 if year == `year' & quarter == 3 & applied == 1
replace applied_`year'_Q4 = 1 if year == `year' & quarter == 4 & applied == 1

replace awarded_`year'_Q1 = 1 if year == `year' & quarter == 1 & awarded == 1
replace awarded_`year'_Q2 = 1 if year == `year' & quarter == 2 & awarded == 1
replace awarded_`year'_Q3 = 1 if year == `year' & quarter == 3 & awarded == 1
replace awarded_`year'_Q4 = 1 if year == `year' & quarter == 4 & awarded == 1

}

drop year quarter applied awarded

replace applicant_name = "8 Rivers" if applicant_name == "8 Rivers - Origen"

collapse (sum) applied* awarded*, by (applicant_name)

save "$projdir/data/temp/applicats_treatment", replace

merge 1:m applicant_name using "$projdir/data/temp/applicants_patents_panel.dta"

* Origen is only in the patents data: weird because the list to feed into Google patents comes from applicants dataset

* The _merge ==1 firms are applicants that do not patent.

drop if _merge ==2 

preserve 

keep if _merge == 3

save "$projdir/data/temp/panel_treat_patents_1", replace // first subset - applicants that appear in patent data

restore

keep if _merge == 1 // These are applicants that do not appear in patent data

* Lets create second subset of panel (applicants that never file a patent)

drop f_modate patent_f_ f_modate_num

foreach monthdate of numlist 564(1)764 {

gen patent_f_`monthdate' = 0

}

reshape long patent_f_, i(applicant_name) j(f_modate)

gen f_modate_num = f_modate

format f_modate %tm

save "$projdir/data/temp/panel_treat_patents_2", replace 

append using "$projdir/data/temp/panel_treat_patents_1.dta"

save "$projdir/data/temp/panel_treat_patents_all", replace 

//// Graphical descriptives towards DID

use "$projdir/data/temp/panel_treat_patents_all", clear

egen awarded=rowtotal(awarded*) 
egen applied=rowtotal(applied*) 

preserve

collapse (sum) patent_f_ (first) f_modate_num, by(awarded f_modate)

label var patent_f_ "Sum of Applicants' Patents"

twoway 	(line patent_f_ f_modate if awarded == 0, color(gs5)) ///
		(line patent_f_ f_modate if awarded == 1, color(red)), 	///
									xlabel(564(10)764, labsize(vsmall) angle(45)) xtitle("") ///
									ylabel(,labsize(small)) ///
									legend(pos(6) col(2) size(small) label(1 "Not Awarded (Ever)") label(2 "Awarded in a Round"))
									
gr export "$projdir/output/sum_patents_awarded_notawarded.pdf", replace

restore

preserve

collapse (mean) patent_f_ (first) f_modate_num, by(awarded f_modate)

label var patent_f_ "Average of Applicants' Patents"

twoway 	(line patent_f_ f_modate if awarded == 0, color(gs5)) ///
		(line patent_f_ f_modate if awarded == 1, color(red)), 	///
									xlabel(564(10)764, labsize(vsmall) angle(45)) xtitle("") ///
									ylabel(,labsize(small)) ///
									legend(pos(6) col(2) size(small) label(1 "Not Awarded (Ever)") label(2 "Awarded in a Round"))
									
gr export "$projdir/output/avg_patents_awarded_notawarded.pdf", replace

restore

* Do this with the collapsed sum data: total number of patents for each group

collapse (sum) patent_f_ (first) f_modate_num, by(awarded f_modate)

bysort awarded: g patent_cumul= sum(patent_f_)

twoway 	(line patent_cumul f_modate if awarded == 0, color(gs5)) ///
		(line patent_cumul f_modate if awarded == 1, color(red)), 	///
									xlabel(564(10)764, labsize(vsmall) angle(45)) xtitle("") ///
									ylabel(,labsize(small)) ytitle("Cumulative Patents") ///
									legend(pos(6) col(2) size(small) label(1 "Not Awarded (Ever)") label(2 "Awarded in a Round"))

gr export "$projdir/output/cumul_patents_awarded_notawarded.pdf", replace

* Generate categorical variable for award date

label define award_date_lab 	1 "2020 Q1" 2 "2020 Q2" 3 "2020 Q3" 4 "2020 Q4" ///
								5 "2021 Q1" 6 "2021 Q2" 7 "2021 Q3" 8 "2021 Q4" ///
								9 "2022 Q1" 10 "2022 Q2" 11 "2022 Q3" 12 "2022 Q4" ///
								13 "2023 Q1" 14 "2023 Q2" 15 "2023 Q3" 16 "2023 Q4" 0 "Not Awarded"
gen awarded_mdate = 0

replace awarded_mdate = 1 	if awarded_2020_Q1 == 1
replace awarded_mdate = 2 	if awarded_2020_Q2 == 1
replace awarded_mdate = 3 	if awarded_2020_Q3 == 1
replace awarded_mdate = 4 	if awarded_2020_Q4 == 1

replace awarded_mdate = 5 	if awarded_2021_Q1 == 1
replace awarded_mdate = 6 	if awarded_2021_Q2 == 1
replace awarded_mdate = 7 	if awarded_2021_Q3 == 1
replace awarded_mdate = 8 	if awarded_2021_Q4 == 1

replace awarded_mdate = 9 	if awarded_2022_Q1 == 1
replace awarded_mdate = 10 	if awarded_2022_Q2 == 1
replace awarded_mdate = 11	if awarded_2022_Q3 == 1
replace awarded_mdate = 12	if awarded_2022_Q4 == 1

replace awarded_mdate = 13	if awarded_2023_Q1 == 1
replace awarded_mdate = 14	if awarded_2023_Q2 == 1
replace awarded_mdate = 15	if awarded_2023_Q3 == 1
replace awarded_mdate = 16	if awarded_2023_Q4 == 1

label values awarded_mdate award_date_lab

collapse (sum) patent_f_, by(awarded_mdate f_modate)

label var patent_f_ "Sum of Applicants' Patents (By Award Date)"

twoway 	(line patent_f_ f_modate if awarded_mdate == 1, lcolor("sea") lpattern(solid))  ///	
								,xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(,labsize(small)) ytitle("Sum of Patents 2020 Q1 Awardees") xline(720, lcolor("red"))
								
gr export "$projdir/output/2020_Q1_patents.pdf", replace
								
twoway 	(line patent_f_ f_modate if awarded_mdate == 6, lcolor("sea") lpattern(solid))  ///	
								,xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(,labsize(small)) ytitle("Sum of Patents 2021 Q2 Awardees") xline(735, lcolor("red"))

gr export "$projdir/output/2021_Q2_patents.pdf", replace

twoway 	(line patent_f_ f_modate if awarded_mdate == 8, lcolor("sea") lpattern(solid))  ///	
								,xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(,labsize(small)) ytitle("Sum of Patents 2021 Q4 Awardees") xline(741, lcolor("red"))
								
gr export "$projdir/output/2021_Q4_patents.pdf", replace							

twoway 	(line patent_f_ f_modate if awarded_mdate == 10, lcolor("sea") lpattern(solid))  ///	
								,xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(,labsize(small)) ytitle("Sum of Patents 2022 Q2 Awardees") xline(747, lcolor("red"))

gr export "$projdir/output/2022_Q2_patents.pdf", replace

twoway 	(line patent_f_ f_modate if awarded_mdate == 12, lcolor("sea") lpattern(solid))  ///	
								,xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(,labsize(small)) ytitle("Sum of Patents 2022 Q4 Awardees") xline(753, lcolor("red"))

gr export "$projdir/output/2022_Q4_patents.pdf", replace	
							
twoway 	(line patent_f_ f_modate if awarded_mdate == 15, lcolor("sea") lpattern(solid))  ///	
								,xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(,labsize(small)) ytitle("Sum of Patents 2023 Q3 Awardees") xline(763, lcolor("red"))
								
gr export "$projdir/output/2023_Q3_patents.pdf", replace	


//// DID Analysis

use "$projdir/data/temp/panel_treat_patents_all", clear

label define award_date_lab 	1 "2020 Q1" 2 "2020 Q2" 3 "2020 Q3" 4 "2020 Q4" ///
								5 "2021 Q1" 6 "2021 Q2" 7 "2021 Q3" 8 "2021 Q4" ///
								9 "2022 Q1" 10 "2022 Q2" 11 "2022 Q3" 12 "2022 Q4" ///
								13 "2023 Q1" 14 "2023 Q2" 15 "2023 Q3" 16 "2023 Q4" 0 "Not Awarded"
								
label define applied_date_lab 	1 "2020 Q1 - Not Awarded" 2 "2020 Q2 - Not Awarded " 3 "2020 Q3 - Not Awarded " 4 "2020 Q4 - Not Awarded" ///
								5 "2021 Q1 - Not Awarded" 6 "2021 Q2- Not Awarded" 7 "2021 Q3 - Not Awarded" 8 "2021 Q4 - Not Awarded" ///
								9 "2022 Q1 - Not Awarded" 10 "2022 Q2 - Not Awarded" 11 "2022 Q3 - Not Awarded" 12 "2022 Q4 - Not Awarded" ///
								13 "2023 Q1 - Not Awarded" 14 "2023 Q2 - Not Awarded" 15 "2023 Q3 - Not Awarded" 16 "2023 Q4 - Not Awarded" 0 "Not Applied"
								
gen awarded_mdate = 0

replace awarded_mdate = 1 	if awarded_2020_Q1 == 1
replace awarded_mdate = 2 	if awarded_2020_Q2 == 1
replace awarded_mdate = 3 	if awarded_2020_Q3 == 1
replace awarded_mdate = 4 	if awarded_2020_Q4 == 1

replace awarded_mdate = 5 	if awarded_2021_Q1 == 1
replace awarded_mdate = 6 	if awarded_2021_Q2 == 1
replace awarded_mdate = 7 	if awarded_2021_Q3 == 1
replace awarded_mdate = 8 	if awarded_2021_Q4 == 1

replace awarded_mdate = 9 	if awarded_2022_Q1 == 1
replace awarded_mdate = 10 	if awarded_2022_Q2 == 1
replace awarded_mdate = 11	if awarded_2022_Q3 == 1
replace awarded_mdate = 12	if awarded_2022_Q4 == 1

replace awarded_mdate = 13	if awarded_2023_Q1 == 1
replace awarded_mdate = 14	if awarded_2023_Q2 == 1
replace awarded_mdate = 15	if awarded_2023_Q3 == 1
replace awarded_mdate = 16	if awarded_2023_Q4 == 1

label values awarded_mdate award_date_lab

gen applied_mdate = .

replace applied_mdate = 1 	if applied_2020_Q1 == 1 & awarded_mdate == 0
replace applied_mdate = 2 	if applied_2020_Q2 == 1 & awarded_mdate == 0
replace applied_mdate = 3 	if applied_2020_Q3 == 1 & awarded_mdate == 0
replace applied_mdate = 4 	if applied_2020_Q4 == 1 & awarded_mdate == 0

replace applied_mdate = 5 	if applied_2021_Q1 == 1 & awarded_mdate == 0
replace applied_mdate = 6 	if applied_2021_Q2 == 1 & awarded_mdate == 0
replace applied_mdate = 7 	if applied_2021_Q3 == 1 & awarded_mdate == 0
replace applied_mdate = 8 	if applied_2021_Q4 == 1 & awarded_mdate == 0

replace applied_mdate = 9 	if applied_2022_Q1 == 1 & awarded_mdate == 0
replace applied_mdate = 10 	if applied_2022_Q2 == 1 & awarded_mdate == 0
replace applied_mdate = 11	if applied_2022_Q3 == 1 & awarded_mdate == 0
replace applied_mdate = 12	if applied_2022_Q4 == 1 & awarded_mdate == 0

replace applied_mdate = 13	if applied_2023_Q1 == 1 & awarded_mdate == 0
replace applied_mdate = 14	if applied_2023_Q2 == 1 & awarded_mdate == 0
replace applied_mdate = 15	if applied_2023_Q3 == 1 & awarded_mdate == 0
replace applied_mdate = 16	if applied_2023_Q4 == 1 & awarded_mdate == 0

label values applied_mdate applied_date_lab

* Extra graphs:

preserve 

replace awarded_mdate = . if awarded_mdate == 0

collapse (sum) patent_f_, by(awarded_mdate f_modate)

drop if awarded_mdate == .

twoway (line patent_f_ f_modate, color(sea) xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(0(1)6,labsize(small)) ytitle("Sum of Patents (Awarded Applicants)") ///
								), by(awarded_mdate, note(""))
								
gr export "$projdir/output/Patents_by_awardee.pdf", replace	

restore

preserve

collapse (sum) patent_f_, by(applied_mdate f_modate) 

drop if applied_mdate == .

twoway (line patent_f_ f_modate, color(red) xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle("") ///
								ylabel(0(1)6,labsize(small)) ytitle("Sum of Patents (Not Awarded Applicants)") ///
								), by(applied_mdate, note(""))
								
gr export "$projdir/output/Patents_by_applicant.pdf", replace	

restore
									
* Create absorbing treatment state 

	* need to double check the starting dates

gen treat = 0

replace treat = 1 if awarded_mdate == 1 & f_modate >=720
replace treat = 1 if awarded_mdate == 6 & f_modate >=735
replace treat = 1 if awarded_mdate == 8 & f_modate >=741
replace treat = 1 if awarded_mdate == 10 & f_modate >=747
replace treat = 1 if awarded_mdate == 12 & f_modate >=753
replace treat = 1 if awarded_mdate == 15 & f_modate >=763

gen groupvar = 0
replace groupvar = 720 if awarded_mdate == 1
replace groupvar = 735 if awarded_mdate == 6
replace groupvar = 741 if awarded_mdate == 8
replace groupvar = 747 if awarded_mdate == 10
replace groupvar = 753 if awarded_mdate == 12
replace groupvar = 763 if awarded_mdate == 15

* SDID

encode applicant_name, gen(applicant)

sort applicant f_modate

bysort applicant: g patent_cumul= sum(patent_f_)

sort patent_cumul

gen IHS_patent = asinh(patent_f_)

* 1) IHS

sdid IHS_patent applicant f_modate treat, vce(noinference) graph g2_opt(ytitle("IHS Patents") xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle(""))

graph export "$projdir/output/IHS_g2_720.pdf", as(pdf) name("g2_720") replace
graph export "$projdir/output/IHS_g2_735.pdf", as(pdf) name("g2_735") replace
graph export "$projdir/output/IHS_g2_741.pdf", as(pdf) name("g2_741") replace
graph export "$projdir/output/IHS_g2_747.pdf", as(pdf) name("g2_747") replace

* 2) Cumulative patents

sdid patent_cumul applicant f_modate treat, vce(noinference) graph g2_opt(ytitle("Cumulative Patents") xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle(""))

graph export "$projdir/output/patent_cumul_g2_720.pdf", as(pdf) name("g2_720") replace
graph export "$projdir/output/patent_cumul_g2_735.pdf", as(pdf) name("g2_735") replace
graph export "$projdir/output/patent_cumul_g2_741.pdf", as(pdf) name("g2_741") replace
graph export "$projdir/output/patent_cumul_g2_747.pdf", as(pdf) name("g2_747") replace


gen ln_patent_cumul = log(patent_cumul + 1)
gen IHS_patent_cumul = asinh(patent_cumul)

sdid ln_patent_cumul applicant f_modate treat, vce(noinference) graph g2_opt(ytitle("Log (Cumulative Patents + 1)") xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle(""))

graph export "$projdir/output/ln_patent_cumul_g2_720.pdf", as(pdf) name("g2_720") replace
graph export "$projdir/output/ln_patent_cumul_g2_735.pdf", as(pdf) name("g2_735") replace
graph export "$projdir/output/ln_patent_cumul_g2_741.pdf", as(pdf) name("g2_741") replace
graph export "$projdir/output/ln_patent_cumul_g2_747.pdf", as(pdf) name("g2_747") replace

sdid IHS_patent_cumul applicant f_modate treat, vce(noinference) graph g2_opt(ytitle("IHS Cumulative Patents") xlabel(564(12)764, labsize(vsmall) angle(45)) xtitle(""))

graph export "$projdir/output/IHS_patent_cumul_g2_720.pdf", as(pdf) name("g2_720") replace
graph export "$projdir/output/IHS_patent_cumul_g2_735.pdf", as(pdf) name("g2_735") replace
graph export "$projdir/output/IHS_patent_cumul_g2_741.pdf", as(pdf) name("g2_741") replace
graph export "$projdir/output/IHS_patent_cumul_g2_747.pdf", as(pdf) name("g2_747") replace

1

generate year = 1960 + floor(f_modate/12)

collapse (sum) patent_f_ (sum) treat (first) awarded_mdate, by(applicant year)

label values awarded_mdate award_date_lab

replace treat = 1 if treat >=1

gen IHS_patent = asinh(patent_f_)

* 3) Year IHS

sdid IHS_patent applicant year treat, vce(noinference) graph

* 4) Year cumulative patents

bysort applicant: g patent_cumul= sum(patent_f_)

sdid patent_cumul applicant year treat, vce(noinference) graph





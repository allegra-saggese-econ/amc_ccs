***************************************************************************
*****							Part II								   ****
***** // Synthetic DID Event Studies - for all cohorts (aggregated) // ****
***************************************************************************

*** 28 industry

// Installing //
***************************************************************************
*ssc install sdid, replace
*net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3) // for scheme lean2

// Formatting //
***************************************************************************
clear

set scheme lean2 

set graphics off 

if "`c(username)'" == "wyf19" {
	global projdir "D:/Dropbox/SOLAR"
	global dropbox "D:/Dropbox"
	global new "$projdir/For meetings/2023"
}	
if "`c(username)'" == "WANGY390" {
	global projdir "C:/Users/WANGY390/Dropbox/SOLAR"
	global dropbox "C:/Users/WANGY390/Dropbox/"
}	

if "`c(username)'"=="laszloda"{
global projdir "/Users/laszloda/Dropbox/SOLAR/"
global dropbox "/Users/laszloda/Dropbox/"
global new "$projdir/For meetings/2023"
}

cd "$projdir/For meetings/2023/meeting-2023/23-08-27/"
/*
use "$new/re-do/re-do-data/patents/patent_everything", replace 

merge 1:1 ADM2_ZH year using "$new/re-do/re-do-data/firm_count_everything"
drop _merge

merge 1:1 ADM2_ZH year using "$new/re-do/re-do-data/orbis/orbis_everything" 
//_merge == 1 because year == 2020
//similar reasons for the rest
drop _merge

merge 1:1 ADM2_ZH year using "$new/re-do/re-do-data/customs/customs_everything"
drop _merge

//merge 1:1 ADM2_ZH year using "$dropbox/SOLAR/data/city_aggregate/ENF_production.dta"
//drop _merge

merge 1:1 ADM2_ZH year using  "$new/re-do/re-do-data/ENF_production.dta"
drop _merge

merge 1:1 ADM2_ZH year using "$dropbox/SOLAR/data/data_aggregate/orbis/gdp_control.dta"
drop _merge

//add city level solar patents
merge 1:1 ADM2_ZH year using  "$dropbox/SOLAR/data/SIPO/SIPO_city_level/solar_patent.dta"
drop if year < 2004
drop if _merge != 3
drop _merge
rename solar_patent solar_patent_city

order ADM2_ZH year id t_a t_d t_s t_innov admin_type solar_patent_city patent_all patent_d patent_u_i solar_u_i_broad solar_u_i_broad_non count revenue_9 panel_production_ panel_capacity_ cell_production_ cell_capacity_ panel_count cell_count value volume price value_solar volume_solar price_solar export_firm_count i_gdp_billion i_population_thousand

keep ADM2_ZH year id t_a t_d t_s t_innov admin_type solar_patent_city patent_all patent_d patent_u_i solar_u_i_broad solar_u_i_broad_non count revenue_9 panel_production_ panel_capacity_ cell_production_ cell_capacity_ panel_count cell_count value volume price value_solar volume_solar price_solar export_firm_count i_gdp_billion i_population_thousand

gen gdp_cap = i_gdp_billion / i_population_thousand * 1000000

label variable t_a "all subsdy"
label variable t_d "demand subsidy"
label variable t_s "supply subsidy"
label variable t_innov "innovation subsidy"
label variable admin_type "admin 2 region type"
label variable solar_patent_city "city level total solar patents"
label variable patent_all "all patents by solar firms"
label variable patent_d "design patents by solar firms"
label variable patent_u_i "utility model & invention patents by solar firms"
label variable solar_u_i_broad "solar patents by solar firms"
label variable solar_u_i_broad_non "non-solar patents by solar firms"
label variable count "firm counts (ENF register data)"
label variable revenue_9 "orbis revenue"
label variable panel_production_ "ENF panel production"
label variable panel_capacity_ "ENF panel capacity"
label variable cell_production_ "ENF cell production"
label variable cell_capacity_ "ENF cell capacity"
label variable panel_count "ENF production data: panel producers"
label variable value "export value, dollar"
label variable volume "export volume"
label variable price "export price"
label variable value_solar "solar export value" 
label variable volume_solar "solar export volume"
label variable price_solar "solar export price"
label variable export_firm_count "number of solar firms exporting "
label variable i_gdp_billion "GDP, billion RMB"
label variable i_population_thousand "population, thousand"


foreach var in solar_patent_city patent_all patent_d patent_u_i solar_u_i_broad solar_u_i_broad_non count panel_production_ panel_capacity_ cell_production_ cell_capacity_ panel_count cell_count value volume price value_solar volume_solar price_solar export_firm_count i_gdp_billion i_population_thousand gdp_cap{
	gen a_`var' = asinh(`var')
}
gen a_revenue_9 = asinh(revenue_9/1000000)
rename a_solar_u_i_broad a_solar_u_i
rename a_solar_u_i_broad_non a_solar_u_i_non
rename solar_u_i_broad solar_u_i
rename solar_u_i_broad_non solar_u_i_non
*/

global lwind=-6
global rwind=6




quietly foreach var in  revenue_9_5_b_long patent_all firm_count {			
	clear all

use "/Users/laszloda/Dropbox/SOLAR/code/paper/cleaning/city_level_dataset.dta"
		merge 1:1 ADM2_ZH year using "$projdir/output/descriptives/firm_level/revenue_adjustments/estimation_data_final", nogenerate 


egen tag=tag(id) 

su revenue_9, det

* Save tmp file
tempfile call
save `call'


// SDID regression by outcome variable //
***************************************************************************

		
	use `call', clear
	
			* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2020

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 
	
	foreach year of numlist 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2020 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
	noisily di "`var' SDID - `year' $S_TIME"
		preserve
		/* ---- is this a 'not-yet-treated' method? 
		drop if groundbreaking`ind' < `year'
		gen t_`ind' = 1 if groundbreaking`ind' !=. & year >= groundbreaking`ind'
		replace t_`ind' = 0 if t_`ind' ==.
		drop if groundbreaking`ind' > `year' & groundbreaking`ind' !=.
		encode ADM2_ZH, gen(id)
		*/ 
		
		egen m=min(year) if t_a==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
		count if mm==`year'
		local mr`year'=r(N)/17

		sdid a_`var' id year t_a, vce(noinference) graph

		matrix lambda`year' = e(lambda)[1..`forward',1] 						// save lambda weight
		matrix yco`year' = e(series)[1..`forward',2]							// control baseline
		matrix ytr`year' = e(series)[1..`forward',3] 							// treated baseline
		matrix aux`year' = lambda`year''*(ytr`year' - yco`year') 				// calculate the pre-treatment mean
		matrix meanpre_o`year' = J(`period',1,aux`year'[1,1])		
		matrix difference`year' = e(difference)[1..17,1..2] 					// Store Ytr-Yco
		
		//mat difference`year'_filled = difference`year'[1..4,1..2]\(1972,.)\difference`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference`year'[10,1..2]\(1982,.)\difference`year'[11..18,1..2]			// Generate missing years
		
		mat difference`year'_filled=difference`year'
		* Harmonizing into event years (from -18 to 21)
		if `year' == 2007 {
			mat tmp = (J(`period'-17,1,.)\difference2007_filled[1..17,2]) - meanpre_o2007 // subtract pre-trend ATT
			mat difference2007_adjusted = eventyear, tmp
		}
		else if `year' == 2020 {
			mat tmp = (difference2020_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_o2020
			mat difference2020_adjusted = eventyear, tmp
		}
		else {
			mat tmp = (J(`lastyr'-`year',1,.)\difference`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_o`year'
			mat difference`year'_adjusted = eventyear, tmp
		}
		
		restore
	}
	
	* SDID event study point estimate using original data (not b-sample)
	foreach year of numlist 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2020 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		svmat difference`year'_adjusted
		ren (difference`year'_adjusted1 difference`year'_adjusted2) (time`year' d_o`year')
		gen mr`year'=0
		replace mr`year'=`mr`year'' if !missing(d_o`year')
		replace d_o`year'=0 if missing(d_o`year')
	
		/*
		su mr`year' if eventyear==$lwind
		if r(mean)==0 {
			replace d_o`year'=0 if !missing(eventyear)
			replace mr`year'=0 if !missing(eventyear)
		}
		su mr`year' if eventyear==$rwind
		if r(mean)==0 {
			replace d_o`year'=0 if !missing(eventyear)
			replace mr`year'=0 if !missing(eventyear)
		}
		*/
		
	}
	egen mr_sum=rowtotal(mr20*)
	replace mr_sum=. if mr_sum==0 
	
	gen d_o=0
	foreach i of numlist 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2020 {
		if 2020-`i'<$rwind | 2004-`i'>$lwind {
				continue 
			}
		gen mweight`i'=mr`i'/mr_sum
		replace d_o=d_o+mweight`i'*d_o`i'
	}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	svmat eventyear
	
	keep eventyear1 d_o
	drop if missing(eventyear1)
	save "file_0.dta", replace 
	
*******************************     CI     *************************************
* Bootstrap for CIs

local b = 1
local B = 100

while `b' <= `B' {
	clear all 
	use `call', clear
	
		* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2020

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 

	bsample, cluster(id) idcluster(c2)
	
	local existing_year ""										// numlist to append randomly drawed treatment years
	local r1 = 0												// r1 will be # of randomly drawed treated counties
	
	foreach year of numlist 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2020{
		if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
		}
		noisily di "`var' CI - `year' - `b' $S_TIME"
		preserve
		
		egen m=min(year) if t_a==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
			qui count if mm==`year'
			local r`year' = r(N)/17
			qui count if mm==.
			local r2 = r(N)/17
			
	 
		if (`r`year'' != 0 & `r2' != 0) {
			qui sdid a_`var' c2 year t_a, vce(noinference) graph
			
			matrix lambda_b`year' = e(lambda)[1..`forward',1] 	// save lambda weight
			matrix yco_b`year' = e(series)[1..`forward',2] 		// control baseline
			matrix ytr_b`year' = e(series)[1..`forward',3] 		// treated baseline
			matrix aux_b`year' = lambda_b`year''*(ytr_b`year' - yco_b`year') 	// calculate the pre-treatment mean
			matrix meanpre_b`year' = J(`period',1,aux_b`year'[1,1])
			matrix difference_b`year' = e(difference)[1..17,1..2]
						
			//mat difference_b`year'_filled = difference_b`year'[1..4,1..2]\(1972,.)\difference_b`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference_b`year'[10,1..2]\(1982,.)\difference_b`year'[11..18,1..2]	// Generate missing years
			mat difference_b`year'_filled=difference_b`year'
			
			
			* Harmonizing into event years (from -18 to 21) and subtract pre-trend ATT
			if `year' == 2007 {
				mat tmp = (J(`period'-17,1,.)\difference_b2007_filled[1..17,2]) - meanpre_b2007 // subtract pre-trend ATT
				mat difference_b2007_adjusted = eventyear, tmp
			}
			else if `year' == 2020 {
				mat tmp = (difference_b2020_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_b2020
				mat difference_b2020_adjusted = eventyear, tmp
			}
			else {
				mat tmp = (J(`lastyr'-`year',1,.)\difference_b`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_b`year'
				mat difference_b`year'_adjusted = eventyear, tmp
			}
			
			local existing_year = "`existing_year'" + " " + "`year'"
			local r1 = `r1' + `r`year''
			noisily di "this: `existing year'"

		}
		
		restore
	}
	
	* Calculating weighted-average ATT for each event study window, which is d_b`b'
	if (`r1' != 0 & `r2' != 0) {
		
		foreach i of local existing_year {
			svmat difference_b`i'_adjusted								// convert mat into var to calculate weight
			ren (difference_b`i'_adjusted1 difference_b`i'_adjusted2) (time_b`i' d_b`i')
		}

		egen cnt = rownonmiss(d_b*)
		gen d_b`b' = 0
		
		foreach i of local existing_year {
			gen r`i' = 0
			replace r`i' = `r`i'' if missing(d_b`i') == 0		// generate var ri using local ri
			
			replace d_b`i' = 0 if missing(d_b`i') & cnt != 0	// replace . with 0 to prevent ATT from being missing
				/*
				su r`i' if eventyear==$lwind
				if r(mean)==0 {
					replace d_b`i'=0 if !missing(eventyear)
					replace r`i'=0 if !missing(eventyear)
				}
				su r`i' if eventyear==$rwind
				if r(mean)==0 {
					replace d_b`i'=0 if !missing(eventyear)
					replace r`i'=0 if !missing(eventyear)
				}
				*/
		}
		
		egen r_sum = rowtotal(r20*)
		replace r_sum =. if r_sum == 0							// replace 0 with . since it will be denominator
		
		foreach i of local existing_year {
			gen weight`i' = r`i'/r_sum							// weight for each treatment in b-sample
			replace d_b`b' = d_b`b' + weight`i' * d_b`i'		// Add weighted-average ATT one by one
		}
		svmat eventyear
		keep if eventyear1 !=.
		keep eventyear1 d_b`b'
		save "file_`b'.dta", replace
		//mkmat d_b`b'											// save d_b`b' as a matrix
		di `b'
		local ++b
		
	}
	
}

********************************************************************************

use "file_0", clear 
drop if missing(eventyear1)
forval b=1/100 {
merge 1:1 eventyear1 using "file_`b'", nogenerate 

}

forval b=100(-1)1 {
rename d_b`b' d_b`b'1
}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	
	
	* Calculating CIs using estimation results of bootstrap samples
	//forvalues b = 1/`B' {
	//	svmat d_b`b'
	//}


	egen rsd = rowsd(d_b11 - d_b`B'1)

	gen LCI = d_o + invnormal(0.025)*rsd 		// lower bounds on bootstrap CIs

	gen UCI = d_o + invnormal(0.975)*rsd 		// upper bounds on bootstrap CIs
	
	* Generate plot
	tw rarea UCI LCI eventyear if eventyear>=$lwind & eventyear<=$rwind , color(gray%30) || scatter d_o eventyear if eventyear>=$lwind & eventyear<=$rwind, color(black) m(d) xtitle("") xlab($lwind(1)$rwind, angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash))
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_nocomp.pdf", replace
	
	save "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_nocomp.dta", replace
}

******
* demand
*****


quietly foreach var in revenue_9_5_b_long patent_all firm_count {			
	clear all

use "/Users/laszloda/Dropbox/SOLAR/code/paper/cleaning/city_level_dataset.dta"
	merge 1:1 ADM2_ZH year using "$projdir/output/descriptives/firm_level/revenue_adjustments/estimation_data_final", nogenerate 

egen tag=tag(id) 

su revenue_9, det

* Save tmp file
tempfile call
save `call'


// SDID regression by outcome variable //
***************************************************************************

		
	use `call', clear
	
			* Define first and last treatment years for the industry
		local fstyr = 2010
		local lastyr = 2020

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 
	
	foreach year of numlist  2010 2012 2013 2014 2015 2016 2017 2018 2020 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
	noisily di "`var' SDID - `year' $S_TIME"
		preserve
		/* ---- is this a 'not-yet-treated' method? 
		drop if groundbreaking`ind' < `year'
		gen t_`ind' = 1 if groundbreaking`ind' !=. & year >= groundbreaking`ind'
		replace t_`ind' = 0 if t_`ind' ==.
		drop if groundbreaking`ind' > `year' & groundbreaking`ind' !=.
		encode ADM2_ZH, gen(id)
		*/ 
		
		egen m=min(year) if t_d==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
		count if mm==`year'
		local mr`year'=r(N)/17

		sdid a_`var' id year t_d, vce(noinference) graph

		matrix lambda`year' = e(lambda)[1..`forward',1] 						// save lambda weight
		matrix yco`year' = e(series)[1..`forward',2]							// control baseline
		matrix ytr`year' = e(series)[1..`forward',3] 							// treated baseline
		matrix aux`year' = lambda`year''*(ytr`year' - yco`year') 				// calculate the pre-treatment mean
		matrix meanpre_o`year' = J(`period',1,aux`year'[1,1])		
		matrix difference`year' = e(difference)[1..17,1..2] 					// Store Ytr-Yco
		
		//mat difference`year'_filled = difference`year'[1..4,1..2]\(1972,.)\difference`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference`year'[10,1..2]\(1982,.)\difference`year'[11..18,1..2]			// Generate missing years
		
		mat difference`year'_filled=difference`year'
		* Harmonizing into event years (from -18 to 21)
		if `year' == 2010 {
			mat tmp = (J(`period'-17,1,.)\difference2010_filled[1..17,2]) - meanpre_o2010 // subtract pre-trend ATT
			mat difference2010_adjusted = eventyear, tmp
		}
		else if `year' == 2020 {
			mat tmp = (difference2020_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_o2020
			mat difference2020_adjusted = eventyear, tmp
		}
		else {
			mat tmp = (J(`lastyr'-`year',1,.)\difference`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_o`year'
			mat difference`year'_adjusted = eventyear, tmp
		}
		
		restore
	}
	
	* SDID event study point estimate using original data (not b-sample)
	foreach year of numlist 2010 2012 2013 2014 2015 2016 2017 2018 2020 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		svmat difference`year'_adjusted
		ren (difference`year'_adjusted1 difference`year'_adjusted2) (time`year' d_o`year')
		gen mr`year'=0
		replace mr`year'=`mr`year'' if !missing(d_o`year')
		replace d_o`year'=0 if missing(d_o`year')
	}

	egen mr_sum=rowtotal(mr20*)
	replace mr_sum=. if mr_sum==0 
	
	gen d_o=0
	foreach i of numlist  2010 2012 2013 2014 2015 2016 2017 2018 2020 {
					if 2020-`i'<$rwind | 2004-`i'>$lwind {
				continue 
			}
		gen mweight`i'=mr`i'/mr_sum
		replace d_o=d_o+mweight`i'*d_o`i'
	}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	svmat eventyear
	
	keep eventyear1 d_o
	drop if missing(eventyear1)
	save "file_0.dta", replace 
	
*******************************     CI     *************************************
* Bootstrap for CIs

local b = 1
local B = 100

while `b' <= `B' {
	clear all 
	use `call', clear
	
		* Define first and last treatment years for the industry
		local fstyr = 2010
		local lastyr = 2020

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 

	bsample, cluster(id) idcluster(c2)
	
	local existing_year ""										// numlist to append randomly drawed treatment years
	local r1 = 0												// r1 will be # of randomly drawed treated counties
	
	foreach year of numlist  2010 2012 2013 2014 2015 2016 2017 2018 2020 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		noisily di "`var' CI - `year' - `b' $S_TIME"

		preserve
		
		egen m=min(year) if t_d==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
			qui count if mm==`year'
			local r`year' = r(N)/17
			qui count if mm==.
			local r2 = r(N)/17
			
		if (`r`year'' != 0 & `r2' != 0) {
			qui sdid a_`var' c2 year t_d, vce(noinference) graph
			
			matrix lambda_b`year' = e(lambda)[1..`forward',1] 	// save lambda weight
			matrix yco_b`year' = e(series)[1..`forward',2] 		// control baseline
			matrix ytr_b`year' = e(series)[1..`forward',3] 		// treated baseline
			matrix aux_b`year' = lambda_b`year''*(ytr_b`year' - yco_b`year') 	// calculate the pre-treatment mean
			matrix meanpre_b`year' = J(`period',1,aux_b`year'[1,1])
			matrix difference_b`year' = e(difference)[1..17,1..2]
						
			//mat difference_b`year'_filled = difference_b`year'[1..4,1..2]\(1972,.)\difference_b`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference_b`year'[10,1..2]\(1982,.)\difference_b`year'[11..18,1..2]	// Generate missing years
			mat difference_b`year'_filled=difference_b`year'
			
			
			* Harmonizing into event years (from -18 to 21) and subtract pre-trend ATT
			if `year' == 2010 {
				mat tmp = (J(`period'-17,1,.)\difference_b2010_filled[1..17,2]) - meanpre_b2010 // subtract pre-trend ATT
				mat difference_b2010_adjusted = eventyear, tmp
			}
			else if `year' == 2020 {
				mat tmp = (difference_b2020_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_b2020
				mat difference_b2020_adjusted = eventyear, tmp
			}
			else {
				mat tmp = (J(`lastyr'-`year',1,.)\difference_b`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_b`year'
				mat difference_b`year'_adjusted = eventyear, tmp
			}
			
			local existing_year = "`existing_year'" + " " + "`year'"
			local r1 = `r1' + `r`year''
			
		}
		
		restore
	}
	
	* Calculating weighted-average ATT for each event study window, which is d_b`b'
	if (`r1' != 0 & `r2' != 0) {
		foreach i of local existing_year {
			svmat difference_b`i'_adjusted								// convert mat into var to calculate weight
			ren (difference_b`i'_adjusted1 difference_b`i'_adjusted2) (time_b`i' d_b`i')
		}

		egen cnt = rownonmiss(d_b*)
		gen d_b`b' = 0
		
		foreach i of local existing_year {
			gen r`i' = 0
			replace r`i' = `r`i'' if missing(d_b`i') == 0		// generate var ri using local ri
			
			replace d_b`i' = 0 if missing(d_b`i') & cnt != 0	// replace . with 0 to prevent ATT from being missing
		}
		
		egen r_sum = rowtotal(r20*)
		replace r_sum =. if r_sum == 0							// replace 0 with . since it will be denominator
		
		foreach i of local existing_year {
			gen weight`i' = r`i'/r_sum							// weight for each treatment in b-sample
			replace d_b`b' = d_b`b' + weight`i' * d_b`i'		// Add weighted-average ATT one by one
		}
		svmat eventyear
		keep if eventyear1 !=.
		keep eventyear1 d_b`b'
		save "file_`b'.dta", replace
		//mkmat d_b`b'											// save d_b`b' as a matrix
		di `b'
		local ++b
		
	}
	
}

********************************************************************************

use "file_0", clear 
drop if missing(eventyear1)
forval b=1/100 {
merge 1:1 eventyear1 using "file_`b'", nogenerate 

}

forval b=100(-1)1 {
rename d_b`b' d_b`b'1
}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	
	
	* Calculating CIs using estimation results of bootstrap samples
	//forvalues b = 1/`B' {
	//	svmat d_b`b'
	//}


	egen rsd = rowsd(d_b11 - d_b`B'1)

	gen LCI = d_o + invnormal(0.025)*rsd 		// lower bounds on bootstrap CIs

	gen UCI = d_o + invnormal(0.975)*rsd 		// upper bounds on bootstrap CIs
	
	

	* Generate plot
	tw rarea UCI LCI eventyear, color(gray%30) || scatter d_o eventyear, color(navy) m(d) xtitle("") xlab(`start'(1)`end', angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(red) lp(solid)) yline(0, lc(red) lp(shortdash))
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_t_d.pdf", replace
	
	save "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_t_d.dta", replace
}

**************
* innovation
*****************


quietly foreach var in revenue_9_5_b_long patent_all firm_count {			
	clear all

use "/Users/laszloda/Dropbox/SOLAR/code/paper/cleaning/city_level_dataset.dta"
	merge 1:1 ADM2_ZH year using "$projdir/output/descriptives/firm_level/revenue_adjustments/estimation_data_final", nogenerate 

egen tag=tag(id) 

su revenue_9, det

* Save tmp file
tempfile call
save `call'


// SDID regression by outcome variable //
***************************************************************************

		
	use `call', clear
	
			* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2018

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 
	
	foreach year of numlist  2007 2009 2011 2014 2018 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
	noisily di "`var' SDID - `year' $S_TIME"
		preserve
		/* ---- is this a 'not-yet-treated' method? 
		drop if groundbreaking`ind' < `year'
		gen t_`ind' = 1 if groundbreaking`ind' !=. & year >= groundbreaking`ind'
		replace t_`ind' = 0 if t_`ind' ==.
		drop if groundbreaking`ind' > `year' & groundbreaking`ind' !=.
		encode ADM2_ZH, gen(id)
		*/ 
		
		egen m=min(year) if t_innov==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
		count if mm==`year'
		local mr`year'=r(N)/17

		sdid a_`var' id year t_innov, vce(noinference) graph

		matrix lambda`year' = e(lambda)[1..`forward',1] 						// save lambda weight
		matrix yco`year' = e(series)[1..`forward',2]							// control baseline
		matrix ytr`year' = e(series)[1..`forward',3] 							// treated baseline
		matrix aux`year' = lambda`year''*(ytr`year' - yco`year') 				// calculate the pre-treatment mean
		matrix meanpre_o`year' = J(`period',1,aux`year'[1,1])		
		matrix difference`year' = e(difference)[1..17,1..2] 					// Store Ytr-Yco
		
		//mat difference`year'_filled = difference`year'[1..4,1..2]\(1972,.)\difference`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference`year'[10,1..2]\(1982,.)\difference`year'[11..18,1..2]			// Generate missing years
		
		mat difference`year'_filled=difference`year'
		* Harmonizing into event years (from -18 to 21)
		if `year' == 2007 {
			mat tmp = (J(`period'-17,1,.)\difference2007_filled[1..17,2]) - meanpre_o2007 // subtract pre-trend ATT
			mat difference2007_adjusted = eventyear, tmp
		}
		else if `year' == 2018 {
			mat tmp = (difference2018_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_o2018
			mat difference2018_adjusted = eventyear, tmp
		}
		else {
			mat tmp = (J(`lastyr'-`year',1,.)\difference`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_o`year'
			mat difference`year'_adjusted = eventyear, tmp
		}
		
		restore
	}
	
	* SDID event study point estimate using original data (not b-sample)
	foreach year of numlist  2007 2009 2011 2014 2018 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		svmat difference`year'_adjusted
		ren (difference`year'_adjusted1 difference`year'_adjusted2) (time`year' d_o`year')
		gen mr`year'=0
		replace mr`year'=`mr`year'' if !missing(d_o`year')
		replace d_o`year'=0 if missing(d_o`year')
	}
	
	egen mr_sum=rowtotal(mr20*)
	replace mr_sum=. if mr_sum==0 
	
	gen d_o=0
	foreach i of numlist   2007 2009 2011 2014 2018 {
				if 2020-`i'<$rwind | 2004-`i'>$lwind {
				continue 
			}
		gen mweight`i'=mr`i'/mr_sum
		replace d_o=d_o+mweight`i'*d_o`i'
	}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	svmat eventyear
	
	keep eventyear1 d_o
	drop if missing(eventyear1)
	save "file_0.dta", replace 
	
*******************************     CI     *************************************
* Bootstrap for CIs

local b = 1
local B = 100

while `b' <= `B' {
	clear all 
	use `call', clear
	
		* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2018

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 

	bsample, cluster(id) idcluster(c2)
	
	local existing_year ""										// numlist to append randomly drawed treatment years
	local r1 = 0												// r1 will be # of randomly drawed treated counties
	
	foreach year of numlist   2007 2009 2011 2014 2018  {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		noisily di "`var' CI - `year' - `b' $S_TIME"

		preserve
		
		egen m=min(year) if t_innov==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
			qui count if mm==`year'
			local r`year' = r(N)/17
			qui count if mm==.
			local r2 = r(N)/17
			
		if (`r`year'' != 0 & `r2' != 0) {
			qui sdid a_`var' c2 year t_innov, vce(noinference) graph
			
			matrix lambda_b`year' = e(lambda)[1..`forward',1] 	// save lambda weight
			matrix yco_b`year' = e(series)[1..`forward',2] 		// control baseline
			matrix ytr_b`year' = e(series)[1..`forward',3] 		// treated baseline
			matrix aux_b`year' = lambda_b`year''*(ytr_b`year' - yco_b`year') 	// calculate the pre-treatment mean
			matrix meanpre_b`year' = J(`period',1,aux_b`year'[1,1])
			matrix difference_b`year' = e(difference)[1..17,1..2]
						
			//mat difference_b`year'_filled = difference_b`year'[1..4,1..2]\(1972,.)\difference_b`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference_b`year'[10,1..2]\(1982,.)\difference_b`year'[11..18,1..2]	// Generate missing years
			mat difference_b`year'_filled=difference_b`year'
			
			
			* Harmonizing into event years (from -18 to 21) and subtract pre-trend ATT
			if `year' == 2007 {
				mat tmp = (J(`period'-17,1,.)\difference_b2007_filled[1..17,2]) - meanpre_b2007 // subtract pre-trend ATT
				mat difference_b2007_adjusted = eventyear, tmp
			}
			else if `year' == 2018 {
				mat tmp = (difference_b2018_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_b2018
				mat difference_b2018_adjusted = eventyear, tmp
			}
			else {
				mat tmp = (J(`lastyr'-`year',1,.)\difference_b`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_b`year'
				mat difference_b`year'_adjusted = eventyear, tmp
			}
			
			local existing_year = "`existing_year'" + " " + "`year'"
			local r1 = `r1' + `r`year''
			
		}
		
		restore
	}
	
	* Calculating weighted-average ATT for each event study window, which is d_b`b'
	if (`r1' != 0 & `r2' != 0) {
		foreach i of local existing_year {
			svmat difference_b`i'_adjusted								// convert mat into var to calculate weight
			ren (difference_b`i'_adjusted1 difference_b`i'_adjusted2) (time_b`i' d_b`i')
		}

		egen cnt = rownonmiss(d_b*)
		gen d_b`b' = 0
		
		foreach i of local existing_year {
			gen r`i' = 0
			replace r`i' = `r`i'' if missing(d_b`i') == 0		// generate var ri using local ri
			
			replace d_b`i' = 0 if missing(d_b`i') & cnt != 0	// replace . with 0 to prevent ATT from being missing
		}
		
		egen r_sum = rowtotal(r20*)
		replace r_sum =. if r_sum == 0							// replace 0 with . since it will be denominator
		
		foreach i of local existing_year {
			gen weight`i' = r`i'/r_sum							// weight for each treatment in b-sample
			replace d_b`b' = d_b`b' + weight`i' * d_b`i'		// Add weighted-average ATT one by one
		}
		svmat eventyear
		keep if eventyear1 !=.
		keep eventyear1 d_b`b'
		save "file_`b'.dta", replace
		//mkmat d_b`b'											// save d_b`b' as a matrix
		di `b'
		local ++b
		
	}
	
}

********************************************************************************

use "file_0", clear 
drop if missing(eventyear1)
forval b=1/100 {
merge 1:1 eventyear1 using "file_`b'", nogenerate 

}

forval b=100(-1)1 {
rename d_b`b' d_b`b'1
}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	
	
	* Calculating CIs using estimation results of bootstrap samples
	//forvalues b = 1/`B' {
	//	svmat d_b`b'
	//}


	egen rsd = rowsd(d_b11 - d_b`B'1)

	gen LCI = d_o + invnormal(0.025)*rsd 		// lower bounds on bootstrap CIs

	gen UCI = d_o + invnormal(0.975)*rsd 		// upper bounds on bootstrap CIs
	
	

	* Generate plot
	tw rarea UCI LCI eventyear, color(gray%30) || scatter d_o eventyear, color(navy) m(d) xtitle("") xlab(`start'(1)`end', angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(red) lp(solid)) yline(0, lc(red) lp(shortdash))
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_t_innov.pdf", replace
	
	save "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_t_innov.dta", replace
}



**************
* supply
*****************


quietly foreach var in revenue_9_5_b_long patent_all firm_count {			
	clear all

use "/Users/laszloda/Dropbox/SOLAR/code/paper/cleaning/city_level_dataset.dta"
	merge 1:1 ADM2_ZH year using "$projdir/output/descriptives/firm_level/revenue_adjustments/estimation_data_final", nogenerate 

egen tag=tag(id) 

su revenue_9, det

* Save tmp file
tempfile call
save `call'


// SDID regression by outcome variable //
***************************************************************************

		
	use `call', clear
	
			* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2018

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 
	
	foreach year of numlist  2007 2008 2009 2011 2012 2013 2014 2015 2016 2018 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
	noisily di "`var' SDID - `year' $S_TIME"
		preserve
		/* ---- is this a 'not-yet-treated' method? 
		drop if groundbreaking`ind' < `year'
		gen t_`ind' = 1 if groundbreaking`ind' !=. & year >= groundbreaking`ind'
		replace t_`ind' = 0 if t_`ind' ==.
		drop if groundbreaking`ind' > `year' & groundbreaking`ind' !=.
		encode ADM2_ZH, gen(id)
		*/ 
		
		egen m=min(year) if t_s==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
		count if mm==`year'
		local mr`year'=r(N)/17

		sdid a_`var' id year t_s, vce(noinference) graph

		matrix lambda`year' = e(lambda)[1..`forward',1] 						// save lambda weight
		matrix yco`year' = e(series)[1..`forward',2]							// control baseline
		matrix ytr`year' = e(series)[1..`forward',3] 							// treated baseline
		matrix aux`year' = lambda`year''*(ytr`year' - yco`year') 				// calculate the pre-treatment mean
		matrix meanpre_o`year' = J(`period',1,aux`year'[1,1])		
		matrix difference`year' = e(difference)[1..17,1..2] 					// Store Ytr-Yco
		
		//mat difference`year'_filled = difference`year'[1..4,1..2]\(1972,.)\difference`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference`year'[10,1..2]\(1982,.)\difference`year'[11..18,1..2]			// Generate missing years
		
		mat difference`year'_filled=difference`year'
		* Harmonizing into event years (from -18 to 21)
		if `year' == 2007 {
			mat tmp = (J(`period'-17,1,.)\difference2007_filled[1..17,2]) - meanpre_o2007 // subtract pre-trend ATT
			mat difference2007_adjusted = eventyear, tmp
		}
		else if `year' == 2018 {
			mat tmp = (difference2018_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_o2018
			mat difference2018_adjusted = eventyear, tmp
		}
		else {
			mat tmp = (J(`lastyr'-`year',1,.)\difference`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_o`year'
			mat difference`year'_adjusted = eventyear, tmp
		}
		
		restore
	}
	
	* SDID event study point estimate using original data (not b-sample)
	foreach year of numlist  2007 2008 2009 2011 2012 2013 2014 2015 2016 2018{
				if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		svmat difference`year'_adjusted
		ren (difference`year'_adjusted1 difference`year'_adjusted2) (time`year' d_o`year')
		gen mr`year'=0
		replace mr`year'=`mr`year'' if !missing(d_o`year')
		replace d_o`year'=0 if missing(d_o`year')
	}
	
	egen mr_sum=rowtotal(mr20*)
	replace mr_sum=. if mr_sum==0 
	
	gen d_o=0
	foreach i of numlist   2007 2008 2009 2011 2012 2013 2014 2015 2016 2018 {
			if 2020-`i'<$rwind | 2004-`i'>$lwind {
				continue 
			}
		gen mweight`i'=mr`i'/mr_sum
		replace d_o=d_o+mweight`i'*d_o`i'
	}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	svmat eventyear
	
	keep eventyear1 d_o
	drop if missing(eventyear1)
	save "file_0.dta", replace 
	
*******************************     CI     *************************************
* Bootstrap for CIs

local b = 1
local B = 100

while `b' <= `B' {
	clear all 
	use `call', clear
	
		* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2018

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		local period = `end' - `start' + 1

		egen eventyear = seq(), f(`start') t(`end')
		mkmat eventyear
		mat eventyear = eventyear[1..`period',1]

		matrix list eventyear 

	bsample, cluster(id) idcluster(c2)
	
	local existing_year ""										// numlist to append randomly drawed treatment years
	local r1 = 0												// r1 will be # of randomly drawed treated counties
	
	foreach year of numlist  2007 2008 2009 2011 2012 2013 2014 2015 2016 2018 {
			if 2020-`year'<$rwind | 2004-`year'>$lwind {
				continue 
			}
		noisily di "`var' CI - `year' - `b' $S_TIME"

		preserve
		
		egen m=min(year) if t_s==1, by(id) //indicator for the year of adoption
		egen mm=mean(m), by(id)
		tab mm
		keep if mm==`year' | mm==. //keep only one time of adoption

		local forward = `year' - 2004
		
			qui count if mm==`year'
			local r`year' = r(N)/17
			qui count if mm==.
			local r2 = r(N)/17
			
		if (`r`year'' != 0 & `r2' != 0) {
			qui sdid a_`var' c2 year t_s, vce(noinference) graph
			
			matrix lambda_b`year' = e(lambda)[1..`forward',1] 	// save lambda weight
			matrix yco_b`year' = e(series)[1..`forward',2] 		// control baseline
			matrix ytr_b`year' = e(series)[1..`forward',3] 		// treated baseline
			matrix aux_b`year' = lambda_b`year''*(ytr_b`year' - yco_b`year') 	// calculate the pre-treatment mean
			matrix meanpre_b`year' = J(`period',1,aux_b`year'[1,1])
			matrix difference_b`year' = e(difference)[1..17,1..2]
						
			//mat difference_b`year'_filled = difference_b`year'[1..4,1..2]\(1972,.)\difference_b`year'[5..9,1..2]\(1978,.\1979,.\1980,.)\difference_b`year'[10,1..2]\(1982,.)\difference_b`year'[11..18,1..2]	// Generate missing years
			mat difference_b`year'_filled=difference_b`year'
			
			
			* Harmonizing into event years (from -18 to 21) and subtract pre-trend ATT
			if `year' == 2007 {
				mat tmp = (J(`period'-17,1,.)\difference_b2007_filled[1..17,2]) - meanpre_b2007 // subtract pre-trend ATT
				mat difference_b2007_adjusted = eventyear, tmp
			}
			else if `year' == 2018 {
				mat tmp = (difference_b2018_filled[1..17,2]\J(`period'-17,1,.)) - meanpre_b2018
				mat difference_b2018_adjusted = eventyear, tmp
			}
			else {
				mat tmp = (J(`lastyr'-`year',1,.)\difference_b`year'_filled[1..17,2]\J(`year'-`fstyr',1,.)) - meanpre_b`year'
				mat difference_b`year'_adjusted = eventyear, tmp
			}
			
			local existing_year = "`existing_year'" + " " + "`year'"
			local r1 = `r1' + `r`year''
			
		}
		
		restore
	}
	
	* Calculating weighted-average ATT for each event study window, which is d_b`b'
	if (`r1' != 0 & `r2' != 0) {
		foreach i of local existing_year {
			svmat difference_b`i'_adjusted								// convert mat into var to calculate weight
			ren (difference_b`i'_adjusted1 difference_b`i'_adjusted2) (time_b`i' d_b`i')
		}

		egen cnt = rownonmiss(d_b*)
		gen d_b`b' = 0
		
		foreach i of local existing_year {
			gen r`i' = 0
			replace r`i' = `r`i'' if missing(d_b`i') == 0		// generate var ri using local ri
			
			replace d_b`i' = 0 if missing(d_b`i') & cnt != 0	// replace . with 0 to prevent ATT from being missing
		}
		
		egen r_sum = rowtotal(r20*)
		replace r_sum =. if r_sum == 0							// replace 0 with . since it will be denominator
		
		foreach i of local existing_year {
			gen weight`i' = r`i'/r_sum							// weight for each treatment in b-sample
			replace d_b`b' = d_b`b' + weight`i' * d_b`i'		// Add weighted-average ATT one by one
		}
		svmat eventyear
		keep if eventyear1 !=.
		keep eventyear1 d_b`b'
		save "file_`b'.dta", replace
		//mkmat d_b`b'											// save d_b`b' as a matrix
		di `b'
		local ++b
		
	}
	
}

********************************************************************************

use "file_0", clear 
drop if missing(eventyear1)
forval b=1/100 {
merge 1:1 eventyear1 using "file_`b'", nogenerate 

}

forval b=100(-1)1 {
rename d_b`b' d_b`b'1
}
	
/// SHOULD WEIGTH BY COHORT SIZES 
	
//	egen d_o = rowmean(d_o*)						// point estimate in event study plot
	
	
	
	* Calculating CIs using estimation results of bootstrap samples
	//forvalues b = 1/`B' {
	//	svmat d_b`b'
	//}


	egen rsd = rowsd(d_b11 - d_b`B'1)

	gen LCI = d_o + invnormal(0.025)*rsd 		// lower bounds on bootstrap CIs

	gen UCI = d_o + invnormal(0.975)*rsd 		// upper bounds on bootstrap CIs
	
	

	* Generate plot
	tw rarea UCI LCI eventyear, color(gray%30) || scatter d_o eventyear, color(navy) m(d) xtitle("") xlab(`start'(1)`end', angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(red) lp(solid)) yline(0, lc(red) lp(shortdash))
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_t_s.pdf", replace
	
	save "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_t_s.dta", replace
}


*******************
* graph redo
*******************



foreach var in  "firm_count" "patent_all" "revenue_9_5_b_long" {
use "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_nocomp.dta", clear
		set scheme lean2 
				* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2020

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
		//drop if eventyear<$lwind | (eventyear>$rwind & !missing(eventyear))
	* Generate plot
	tw rarea UCI LCI eventyear if !missing(d_o), color(gray%30) || scatter d_o eventyear if !missing(d_o),  xline($lwind, lcolor(red) ) xline($rwind, lcolor(red) )  color(black) m(d) xtitle("") xlab($lwind (1)$rwind , angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash))  note("Any subsidy")
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_nocomp $lwind $rwind.png", replace
}




foreach var in "firm_count" "patent_all" "revenue_9_5_b_long" {
use "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_t_d.dta", clear
		set scheme lean2 
				* Define first and last treatment years for the industry
		local fstyr = 2010
		local lastyr = 2020

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
	* Generate plot
	tw rarea UCI LCI eventyear if !missing(d_o), color(gray%30) || scatter d_o eventyear if !missing(d_o), xline($lwind, lcolor(red) ) xline($rwind, lcolor(red) )  color(black) m(d) xtitle("") xlab($lwind (1)$rwind , angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash)) note("Demand subsidy")
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_t_d  $lwind $rwind.png", replace
}



foreach var in "firm_count" "patent_all" "revenue_9_5_b_long" {
use "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_t_innov.dta", clear
		set scheme lean2 
				* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2018

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
	* Generate plot
	tw rarea UCI LCI eventyear if !missing(d_o), color(gray%30) || scatter d_o eventyear if !missing(d_o), xline($lwind, lcolor(red) ) xline($rwind, lcolor(red) )  color(black) m(d) xtitle("") xlab($lwind (1)$rwind , angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash))  note("Innovation subsidy")
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_t_innov  $lwind $rwind.png", replace
}

foreach var in "firm_count" "patent_all" "revenue_9_5_b_long" {
use "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_result_t_s.dta", clear
		set scheme lean2 
				* Define first and last treatment years for the industry
		local fstyr = 2007
		local lastyr = 2018

		* Define eventyear as a column vector, (-18,...,21)'
		local start = 2004 - `lastyr'
		local end = 2020 - `fstyr'
	* Generate plot
	tw rarea UCI LCI eventyear if !missing(d_o), color(gray%30) || scatter d_o eventyear if !missing(d_o), xline($lwind, lcolor(red) ) xline($rwind, lcolor(red) )  color(black) m(d) xtitle("") xlab($lwind (1)$rwind , angle(45)) legend(order(2 "Point Estimate" 1 "95% CI") pos(12) col(2)) xline(-1, lc(black) lp(solid)) yline(0, lc(red) lp(shortdash)) note("Supply subsidy")
	
	graph export "$projdir/output/Regressions/temp/figure/event_sdid//`var'_aggregated_t_s  $lwind $rwind.png", replace
}



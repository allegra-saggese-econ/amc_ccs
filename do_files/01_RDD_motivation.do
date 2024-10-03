
clear all

set scheme lean2

global projdir "/Users/ibanares/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs/"


** FIRST DATASET: CARBONPLAN

* list of projects (Stripe rounds 2020, Fall 2021, Spring 2021 and Microsoft round 2021)

import delimited "$projdir/data/all_projects/CarbonPlan/CarbonPlan-CDR-Database.csv", varnames(1) rowrange(10) clear

* Rename variables:

rename carbonplancdrdatabase 	id	
rename v2 						applicant
rename v3 						location
rename v4 						description	
rename v5 						tags
rename v6 						source	
rename v7 						source_id
rename v8 						source_date	
rename v9						source_url	 
rename v10 						source_license	
rename v11 						documentation	
rename v12 						mechanism	
rename v13 						mechanism_rating
rename v14						mechanism_notes	 
rename v15 						mechanism_comment	
rename v16 						volume	
rename v17 						volume_rating
rename v18 						volume_notes
rename v19 						volume_comment
rename v20 						negativity	
rename v21 						negativity_rating
rename v22 						negativity_notes
rename v23 						negativity_comment	
rename v24 						permanence		
rename v25 						permanence_rating	
rename v26 						permanence_notes	
rename v27 						permanence_comment
rename v28 						additionality	
rename v29 						additionality_notes	
rename v30 						additionality_comment	
rename v31 						price	
rename v32 						price_notes	
rename v33 						price_comment
rename v34 						specificity	
rename v35 						specificity_notes
rename v36 						specificity_comment	
rename v37 						rating
rename v38 						revisions	
rename v39						notes

* Create winner variables

gen winner = 0

/*

** Winners STRIPE 2020:

	Climeworks
	Project Vesta
	CarbonCure
	Charm Industrial

*/

replace winner = 1 if applicant=="Climeworks"					& source =="Stripe 2020 Negative Emissions Purchase"
replace winner = 1 if applicant=="Project Vesta" 				& source =="Stripe 2020 Negative Emissions Purchase"
replace winner = 1 if applicant=="CarbonCure" 					& source =="Stripe 2020 Negative Emissions Purchase"
replace winner = 1 if applicant=="Charm Industrial" 			& source =="Stripe 2020 Negative Emissions Purchase"

/*

** Winners STRIPE FALL 2021:

	44.01 (Protostar Group, Ltd. (“44.01”))
	Ebb (Ebb Carbon)
	Eion (Eion Corp)
	Sustaera
	
*/

replace winner = 1 if applicant=="44.01"						& source =="Stripe Fall 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Ebb Carbon" 					& source =="Stripe Fall 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Eion Corp" 					& source =="Stripe Fall 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Sustaera" 					& source =="Stripe Fall 2021 Negative Emissions Purchase"

/*


** Winners STRIPE SPRING 2021:

	CarbonBuilt
	Heirloom
	Mission Zero Technologies (Mission Zero)
	Running Tide
	SeaChange
	The Future Forest Company (Future Forest)
	
*/

replace winner = 1 if applicant=="CarbonBuilt"					& source =="Stripe Spring 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Heirloom"						& source =="Stripe Spring 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Mission Zero"					& source =="Stripe Spring 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Running Tide"					& source =="Stripe Spring 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="SeaChange"					& source =="Stripe Spring 2021 Negative Emissions Purchase"
replace winner = 1 if applicant=="Future Forest"				& source =="Stripe Spring 2021 Negative Emissions Purchase"


/*


** Winners STRIPE SPRING 2022 (DO NOT APPEAR IN THE CARBONPLAN DATASET YET):

 	8 Rivers
	AspiraDAC
	Living Carbon (note: they won an R&D grant)
	Origen
	RepAir
	Travertine

*/

keep id applicant source mechanism mechanism_rating volume volume_rating negativity negativity_rating permanence permanence_rating additionality price specificity rating winner

destring mechanism mechanism_rating volume volume_rating negativity_rating permanence permanence_rating additionality specificity rating, replace

replace price ="1000" if price=="$1M"

destring price negativity, force replace

drop if source == "Microsoft 2021 CDR RFP"

* RDD-SUGGESTIVE PRELIMINARY GRAPHS
/*

scatter winner permanence, 		by(source) 
scatter winner volume if volume <200000, 			by(source) 
scatter winner negativity, 		by(source) 
scatter winner price, 			by(source) 
scatter winner additionality, 	by(source) 
scatter winner rating, 			by(source)

*/

replace applicant = "C Sink" 									if applicant == "C-Sink"
replace applicant = "CO2-Zero" 									if applicant == "COâ-Zero"
replace applicant = "Carbix" 									if applicant == "Carbix "
replace applicant = "Carbo Culture" 								if applicant == "Carbo Culture "
replace applicant = "Carbon Sequestration Inc" 					if applicant == "Carbon Sequestration"
replace applicant = "Climate Foundation Marine Permaculture"		if applicant == "Climate Foundation"
* rename applicant = if applicant == "EBS" Cant find it in the patents data
replace applicant = "Ensyn Biochar" 								if applicant == "Ensyn"
replace applicant = "Finnish Log House Industry"					if applicant == "Finnish Log House"
replace applicant = "GreenSand" 									if applicant == "GreenSand "
replace applicant = "IndigoAg"									if applicant == "Indigo Ag"
replace applicant = "Mission Zero Technologies"					if applicant == "Mission Zero"
replace applicant = "Nuestark"									if applicant == "Neustark"
replace applicant = "Out of the Blue"							if applicant == "Out of the Blue "
replace applicant = "Rizomeco"									if applicant == "Rizome"
replace applicant = "RockFarm"									if applicant == "RockFarm "
replace applicant = "Seachange"									if applicant == "SeaChange"
replace applicant = "Standard Gas Technologies" 					if applicant == "Standard Gas"
replace applicant = "Susewi" 									if applicant == "SuSeWi"
replace applicant = "Bio-Concrete (VTT and PURO.EARTH)" 			if applicant == "VTT Bio-concrete"

pca volume negativity permanence additionality price, comp(1) 

predict stripepca, score

scatter winner stripepca

scatter winner stripepca, by(source)

save "$projdir/data/temp/stripe_score.dta", replace

import delimited "$projdir/data/Google_patents/firm_level_data.csv", varnames(1) clear

rename applicant_name applicant

keep applicant post_app_patents

duplicates drop

duplicates list applicant

drop if applicant == "Parallel Carbon" // figure our what to do with this one

merge 1:m applicant using "$projdir/data/temp/stripe_score.dta"

replace winner = 1 if applicant == "8 Rivers" // !!

scatter winner stripepca

gr export "$projdir/output/PCA_winners.pdf", replace

scatter post_app_patents stripepca

gen IHS_post_patents = asinh(post_app_patents)

scatter post_app_patents stripepca if winner==0, msize(small) color(sea)  msymbol(circle_hollow)  ||  ///
scatter post_app_patents stripepca if winner==1, msize(small) color(red)  msymbol(circle_hollow) ///
legend(off) xline(0, lstyle(foreground)) ytitle("Patents Post Application")  xtitle("Score") 

gr export "$projdir/output/PCA_patents.pdf", replace

scatter IHS_post_patents stripepca if winner==0, msize(small) color(sea)  msymbol(circle_hollow)  ||  ///
scatter IHS_post_patents stripepca if winner==1, msize(small) color(red)  msymbol(circle_hollow) ///
legend(off) xline(0, lstyle(foreground)) ytitle("IHS Patents Post Application")  xtitle("Score") 

gr export "$projdir/output/PCA_IHS_patents.pdf", replace

ivregress 2sls post_app_patents (winner = stripepca), first vce(robust)

ivregress 2sls IHS_post_patents (winner = stripepca), first vce(robust)

* rdplot IHS_post_patents stripepca, fuzzy(winner) binselect(es) p(2) legend(off)

* rdplot post_app_patents stripepca, fuzzy(winner) binselect(es) p(2) legend(off)

* cmogram post_app_patents stripepca, cut(0) scatter line(0) qfit

* cmogram IHS_post_patents stripepca, cut(0) scatter line(0) qfit

logit winner stripepca
predict pr

twoway connected pr stripepca, sort
gr export "$projdir/output/predicted_winner.pdf", replace

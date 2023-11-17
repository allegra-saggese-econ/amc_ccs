# AMC Project - Innovation and Carbon Capture and Storage

## Accessing and Describing Data
Data includes 2020-21 Stripe/Microsoft data as provided publicly by Carbon Plan. This is stored in /data/projects.csv
Data also includes 2022-23 Stripe data as provided publicy by Frontier Climate. This is stored in /data/project_applications_22_23/

Data set (1) from 2020-21 contains all applications, with their scores across six metrics for performance. This data has been cleaned
and compiled by CDR-CarbonPlan and accessed online. All project PDFs can be found on this github repo: https://github.com/stripe/carbon-removal-source-materials/tree/master

Data set (2) from 2022-23 is the universe of all applicants to the Frontier Climate AMC. All applicants are included. Winners of the early stage
mechanism are published online, while larger offtake agreements are not required to be published (according to discussions with Frontier staff). 
All project PDFs can be found on this github repo: github.com/frontierclimate/carbon-removal-source-materials/tree/main

All Stripe applicant data was merged into one dataset: 
- to categorise winners/losers, the publicly available information for who was awarded a grant is made public on Stripe's website
- for 2022-23, the purchase agreements are made publicly available on Stripe/Frontier climate github
- firm names were scraped, then additional information was added in manually

To download the data in bulk, use this website: https://download-directory.github.io/ and copy and paste the above repo links into the website for a ZIP file of all raw data. 
Raw data is ignored (see gitignore) and must be downloaded yourself. 

## Scripts 
Currently three scripts:
- (1) PDF scraping -- includes use of pdftools to parse using keyword, includes use of PDE package to scrape list of firm names. All code is test code for Stripe, used for Microsoft
- (2) Microsoft data cleaning -- read in world of applicants from CDR and match with Microsoft's disclosed winners. Can only match 2020 data. Project codes from winning projects are identified. See notes -- not all project applicants are awarded as is, some are merged into one large purchase or one partial purchase, data cleaning needed here
- (3) Stripe data cleaning -- read in manual data set and produce summary statistics on Stripe data (including probit model for determining success of applicant based on limited characteristics)
- (4) Financial data assembly -- TBD 
- (5) Policies and general projects -- TBD

## Next steps
We will increase clarity of data manipulation in the Scripts- description. 
We will use the third script to build out a data set of all projects with relevant firm-level characteristics
We may expand code to include relevant policies (CCS/CCUS database)
We may expand code to include originally collected data (financials). 

# AMC Project - Innovation and Carbon Capture and Storage

## Accessing and Describing Data
Data includes 2020-21 Stripe/Microsoft data as provided publicly by Carbon Plan. This is stored in /data/projects.csv
Data also includes 2022-23 Stripe data as provided publicy by Frontier Climate. This is stored in /data/project_applications_22_23/

Data set (1) from 2020-21 contains all applications which were winners, with their scores across six metrics for performance. This data has been cleaned
and compiled by CDR-CarbonPlan and accessed online. All project PDFs can be found on this github repo: https://github.com/stripe/carbon-removal-source-materials/tree/master

Data set (2) from 2022-23 is the universe of all applicants to the Frontier Climate AMC. All applicants are included. Winners of the early stage
mechanism are published online, while larger offtake agreements are not required to be published (according to discussions with Frontier staff). 
All project PDFs can be found on this github repo: github.com/frontierclimate/carbon-removal-source-materials/tree/main

To download the data in bulk, use this website: https://download-directory.github.io/ and copy and paste the above repo links into the website for a ZIP file of all raw data. 
Raw data is ignored (see gitignore) and must be downloaded yourself. 

## Code 
Currently three scripts:
(1) PDF scraping 
(2) Data merging
(3) Financial data assembly 

# AMC Project - Innovation and Carbon Capture and Storage

## Overview

This project investigates the intersection of innovation and Carbon Capture and Storage (CCS) technologies. It leverages datasets from organizations like Stripe, Microsoft, and Frontier Climate to analyze trends and developments in the CCS domain.

## Repository Structure

- **.Rproj.user/**: R project user-specific files.
- **clean_outputs/**: Contains cleaned data outputs.
- **data/**: Raw datasets used for analysis.
    -    Data includes 2020-21 Stripe/Microsoft data as provided publicly by Carbon Plan. This is stored in         **/data/projects.csv**. Data also includes 2022-23 Stripe data as provided publicy by Frontier Climate. This is stored in **/data/project_applications_22_23/** Data set (1) from 2020-21 contains all applications, with their scores across six metrics for performance. This data has been cleaned and compiled by CDR-CarbonPlan and accessed online. All project PDFs can be found on this github repo: *https://github.com/stripe/carbon-removal-source-materials/tree/master.* Data set (2) from 2022-23 is the universe of all applicants to the Frontier Climate AMC. All applicants are included. Winners of the early stagemechanism are published online, while larger offtake agreements are not required to be published (according to discussions with Frontier staff).  All project PDFs can be found on this github repo: *github.com/frontierclimate/carbon-removal-source-materials/tree/main*
    - All Stripe applicant data was merged into one dataset: to categorise winners/losers, the publicly available information for who was awarded a grant is made public on Stripe's website, for 2022-23, the purchase agreements are made publicly available on Stripe/Frontier climate github, firm names were scraped from website, then additional information was added in manually
- **do_files/**: Scripts for data processing and analysis.
- **graphs_output/**: Generated graphs and visualizations.
- **visual_outputs/**: Additional visual materials.

## Key Files

- **amc_ccs.Rproj**: R project file.
- **did_df.csv**: Dataset for Difference-in-Differences analysis.
- **microsoft_data_cleaning.R**: Script for cleaning Microsoft data.
- **orbis-clean-merge.R**: Script for merging and cleaning Orbis data.
- **orbis_desc_DID_R-conversion.R**: Orbis data processing for DiD analysis.
- **patent_level_df.csv**: Patent-level dataset.
- **patstat-cleaning-test.R**: Script for cleaning PATSTAT data.
- **pdf-scraping-loop.R**: Script for scraping data from PDFs.
- **prelim_analysis.R**: Preliminary data analysis script.

## Getting Started

1. **Clone the repository**:
   ```bash
   git clone https://github.com/allegra-saggese-econ/amc_ccs.git
2.	Navigate to the project directory:
3. **Analysis Scripts**
   - Data Cleaning:
           - microsoft_data_cleaning.R: Cleans Microsoft data.
           - orbis-clean-merge.R: Merges and cleans Orbis data.
           - patstat-cleaning-test.R: Cleans PATSTAT data.
   - Data Analysis:
            - prelim_analysis.R: Conducts preliminary analysis.
           - orbis_desc_DID_R-conversion.R: Processes Orbis data for DiD analysis.
   - Data Scraping:
            - pdf-scraping-loop.R: Scrapes data from PDFs.
***

##### Outputs
- Cleaned Data: Stored in clean_outputs/.
- Graphs and Visualizations: Available in graphs_output/ and visual_outputs/.

##### Contributing
Contributions are welcome! Please fork the repository and create a pull request with your proposed changes.

##### License
This project is licensed under the MIT License.

##### Contact
For questions or feedback, please contact Allegra Saggese.

## Outstanding tasks 
- [ ] We will increase clarity of data manipulation in the Scripts- description. 
- [ ] We will use the third script to build out a data set of all projects with relevant firm-level characteristics
- [ ] We may expand code to include relevant policies (CCS/CCUS database)
- [ ] We may expand code to include originally collected data (financials). 

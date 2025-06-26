#' Author: [Allegra Saggese]
#' Purpose: cript to clean and merge the microsoft data into one dataset,
#' data was scraped from PDF, using Excel - PDF extraction software (no coding),
#' includes only projects selected (not the applicant pool)
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

# read in the three datasets 
m21 <- read_csv("Microsoft_2021_CDR.csv", 
                               col_types = cols(`Newly contracted volume (mtCO2)` = col_number(), 
                                                `Contracted durability` = col_number()))

m22 <- read_csv("Microsoft_2022_CDR.csv", 
                col_types = cols(`Newly contracted volume (mtCO2)` = col_number(), 
                                 `Contracted durability (years)` = col_number()))

m23 <- read_csv("Microsoft_2023_CDR.csv", 
                col_types = cols(`Newly contracted volume (mtCO2)` = col_number(), 
                                 `Contracted durability (years)` = col_number()))

# drop empty row in m21
m21 <- m21 %>% filter_all(any_vars(!is.na(.))) %>% 
  rename("Contracted durability (years)" = "Contracted durability") 

# upload the project CSV for comparison of the microsoft projects
carbonplan <- read_csv("C:/Users/SAGGESE/Documents/GitHub/amc_ccs/data/Carbonplan_projects_20_21.csv")
mcarbonplan <- subset(carbonplan, source=="Microsoft 2021 CDR RFP")

# manual check of project by project in winning purchases (bottom to top of list)
land_olake <- "MSFT030" - #closest match to the project in m21, for project 15

nat_con <- subset(mcarbonplan, applicant=="The Nature Conservancy") #for project 14
washington_component <- "MSFT150" # issue here is they proposed much greater than actual contracted - why? 
clinch_valley_component <- c("MSFT101", "MSFT090", "MSFT086", "MSFT085")
# findings indicate the one winning contract is a combination of these four proposals - sequestration only 

silvia <- "MSFT141" # only match for project 13
shell <- "MSFT053" # only match for project 12
regen <- c("MSFT175", "MSFT176", "MSFT177", "MSFT178") # combination and downscale of project 11
nat_arb <- c("MSFT133", "MSFT146") # matches for project 10, description includes both of these projects
greendiamond <- c("MSFT081", "MSFT099")# two matches based on project description, project 9
echo2 <- "MSFT127" # only match for project 8 
cumberland <- c("MSFT080", "MSFT082", "MSFT083", "MSFT089") # four matches managed by Nature Conservancy - downscaled from application, for project 7 
rabo <- "MSFT001" # only match for project 6
climeworks <- "MSFT047" # only match for project 5 
climatecare <- "MSFT092" # only match for project 4 
charm <- "MSFT143" # only match for project 3 
carboncycle <- "MSFT120" # only match for project 2 
carbofex <- "MSFT110" # only match for project 1 

# create tuples between firm name in the winning bid applications and the IDs to match along


# create two new columns in application data - winning bid (partial or full), 
#partial winning bid (for those with combination contracts)


# create column of share of offered credits actually 
#contracted for winning bid (applicant proposal / actual contracted volume)


# bind m21-23
full_df <- rbind(m21,m22,m23) %>% filter_all(any_vars(!is.na(.)))





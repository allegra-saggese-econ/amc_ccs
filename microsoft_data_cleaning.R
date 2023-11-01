# script to clean and merge the microsoft data into one dataset
# data was scraped from PDF, using Excel - PDF extraction software (no coding)
# data includes only projects selected (not the applicant pool)

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
                                 `Contracted durability` = col_number()))

m23 <- read_csv("Microsoft_2023_CDR.csv", 
                col_types = cols(`Newly contracted volume (mtCO2)` = col_number(), 
                                 `Contracted durability` = col_number()))

# drop empty row in m21
m21 <- m21 %>% filter_all(any_vars(!is.na(.)))

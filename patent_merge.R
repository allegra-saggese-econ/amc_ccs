# PURPOSE: get patent data from google and PATSTAT, merge with the STRIPE data 
# Author: ALLEGRA SAGGESE


library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(sqldf)
library(R.utils)
library(RSQLite)

# to input the PATSTAT DATA 
setwd("C:/Users/SAGGESE/Documents/GitHub/amc_ccs/data/PATSTAT")
temp = list.files(pattern="\\.csv$")
#for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) # too time intensive

file1 = paste0(getwd(), "/tls226_part02.csv")
data.table.timing <- system.time(allData <- read_csv(file1)) # test the time to read in the PATSTAT

custom_directory <- "C:/Users/SAGGESE/Documents/GitHub/amc_ccs/data/PATSTAT/temps" #custom directory for tempfiles
custom_tempfile <- tempfile(tmpdir = custom_directory)

custom <- file.path(custom_directory, "bob.csv")
df <- read.csv.sql(file1, 
                   sql = "SELECT * FROM file1",
                   dbname = custom, 
                   header = TRUE)


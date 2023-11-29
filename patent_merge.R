# merge PATSTAT data

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(sqldf)
library(R.utils)

setwd("C:/Users/SAGGESE/Documents/GitHub/amc_ccs/data/PATSTAT")
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

file = paste0(getwd(), "/tls226_part02.csv")
data.table.timing <- system.time(allData <- read.csv(file)) # test the time to read in the PATSTAT


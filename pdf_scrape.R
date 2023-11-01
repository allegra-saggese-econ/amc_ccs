install.packages("pdftools", dependencies=TRUE)
install.packages("tabulizer") # to parse out tables in the PDF
#install.packages("pdftools")
library(pdftools)
library(tabulizer)
library(tidyr)
library(lubridate)
library(kableExtra)
library(dplyr)
library(tidyverse)

# A: PURPOSE: Extract tables from Microsoft Annual reports on purchase agreements 
# Set working directory
setwd("~/")
setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\microsoft_2022_23")
mfiles <- list.files(pattern = "pdf$" )# create a vector of PDF file names
cdr_23 <- pdftools::pdf_text(pdf = mfiles[3])

write(cdr_23)

tablekey <- "Supplier"
table_unformat <- grep(tablekey, cdr_23, value=TRUE)
table_split <- unlist(strsplit(table_unformat, "\n\n\n"))
# drop the headers/footers from the string of characters in the PDF
dftest <- data.frame(table_split) %>% 
    filter(!row_number() %in% c(3)) %>% 
    filter(!row_number() %in% c(9)) %>%
    filter(!row_number() %in% c(6)) %>%
    filter(!row_number() %in% c(6)) %>%
    filter(!row_number() %in% c(3))

dfmod <- dftest %>%  
  mutate(table_split = gsub("\\s{2,}", " ", table_split))
# need to trouble shoot - because there are some parts of the table where the text goes onto two lines
## and therefore this string-split is not capturing this fully 




# generate a function to split the data into separate columns based on distance from next character
# can only do this once the dataframe is a bit more clean
split_string <- function(row) {
  split_values <- unlist(strsplit(row, " "))
  new_values <- character(0)
  current_chunk <- ""
  for (value in split_values) {
    if (nchar(value) > 5) { # make the difference
      if (nzchar(current_chunk)) {
        new_values <- c(new_values, current_chunk)
        current_chunk <- ""
      }
      new_values <- c(new_values, value)
    } else {
      current_chunk <- paste(current_chunk, value, sep = " ")
    }
  }
  if (nzchar(current_chunk)) {
    new_values <- c(new_values, current_chunk)
  }
  return(new_values)
}



# B: PURPOSE: Extract relevant text from PDFs of project applications for Stripe / Frontier 
setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\applications_2022_23\\2022_fall")
files <- list.files(pattern = "pdf$") # create a vector of PDF file names - 2022 fall only
# repeat for spring22/summer23
setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\applications_2022_23\\2022_spring")
spfiles <- list.files(pattern = "pdf$")

setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\applications_2022_23\\2023_summer")
sumfiles <- list.files(pattern = "pdf$")

## Case example - use one PDF to see what relevant information can be extracted (and how)
bob<-pdftools::pdf_text(pdf = files[1])
write(bob)

# step 1: identify first section of relevant information (projections of sequestration)
# clean up PDF, extracting key information
keyword <- "Year"
# identify first table write table 2023-2028 (beginning with Year)
bob[5]
# write table for 2029, 2030
p2 <- bob[6]


# get first table -- sequestration potential per year
txt_key <- grep(keyword, bob, value=TRUE)
lines <- unlist(strsplit(txt_key, "\n"))
df <- data.frame(Text=lines) %>%
  filter(trimws(Text) != "") %>% 
  slice(24:n()) %>%
  separate(Text, into = c("Year", "Estimated Gross Capacity (tonnes)"), sep = "              ", remove = FALSE) %>%
  subset(select = -Text)

df_1 <- df[-c(1, nrow(df)), ]

bottom_df <- unlist(strsplit(p2,"\n")) %>% data.frame(Text=p2) %>% 
  filter(trimws(Text) !="") %>%
  head(-26) %>%
  subset()

#step 2: next table - patent data - p2 contains PATENT DATA - we should extract! 


# step X: export firm-level data set (eventually we will want to push them all into the same dataframe)
## don't forget to set the WD to the output folder 


## Looping application - run the test cases over the entirety of the project folder, 
## see what % of the files can be effectively run through the script. See what errors occur elsewhere. 

#install.packages("pdftools", dependencies=TRUE)
#install.packages("tabulizer") # to parse out tables in the PDF (requires rJava environment)
#install.packages("devtools") # need to ensure Rtools - dependency - is installede here 
#install.packages("pdftools")
# second choice on the package
#install.packages("tcltk2") 
#install.packages("PDE", dependencies = TRUE)

# libraries
library(pdftools)
library(devtools)
library(tabulizer)
library(tabulizerjars)
library(tidyr)
library(lubridate)
library(kableExtra)
library(dplyr)
library(tidyverse)
library(tcltk2)
library(PDE)

# A: PURPOSE: Extract relevant text from PDFs of project applications for Stripe / Frontier 

# Step 1: UPLOAD ALL THE FOLDERS OF THE PDF APPLICATIONS
  setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\stripe_data\\2020_total")
  f20 <- list.files(pattern = "pdf$")
  
  extracted_pdfs_2020 <- list()
  
  for (pdffile in f20){ 
    pdf_text <- pdf_text(pdffile)
    extracted_pdfs_2020[[pdffile]] <- pdf_text
  }
  
  setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\stripe_data\\2021_fall")
  f21_1 <- list.files(pattern = "pdf$")
  
  extracted_pdfs_2021_1 <- list()
  
  for (pdffile in f21_1){ 
    pdf_text <- pdf_text(pdffile)
    extracted_pdfs_2021_1[[pdffile]] <- pdf_text
  }
  
  setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\stripe_data\\2021_spring")
  f21_2 <- list.files(pattern = "pdf$")
 
  extracted_pdfs_2021_2 <- list()
  
    for (pdffile in f21_2){ 
    pdf_text <- pdf_text(pdffile)
    extracted_pdfs_2021_2[[pdffile]] <- pdf_text
  }
  
  setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\stripe_data\\2022_fall")
  f22_2 <- list.files(pattern = "pdf$") # create a vector of PDF file names - 2022 fall only
  
  extracted_pdfs_2022_2 <- list()
  
  for (pdffile in f22_2){ 
    pdf_text <- pdf_text(pdffile)
    extracted_pdfs_2022_2[[pdffile]] <- pdf_text
  }
  # repeat for spring22/summer23
  setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\stripe_data\\2022_spring")
  f22_1 <- list.files(pattern = "pdf$")
  
  extracted_pdfs_2022_1 <- list()
  
  for (pdffile in f22_1){ 
    pdf_text <- pdf_text(pdffile)
    extracted_pdfs_2022_1[[pdffile]] <- pdf_text
  }
  
  setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\stripe_data\\2023_summer")
  f23 <- list.files(pattern = "pdf$")
  
  extracted_pdfs_2023 <- list()
  
  for (pdffile in f23){ 
    pdf_text <- pdf_text(pdffile)
    extracted_pdfs_2023[[pdffile]] <- pdf_text
  }

# Extract list of firm names from the PDF information
PDE_analyzer_i()
PDE_reader_i()
  
#STEP 2: Extract patent data by text
# create a separate set of data with the patent information and then merge with the existing list of firms 

# NOTE: 2020 data DOES NOT CONTAIN ANY SELF DISCLOSED PATENT DATA 
# start with 2021 - we're looking for this phrasing: "d. If any, please link to your patents, pending or granted, that are available publicly"


# Display sections containing 'patent' in each text
print(sections_with_patent)

# step X: export firm-level data set (eventually we will want to push them all into the same dataframe)
## don't forget to set the WD to the output folder 

  

  


######################## CASE: EXTRACTING ALL INFORMATION FROM ONE PDF #################
  ## Case example - use one PDF to see what relevant information can be extracted (and how)
  bob<-pdftools::pdf_text(pdf = f23[1])
  write(bob)
  # identify first section of relevant information (projections of sequestration)
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


















# B: PURPOSE: Extract tables from Microsoft Annual reports on purchase agreements 
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

# next approach ---- taking cdr_23 and using tabulizer
# testing pure extraction of tables - lines do not line up well at all - skip this approach 
table_list <- extract_tables(mfiles[3], pages = 21:23)

# test function for extraction of tables - fails                              
for (i in 1:length(table_list)) {
  cat(paste("Table", i, ":\n"))
  df <- as.data.frame(table_list[[i]])
  print(df)
}


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
############ ENDING: we don't use the PDF parser for microsoft - manual parsing (PDF to EXCEL) is used instead 

install.packages("pdftools", dependencies=TRUE)
# install.packages("pdftools")
library(pdftools)
library(tidyr)
library(lubridate)
library(kableExtra)
library(dplyr)
# create a vector of PDF file names
# Set working directory
setwd("~/")
setwd("C:\\Users\\SAGGESE\\Documents\\GitHub\\amc_ccs\\data\\applications_2022_23\\2022_fall")
files <- list.files(pattern = "pdf$")
# extracting text with pdf_text. Using the lapply function, we can apply the pdf_text function to each element in the vector
# it creates an object called “applications_2020”.

bob<-pdftools::pdf_text(pdf = files[1])

write(bob)

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

#p2 contains PATENT DATA - we should extract! 
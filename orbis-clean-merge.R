#' Author: [Allegra Saggese]
#' Purpose: Clean orbis data for merging with firm-level data,
#' firstly, transform the Orbis dataframe from wide to long format.
#' NOTE: DATA DOWNLOAD FROM ORBIS WAS SPRING 2024
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

##### Load in raw historical data, taken from download on ORBIS 
orbis <- read_excel("data/orbis/historical_df_orbis.xlsx", sheet = "Results")


# CLEAN COLNAMES 

# Function to clean column names and replace "Year - X" with actual years
clean_and_replace_years <- function(names, base_year = 2024) {
  names %>%
    str_replace_all("\n", " ") %>%          # Replace newline characters with spaces
    str_replace_all("\\.\\.\\..*", "") %>%  # Remove trailing dots/numbers
    str_replace_all("Last avail. yr", as.character(base_year)) %>%  # Assume last available year is 2024
    str_replace_all("Year - (\\d+)", function(x) {
      year_num <- as.numeric(str_extract(x, "\\d+"))
      return(as.character(base_year - year_num))
    }) 
}

colnames(orbis) <- clean_and_replace_years(colnames(orbis), base_year = 2024)
print(colnames(orbis))

# clean colnames with fine-tooth comb
colnames(orbis)[colnames(orbis) == "Company name Latin alphabet"] <- "firm_name"

# check dupes
any(duplicated(colnames(orbis)))  # duplicates exist
colnames(orbis) <- make.unique(colnames(orbis))
colnames(orbis)[colnames(orbis) == ""] <- paste0("Unnamed_", seq_len(sum(colnames(orbis) == "")))

# take out static cols before we pivot 
static_cols <- colnames(orbis)[!str_detect(colnames(orbis), "\\d{4}$")]

orbis_static <- orbis %>%
  select(all_of(static_cols))

storestring <- colnames(orbis_static)

# drop empty rows 
orbis <- orbis %>%
  filter(!is.na(firm_name))

# Keep only the columns that need to be pivoted
orbis_pivot <- orbis %>%
  select(-all_of(setdiff(static_cols, "firm_name")))

storestring2 <- colnames(orbis_pivot)

# drop the intersecting cols 
intersect <- intersect(storestring, storestring2) # its empty! 

# cols sum to 711 - total in raw data
dim(orbis_static)  # Number of static columns
dim(orbis_pivot)   # Number of columns to pivot

# WIDE TO LONG
orbis_long <- orbis_pivot %>%
  pivot_longer(
    cols = -c(firm_name),
    names_to = c(".value", "Year"),
    names_pattern = "^(.*) (\\d{4})\\.*\\d*$"  # Extract variable name + year, ignoring suffixes
  ) %>%
  mutate(Year = as.numeric(Year))

# Check transformed data
head(orbis_long)

# drop extra cols in orbis-static that have been transformed 
orbis_static_2 <- orbis_static %>% 
  select(-matches("\\.1$")) %>% 
  select(-matches("\\.2$"))

num_cols <- ncol(orbis_static_2) # Count remaining columns
print(num_cols)

orbis_static_2 <- orbis_static_2 %>%
  select(-1) %>%
  filter(!is.na(firm_name))

# merge back 
orbis_final <- left_join(orbis_long, orbis_static_2, 
                         by = "firm_name", 
                         relationship = "many-to-many") # double check many-to-many matches


# get char column names 

# get other columns - and make them all numeric for ease

# DROP completely empty columns (no data for any years) with both col and NA 


# export long form orbis
date_str <- Sys.Date()
filename <- paste0("orbis_long_", date_str, ".csv")

write.csv(clean_orbis, filename, row.names = FALSE)

# Check data fill 
non_missing_df <- non_missing_percentage %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Percent_Filled")

non_missing_sorted <- non_missing_df %>%
  arrange(desc(Percent_Filled))

# plot data matches

ggplot(non_missing_df, aes(x = reorder(Column, Percent_Filled), y = Percent_Filled)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip for better readability
  labs(
    title = "Percentage of Non-Missing Data per Column",
    x = "Column Name",
    y = "Percentage of Filled Observations"
  ) +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 4))

# NEXT STEPS -- we need to figure out why there are duplicates and make 
# sure they fit into the pivot -- otherwise the data is all messed up 



########## EARLIER CODE --- TO REVISIT TO SEE IF IT IS HELPFUL! 
# ------ QA THE RENAME 
# manual check of cols show some still remain without format adjustments
# one is unimportant -- i.e. SOURCE OF FINANCIALS (drop all that begin with that phrase)

no_employee <- grep("Number of employees", colnames(orbis), value = TRUE)
# repeats on year 1,2,3,4
employee_year_columns <- grep("Number of employees\nYear - [1-4]", colnames(orbis), value = TRUE)
subset_df <- orbis[, employee_year_columns]

# subset from manual check looks duplicated -- will check again 
comparison_results <- list()
for (year in 1:4) {
  # Step 3: Find all columns related to this year
  year_cols <- grep(paste0("Number of employees\nYear - ", year), employee_year_columns, value = TRUE)
  
  # Step 4: Pairwise comparison between columns for this year
  if (length(year_cols) > 1) {
    for (i in 1:(length(year_cols) - 1)) {
      for (j in (i + 1):length(year_cols)) {
        col1 <- year_cols[i]
        col2 <- year_cols[j]
        
        # Compare the two columns and store the result
        comparison_results[[paste(col1, "vs", col2)]] <- all.equal(subset_df[[col1]], subset_df[[col2]])
      }
    }
  }
}

# Print the comparison results
print(comparison_results) # no string mismatches for equivalent years, can drop the dupes





# Print the transformed dataframe
print(orbis_24_long)

## check to see what percentage of the variables contain data for year-firm matches
# clean formatting of na values
replace_na_text <- function(df) {
  df %>%
    mutate_all(~ ifelse(. == "n.a.", NA, .)) # Replace "n.a." with NA across all columns
}

# clean dataframe
orbis_24_long_cleaned <- replace_na_text(orbis_24_long)
# id column formats
class_cols <- sapply(orbis_24_long_cleaned, class)
sum_class_cols <- table(sapply(orbis_24_long_cleaned, class))


# function to calculate NA percentage
na_percentage_by_year <- function(df) {
  df %>%
    group_by(Year) %>%
    summarise(across(where(is.numeric), ~ mean(is.na(.)) * 100))  # Calculate NA percentage for all numeric columns
}
# Example usage:
orbis_24_na_summary <- na_percentage_by_year(orbis_24_long_cleaned)

# Print the NA percentage summary
print(orbis_24_na_summary)





)
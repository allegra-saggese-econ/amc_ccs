#' Author: [Allegra Saggese]
#' Purpose: Clean orbis data for merging with firm-level data,
#' firstly, transform the Orbis dataframe from wide to long format.
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)

##### Load in raw historical data, taken from download on ORBIS 
orbis <- read_excel("data/orbis/historical_df_orbis.xlsx", sheet = "Results")

#### Cleaning step 1
df_filtered <- orbis %>%
  select(-matches("\\.{3}\\d{2}$")) %>%
  select(-matches("\\.{3}\\d{3}$"))

matched_cols <- grep("(.*)(.*Year - \\d{1,2})", colnames(df_filtered), value = TRUE)
prefixes <- gsub("(.*)(.*Year - \\d{1,2})", "\\1", matched_cols)
unique_prefixes <- unique(prefixes)

orbis_long <- df_filtered %>%
  pivot_longer(
    cols = all_of(matched_cols), # consider matched cols with annual ending
    names_to = "colname",
    values_to =  "value"
  ) %>%
  mutate(prefix = gsub("(.*)(.*Year - \\d{1,2})", "\\1", colname)
  ) %>%
  pivot_wider(
    names_from = prefix,
    values_from = value,
    values_fill = list(value = NA)
  ) 

unnest_orbis <- orbis_long %>% 
  unnest_wider(everything(), names_sep="_") 

unnest_orbis <- as.data.frame(unnest_orbis)

# export long form orbis
write.csv(unnest_orbis, "orbis_long.csv", row.names = FALSE)


#### Cleaning step 2 (need to preserve the ORBIS date/time column
# Rename the columns to standardize the year pattern
orbis_renamed <- orbis %>%
  rename_with(.cols = ends_with("Last avail. yr"), 
              .fn = ~ sub("Last avail\\. yr", "Year - 0", .))

# Pivot the dataframe from wide to long format
orbis_24_long <- orbis_renamed %>%
  pivot_longer(
    cols = matches("Year - \\d+"), # Select columns ending in "Year - ..."
    names_to = c(".value", "Year"),
    names_pattern = "(.*) Year - (\\d+)"
  ) %>%
  mutate(Year = 2023 - as.integer(Year)) # Convert year pattern to actual years

# Print the transformed dataframe
print(orbis_24_long)

## check to see what percentage of the variables contain data for year-firm matches
# clean formatting of na values
replace_na_text <- function(df) {
  df %>%
    mutate_all(~ ifelse(. == "n.a.", NA, .)) # Replace "n.a." with NA across all columns
}

# Example usage:
orbis_24_long_cleaned <- replace_na_text(orbis_24_long)


# function to calculate NA percentage
na_percentage <- function(column) {
  mean(is.na(column)) * 100
}

orbis_24_na_summary <- orbis_24_long_cleaned %>%
  group_by(Year) %>%
  summarize(across(everything(), na_percentage))





)
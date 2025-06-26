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
library(lubridate)

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
orbis_final %>%
  summarise(across(everything(), ~ class(.))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Type") %>%
  count(Type)

# make list of actual char cols 
charcols <- c("Source of financials", "Inactive", "Quoted", "Branch", "OwnData", "Woco",
              "Country ISO code", "Consolidation code", "Type of entity", "Size classification",
              "Entity type", "Ticker symbol", "firm_name")

# rename date of incorp col for ease of manipulation
orbis_final <- orbis_final %>%
  rename(datinc = `Date of incorporation`)

orbis_final <- orbis_final %>%
  mutate(
    datinc = na_if(datinc, "n.a."),         # convert "n.a." to NA
    datinc = as.numeric(datinc),            # convert to numeric (NAs stay as NA)
    datinc = if_else(
      !is.na(datinc) & datinc > 3000,       # treat as Excel serial date - based on where I pulled the data
      year(as.Date(datinc, origin = "1899-12-30")),
      datinc                                 
    )
  )

# convert all to numeric except for purposefully characters
orbis_final2 <- orbis_final %>%
  mutate(across(
    .cols = -all_of(charcols),
    .fns = ~ as.numeric(as.character(.))
  ))

# DROP completely empty columns (no data for any years) with both col and NA 
orbis_long_cleaned <- orbis_final2 %>%
  group_by(firm_name) %>%
  filter(!all(across(-c(firm_name, Year), ~ all(is.na(.))))) %>%
  ungroup()

# calc the share of col filled out by num / char / total
indic_cols <- c("firm_name", "Year")

numeric_cols <- orbis_final2 %>%
  select(-all_of(indic_cols)) %>%
  select(where(is.numeric)) %>%
  names()

character_cols <- orbis_final2 %>%
  select(-all_of(c(indic_cols, numeric_cols))) %>%
  select(where(is.character)) %>%
  names()

fill_rate <- function(x) {
  !is.na(x) & x != "n.a."
}

# Compute percentages for each row
row_summary <- orbis_final2 %>%
  mutate(
    numeric_fill = rowSums(across(all_of(numeric_cols), ~ !is.na(.))) / length(numeric_cols),
    character_fill = rowSums(across(all_of(character_cols), fill_rate)) / length(character_cols),
    overall_fill = rowSums(across(-all_of(indic_cols), fill_rate)) / (ncol(.) - length(indic_cols))
  ) %>%
  select(firm_name, Year, numeric_fill, character_fill, overall_fill)

# View as a summary table
print(row_summary)

firm_fill_summary <- row_summary %>%
  group_by(firm_name) %>%
  summarise(
    avg_numeric_fill = mean(numeric_fill, na.rm = TRUE),
    avg_character_fill = mean(character_fill, na.rm = TRUE),
    avg_overall_fill = mean(overall_fill, na.rm = TRUE),
    .groups = "drop"
  )

# View the summary
print(firm_fill_summary)


# plot - histogram by data completeness
ggplot(firm_fill_summary, aes(x = avg_overall_fill)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Average Overall Fill Rate by Firm",
    x = "Average Overall Fill Rate",
    y = "Number of Firms"
  ) +
  theme_minimal()

#plot - histogram data completeness by year
ggplot(row_summary, aes(x = overall_fill)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  facet_wrap(~ Year, scales = "free_y") +
  labs(
    title = "Distribution of Overall Fill Rate by Year",
    x = "Overall Fill Rate",
    y = "Number of Firms"
  ) +
  theme_minimal()

#plot - histogram data completeness by firm size
size_lookup <- orbis_final2 %>%
  select(firm_name, `Size classification`, `Type of entity`, `NACE Rev. 2, core code (4 digits)`) %>%
  distinct()
ff_size <- left_join(firm_fill_summary, size_lookup, by = "firm_name") #df with size 

ggplot(ff_size, aes(x = avg_overall_fill)) +
  geom_histogram(binwidth = 0.05, fill = "darkcyan", color = "white") +
  facet_wrap(~ `Size classification`, scales = "free_y") +
  labs(
    title = "Distribution of Overall Fill Rate by Firm Size",
    x = "Overall Fill Rate",
    y = "Number of Firms"
  ) +
  theme_minimal()

#plot - histogram by entity type 
ggplot(ff_size, aes(x = avg_overall_fill)) +
  geom_histogram(binwidth = 0.05, fill = "darkgreen", color = "white") +
  facet_wrap(~ `Type of entity`, scales = "free_y") +
  labs(
    title = "Distribution of Overall Fill Rate by entity type",
    x = "Overall Fill Rate",
    y = "Number of Firms"
  ) +
  theme_minimal()


# plot - bar chart by data completeness per firm
ggplot(firm_fill_summary, aes(x = reorder(firm_name, avg_overall_fill), y = avg_overall_fill)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Average Overall Fill Rate by Firm",
    x = "Firm Name",
    y = "Average Fill Rate"
  ) +
  coord_flip() +
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 5)  # Smaller text for firm names
  )

# create data of filled percentage by column (data point)
numeric_cols <- orbis_final2 %>%
  select(where(is.numeric))

# Calculate percentage of NAs and non-NAs
na_summary <- data.frame(
  column_name = colnames(numeric_cols),
  percent_na = colMeans(is.na(numeric_cols)) * 100
) %>%
  mutate(percent_non_na = 100 - percent_na)

# View the summary table
print(na_summary)
write_csv(na_summary, "numeric_column_na_summary.csv")

# now by year
numeric_cols <- setdiff(
  names(orbis_final2)[sapply(orbis_final2, is.numeric)],
  c("Year")
)

# Calculate % NA and % non-NA by Year and Column
na_by_year <- orbis_final2 %>%
  select(Year, all_of(numeric_cols)) %>%
  pivot_longer(-Year, names_to = "column_name", values_to = "value") %>%
  group_by(Year, column_name) %>%
  summarise(
    percent_na = mean(is.na(value)) * 100,
    percent_non_na = 100 - percent_na,
    .groups = "drop"
  )

# View the result
print(na_by_year)
write_csv(na_by_year, "numeric_na_summary_by_year.csv")


#### EXPORT DATA 
# export long form orbis with NAs
date_str <- Sys.Date()
filename <- paste0("orbis_long_", date_str, ".csv")

write.csv(orbis_final2, filename, row.names = FALSE)








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
#' Author: [Allegra Saggese]
#' Purpose: Convert SDID code from STATA to R (provided by Ignacio)
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`

# Load necessary libraries
library(stringdist)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)

# Define project directory based on username
username <- Sys.getenv("USER")  # or use "ignaciobanaressanchez", "ibanares", "BANARESS" for testing
if (username == "ignaciobanaressanchez") {
  projdir <- "/Users/ignaciobanaressanchez/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs"
} else if (username == "ibanares") {
  projdir <- "/Users/ibanares/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs"
} else if (username == "BANARESS") {
  projdir <- "C:/Users/BANARESS/LSE Energy Dropbox/Ignacio Banares/Projects/AMCs"
} else if (username == "allegrasaggese") {
  projdir <- "/Users/allegrasaggese/Documents/GitHub/amc_ccs/"
}

####### IMPORT DATA SETS ################
# Import Orbis data - updated with clean data
orbis_data <- read_csv(file.path(projdir, "clean_outputs/orbis_long_2025-06-26.csv"))

# Import patent<>firm matched data (do from scratch - take OG dataset) 
patent_data_raw <- read_csv(file.path(projdir, "clean_outputs/firm_level_data.csv")) # does not include treatment variavble 

# Import cleaned version of patent <> firm matched data
patent_data_clean <- read_csv(file.path(projdir, "clean_outputs/did_df_copy.csv"))

# before we merge orbis to patent data -- make orbis data month-year
months_df <- tibble(Month = 1:12)
orbis_month <- orbis_data %>%
  tidyr::crossing(months_df) %>%
  arrange(firm_name, Year, Month)

head(orbis_month, 20)

orbis_month <- orbis_month %>%
  mutate(
    m_y = format(as.Date(paste(Year, Month, "01", sep = "-")), "%b %Y")
  )

# extract out firm name in patent_data_clean
patent_f <- patent_data_clean %>%
  mutate(firm_name = substr(key, 1, nchar(key) - 5))

############ FIRM NAME MATCH #################
# WILL NEED TO CLEAN UP BOTH SETS OF NAMES HERE for a fuzzy match

# step 1 - review the existing matches 
names_1 <- unique(patent_f$firm_name) # check to see if there are any clear matches
names_2 <- unique(orbis_month$firm_name)

only_in_df1 <- setdiff(names_1, names_2)
only_in_df2 <- setdiff(names_2, names_1)

comparison_result <- list(
  only_in_df1 = only_in_df1,
  only_in_df2 = only_in_df2
)

common_names <- intersect(names_1, names_2) # no common names - need to ignore case! 
# ignoring case theres only two that match perfectly 
common_names <- intersect(tolower(names_1), tolower(names_2)) 

# without clear matches - use FUZZY MATCH 



# Merge orbis and clean patent data
fdf <- full_join(orbis_month, patent_f, by = c("m_y, firm_name"))



# Convert file_date to Date and extract year and month
orbis_data <- orbis_data %>%
  mutate(file_date = as.Date(file_date),
         f_year = year(file_date),
         f_month = month(file_date, label = TRUE, abbr = FALSE),
         f_modate = as.yearmon(file_date))



# Generate patent filed variable
fulldf <- f_df %>%
  mutate(patent_filed = 1)

# Collapse data
collapsed_data <- orbis_data %>%
  group_by(applicant_name, f_modate) %>%
  summarise(patent_filed = sum(patent_filed), .groups = 'drop')

# Reshape wide
wide_data <- collapsed_data %>%
  pivot_wider(names_from = f_modate, values_from = patent_filed, values_fill = list(patent_filed = 0))

# Create full panel
full_panel <- wide_data %>%
  pivot_longer(cols = starts_with("patent_filed"), names_to = "f_modate", values_to = "patent_f_") %>%
  mutate(f_modate_num = as.numeric(f_modate))

# Visualize the data
ggplot(full_panel, aes(x = f_modate_num, y = patent_f_)) +
  geom_line() +
  scale_x_continuous(breaks = seq(564, 764, by = 10)) +
  labs(x = "", y = "Sum of Applicants' Patents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = file.path(projdir, "output/sum_patents.pdf"))

# Import Google patents data
google_data <- read_csv(file.path(projdir, "data/Google_patents/firm_level_data.csv"))

# Keep relevant columns
google_data <- google_data %>%
  select(applicant_name, year, quarter, applied, awarded)

# Create applied and awarded dummy variables
for (year in 2020:2023) {
  for (q in 1:4) {
    google_data <- google_data %>%
      mutate(!!paste0("applied_", year, "_Q", q) := if_else(year == year & quarter == q & applied == 1, 1, 0),
             !!paste0("awarded_", year, "_Q", q) := if_else(year == year & quarter == q & awarded == 1, 1, 0))
  }
}

# Collapse Google data
collapsed_google <- google_data %>%
  group_by(applicant_name) %>%
  summarise(across(starts_with("applied_"), sum, .names = "sum_{.col}"),
            across(starts_with("awarded_"), sum, .names = "sum_{.col}"),
            .groups = 'drop')

# Save and merge data
write_csv(collapsed_google, file.path(projdir, "data/temp/applicants_treatment.csv"))
merged_data <- full_panel %>%
  left_join(collapsed_google, by = "applicant_name")

### NOTE THIS IS NOT COMPLETE! 




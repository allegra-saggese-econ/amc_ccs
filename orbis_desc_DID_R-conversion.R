#' Author: [Allegra Saggese]
#' Purpose: Convert SDID code from STATA to R (provided by Ignacio)
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`

# Load necessary libraries
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



# Import Orbis data
orbis_data <- read_csv(file.path(projdir, "data/orbis/orbis_gen_1.csv"))

# Import patent<>firm matched data 
##### needs formatting work 
patent_data <- read_csv(file.path(projdir, "clean_outputs/did_df.csv"))


# Keep relevant columns
orbis_data <- orbis_data %>%
  select(applicant_name, file_date, treat_date)

# Convert file_date to Date and extract year and month
orbis_data <- orbis_data %>%
  mutate(file_date = as.Date(file_date),
         f_year = year(file_date),
         f_month = month(file_date, label = TRUE, abbr = FALSE),
         f_modate = as.yearmon(file_date))

# Drop rows with NA in f_modate
orbis_data <- orbis_data %>% drop_na(f_modate)

# Generate patent filed variable
orbis_data <- orbis_data %>%
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




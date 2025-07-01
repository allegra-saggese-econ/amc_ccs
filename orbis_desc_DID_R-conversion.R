#' Author: [Allegra Saggese]
#' Purpose: Convert match and SDID code from STATA to R (provided by Ignacio)
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`

# Load necessary libraries
library(stringdist)
library(stringr)
library(fuzzyjoin)
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

# step 2 - drop the endings of the firm name (LLC, INC, etc.) from the orbis data
orbis_month$firm_name_clean <- str_to_lower(orbis_month$firm_name) %>%
  str_remove_all("\\b(ltd|llc|inc\\.?|co\\.?|corp\\.?|limited|corporation)\\b") %>%
  str_remove_all("[\\.,]") %>%  # remove dots and commas
  str_squish()

# step 3 - use FUZZY MATCH 
names_1 <- unique(tolower(patent_f$firm_name)) # refill with lowercase names from both dfs 
names_2 <- unique(orbis_month$firm_name_clean)
dist_matrix <- stringdistmatrix(names_1, names_2, method = "jw") # distance calc

min_match_indices <- apply(dist_matrix, 1, which.min)
closest_matches <- names_2[min_match_indices]

# Combine into a data.frame for inspection
fuzzy_matches <- data.frame(
  original = names_1,
  matched = closest_matches,
  distance = mapply(function(i, j) dist_matrix[i, j], seq_along(min_match_indices), min_match_indices)
)

# review matches
fuzzy_matches <- fuzzy_matches[order(fuzzy_matches$distance), ]
head(fuzzy_matches, 20)

# with manual review we have 4 mismatch and 40 no match 
# manual override of mismatch by manual change to names 
patent_f_n <- patent_f %>%
  mutate(firm_name = case_when(
    firm_name == "captura" ~ "captura group",
    firm_name == "carba" ~ "carba - srl",
    firm_name == "carbyon" ~ "carbyon holding bv",
    firm_name == "living carbon" ~ "living carbon pbc which will do business in california as living carbon",
    TRUE ~ firm_name
  ))

# make both lower case
patent_f_n <- patent_f_n %>%
  mutate(firm_name = tolower(firm_name))

orbis_month_n <- orbis_month %>%
  mutate(firm_name_clean = tolower(firm_name_clean)) %>%
  mutate(firm_name = firm_name_clean)

# ignore 40 mismatched (create new vector to skip through in match)
man_nomatch_vector <- c("alkali earth", "andes", "arbon", "bio-concrete (vtt and puro.earth)",
                        "biomass burial (ebs and puro.earth)", "bluesource", "breadtree farms",
                        "carboncapture", "carbonrun", "carbonto stone", "carbotura",
                        "clairity", "climate foundation marine permaculture", "co2 rail",
                        "co2-zero", "e-quester", "ensyn biochar", "finnish log house industry", 
                        "first gigaton", "future forest", "graphyte", "holy grail", "hyrogas sia",
                        "maia paebbl", "mati", "methane oxidation corp", "minerali", "noble thermodynamics",
                        "nori", "nuestark", "out of the blue", "repair", "rewind earth", "rizomeco",
                        "silicate", "solid carbon", "susewi", "thermodynamic geoengineering", 
                        "travertine tech", "vaulted")

# Merge orbis and clean patent data
df1_nomatch <- patent_f_n %>% 
  filter(firm_name %in% man_nomatch_vector)

df1_to_match <- patent_f_n %>% 
  filter(!firm_name %in% man_nomatch_vector)

# join first on date then closest match 
fuzzy_matched <- df1_to_match %>%
  inner_join(orbis_month_n, by = "m_y") %>%  # regular join on m_y
  mutate(dist = stringdist::stringdist(firm_name.x, firm_name.y, method = "jw")) %>%
  filter(dist <= 0.35) %>%
  group_by(firm_name.x, m_y) %>%
  slice_min(dist, n = 1) %>%       
  ungroup()

#row bind unmatched firms
final_df <- bind_rows(fuzzy_matched, df1_nomatch)

#QA - total
non_na_summary <- final_df %>%
  summarise(across(everything(), ~ mean(!is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Percent_Non_NA")

#QA - by firm 
grouped_non_na_summary <- final_df %>%
  group_by(firm_name) %>%
  summarise(across(everything(), ~ mean(!is.na(.)) * 100, .names = "pct_non_na_{.col}")) %>%
  ungroup()

grouped_non_na_summary_long <- grouped_non_na_summary %>%
  pivot_longer(-firm_name, names_to = "Variable", values_to = "Percent_Non_NA")

summary_stats <- grouped_non_na_summary_long %>%
  summarise(
    mean = mean(Percent_Non_NA, na.rm = TRUE),
    median = median(Percent_Non_NA, na.rm = TRUE),
    mode = names(sort(table(Percent_Non_NA), decreasing = TRUE))[1],
    range = max(Percent_Non_NA, na.rm = TRUE) - min(Percent_Non_NA, na.rm = TRUE),
    sd = sd(Percent_Non_NA, na.rm = TRUE)
  )

print(summary_stats)
# QA shows huge range of data quality. Note we excluded 40 firms bc not in data - so that's driving low median


#write file 
write_csv(final_df, file.path(projdir, "clean_outputs", "250701_patent_firm_orbis.csv"))
#make col date
final_df$m_y_date <- as.Date(paste0("01 ", final_df$m_y), format = "%d %b %Y")


### DATA ALREADY GENERATED FOR PATENTS AND COUNT -- SHOULD REPEAT WITH PATENT RAW DATA 
### NOTE THIS SECTION IS THEREFORE PULLING OFF THE .DO FILE ANALYSIS and is NOT REPLICABLE
## AT THIS POINT -- WILL NEED TO REPEAT! 

# # # ## # # # # # Visualize the data # # # # # # # # # # 
######### PICK UP HERE!! 

ggplot(final_df , aes(x = m_y_date, y = patents_in_period)) +
  geom_line() +
  scale_x_continuous(breaks = seq(564, 764, by = 10)) +
  labs(x = "", y = "Sum of Applicants' Patents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 # ggsave(filename = file.path(projdir, "/graphs_output/2025_sum_patent_vis.pdf"))

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





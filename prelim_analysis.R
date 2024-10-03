#' Author: [Allegra Saggese]
#' Purpose: data cleaning of STRIPE data, summary stats and reg analysis , 
#' include GOOGLE PATENT data (manually collected), and run prelim regression
## on the stripe/google data on identifying trends in winners 
#' Last updated: `r format(Sys.Date(), "%Y-%m-%d")`
#-------------------

library(readr)
library(readxl)
library(plm)
library(stats)
library(lmtest)
library(tidyr)
library(tidyverse)
library(dplyr)
library(survival)
library(ggplot2)
library(ggpubr)
library(zoo)


# Read in data
stripedf <- read_excel("data/stripe_data/cleaned_outputs/manual-aggregated-firmlist.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                                             "numeric", "numeric", "text", "text", 
                                                             "skip", "skip", "numeric", "skip", 
                                                             "skip", "numeric", "numeric", "skip", 
                                                             "skip", "skip", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
#new cols
stripedf$awarded_factor <- as.factor(stripedf$awarded) 
stripedf$realised_cost_decrease <- (stripedf$current_cost_per_tonne-stripedf$price)*(-1)
stripedf <- stripedf %>%
  mutate(price_contract_diff = case_when(awarded==1 & R_and_D_offer==0 ~ price-contractprice,
                                         awarded==0 ~ 0),
         quantity_contract_diff = case_when(awarded==1 & R_and_D_offer==0 ~ offer_quantity-contract_quantity,
                                            awarded==0 ~ 0)
  )

# subset the df for awarded firms only - to use later 
stripedf$awardednum <- as.numeric(stripedf$awarded)
award_onlydf <- stripedf %>% 
  filter(awarded>0)

# filtered df - proposals only
prop_onlydf <- stripedf %>% filter(awarded==0)


########### REGRESSION ANALYSIS: FIRM CHARACTERISTICS ON WINNING/LOSING ####################
# Step 1: Early analysis 

# test logit regressions 
no_control <- glm(formula = awarded_factor ~ price, data = stripedf, 
                  family = binomial(link="logit"))
# price is not a significant determinant of choice of award/not awarded
summary(no_control) 


diff_realised_price <- glm(formula = awarded_factor ~ (realised_cost_decrease) + 
                             year, data = stripedf, family = binomial(link="logit"))

diff_2 <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year 
              + offer_quantity, data = stripedf, family = binomial(link="logit"))

diff_3 <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year 
              + stripedf$`lifetime mtco2e`, data = stripedf, 
              family = binomial(link="logit"))
# realised cost decrease has no impact on award choice
summary(diff_realised_price)
summary(diff_2)
summary(diff_3)


long_reg <- glm(formula = awarded_factor ~ price + offer_quantity 
                + price*offer_quantity + year, data = stripedf, 
                family = binomial(link="logit"))
summary(long_reg)




# summary statistics
sum_per_yr <- stripedf %>% group_by(year, quarter) %>% 
  summarise(total_count=n(),.groups='drop') %>%
  as.data.frame()

sum_award_group <- stripedf %>% filter(stripedf$awarded>0) %>% group_by(year, quarter) %>% 
  summarise(total_count=n(),.groups='drop') %>%
  as.data.frame()

tot_award_sum <- stripedf %>% filter(stripedf$awarded>0) %>% group_by(year) %>%
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE)) %>%
  mutate(awarded <- "1")

tot_losses_sum <- stripedf %>% filter(stripedf$awarded==0) %>% group_by(year) %>%
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE)) %>%
  mutate(awarded <- "0")

sum_2 <- stripedf %>% group_by(year, quarter) %>% 
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE))


#combine tot_award_sum
summary_join <- full_join(tot_award_sum, tot_losses_sum)



########### SUMMARY STATISTICS OF STRIPE APPLICANT DATA ####################
# Step 1: plot price and quantity


plotdata <- stripedf %>% filter(price<1000000) %>% filter(offer_quantity<10000000)
fine_tune <- stripedf %>% filter(price<10000) %>% filter(offer_quantity<100000)
ggplot(data=plotdata, aes(x=year, y=price))+ geom_point()


price_histo <- ggplot(data=plotdata, aes(x=price))+ geom_histogram(bins=20)
price_histo_winners <- ggplot(data=award_onlydf, aes(x=award_onlydf$price))+ geom_histogram(bins=20)
price_histo_losers <- ggplot(data=prop_onlydf, aes(x=prop_onlydf$price))+ geom_histogram(bins=20)

ggplot(award_onlydf, aes(x = award_onlydf$price, y = award_onlydf$offer_quantity)) + geom_point()
ggplot(fine_tune, aes(x = fine_tune$price, y = fine_tune$offer_quantity)) + geom_point()


ggplot(award_onlydf, aes(x = year, y = price)) + geom_point()
ggplot(award_onlydf, aes(x = year, y = offer_quantity)) + geom_point()



#make awarded numeric again
stripedf$awardednum <- as.numeric(stripedf$awarded) 
# summary table for the numeric cols
sum_awarded <- stripedf %>% group_by(awarded) %>% 
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE))



################# READ IN GOOGLE PATENT DATA #######################################

gpatent_data <-  read_csv("data/stripe_data/cleaned_outputs/google_patent_data_manual_CSV.csv", 
                          col_types = cols(file_date = col_date(format = "%d/%m/%Y")))

full_df <- merge(gpatent_data, stripedf, by=c("applicant_name", "year", "quarter"))
# QA - no issue
rows_dropped <- anti_join(gpatent_data, stripedf, by=c("applicant_name", "year", "quarter"))
nonmatched <- data.frame(unique(rows_dropped$applicant_name))


full_df_sum <- full_df %>% group_by(applicant_name, year, quarter) %>%
  mutate(sum_patents = sum(!is.na(file_date))) %>%
  arrange(file_date) 

qa_no_patents <- full_df_sum %>% filter(sum_patents==0) %>% summarise(n()) # 97 firms without patents 
rm(qa_no_patents)
# 419 patents, shared by a total of 70 applicants (40% of applicants have patents in Google Patent data)

post_prize_patent <- full_df_sum %>%
  summarise(count = sum(as.integer(format(file_date, "%Y")) > year, na.rm = TRUE))

full_df_sum <- full_df_sum %>% ungroup()
# fill in missing data
full_df_sum$file_year <- as.numeric(format(full_df_sum$file_date, "%Y"))

df_regtest <- merge(full_df_sum, post_prize_patent, by = c("applicant_name", "year", "quarter")) %>%
  rename(post_app_patents = "count") %>% distinct(applicant_name, year, quarter, .keep_all = TRUE) %>%
  arrange(applicant_name, year) %>% ungroup()

wopatents <- df_regtest %>% filter(sum_patents==0)
########## qa of df regtest (dataframe to use for regressions)
any_na <- any(is.na(df_regtest$sum_patents)) # has no missing values (0-77)
any_na1 <- any(is.na(df_regtest$decision_date)) # has missing values

# Output the result
if (any_na) {
  cat("The column contains NA values.")
} else {
  cat("The column does not contain NA values.")
}
  
############## reg on patent output ##########################
patent_outcome <- plm(post_app_patents ~ awarded_factor, 
                      data = df_regtest, index=c("applicant_name", "year", "quarter"), model = "within")
summary(patent_outcome)
# unbalanced panel --- to revisit

patent_outcome_1 <- glm(post_app_patents ~ awarded_factor + year, 
                        data = df_regtest) # want to use year fixed effects - not control for year (two cycles within this)

patent_outcome_2 <- lm(post_app_patents ~ awarded_factor + year, 
                       data = df_regtest) # small positive sig

patent_outcome_3 <- lm(post_app_patents ~ awarded_factor + year, 
                       data = df_regtest) # small positive sig

# some results showing when you control for previous patents --- there is signficance
patent_outcome_4 <- lm(post_app_patents ~ awarded_factor + year + 
                         (sum_patents - post_app_patents), data = df_regtest)

# create new factor variables for regression
df_regtest$year_factor <- as.factor(df_regtest$year)
df_regtest$repeat_factor <- as.factor(df_regtest$previous_application_stripe)
entity_counts <- table(df_regtest$applicant_name)
multiple_year_entities <- names(entity_counts[entity_counts > 1])
filtered_data <- df_regtest[df_regtest$applicant_name %in% multiple_year_entities, ] # 13 firms that applied more than once
balanced_df <- df_regtest[!(df_regtest$applicant_name %in% multiple_year_entities), ]
# issues with this reg model ---- to revisit
patent_outcome_4a <- plm(post_app_patents ~ awarded_factor + (sum_patents - post_app_patents),
                         data = balanced_df, index = c("applicant_name", "year_factor"), model="within")

patent_outcome_5 <- lm(post_app_patents ~ awarded_factor + year_factor + 
                         (sum_patents - post_app_patents) + offer_quantity, 
                       data = df_regtest)
# adding in if they were a repeated applicant 
patent_outcome_5a <- lm(post_app_patents ~ awarded_factor + year_factor + 
                         (sum_patents - post_app_patents) + repeat_factor, 
                       data = df_regtest)

atent_outcome_6 <- lm(post_app_patents ~ awarded_factor + year + 
                         (sum_patents - post_app_patents) + offer_quantity + price, 
                       data = df_regtest)
# no relationship here
patent_outcome_7 <- lm(post_app_patents ~ awarded_factor + year + 
                         df_regtest$'lifetime mtco2e' + realised_cost_decrease, 
                       data = df_regtest)

# running the opposite- direction --- no effect
award_outcome <- glm(awarded_factor ~ (sum_patents - post_app_patents), 
                     data = df_regtest, 
                     family = binomial(link="logit"))


####################### SUMMARY STATISTICS AGAIN ########################
# plot the variables by award/not award

# summary stat on some of the variables --- to functionalize
summary(df_regtest$sum_patents)
mean_val <- mean(df_regtest$sum_patents, na.rm=TRUE)
sd_val <- sd(df_regtest$sum_patents, na.rm=TRUE)

filtered_data <- df_regtest %>%
  filter(sum_patents <= mean_val + 2 * sd_val)

plot(density(filtered_data$sum_patents), main = "Density Plot without Outliers", xlab = "Sum of patents by applicants")

summary(df_regtest$contractprice)
mean_val <- mean(df_regtest$contractprice, na.rm=TRUE)
sd_val <- sd(df_regtest$contractprice, na.rm=TRUE)

filtered_data <- df_regtest %>%
  filter(contractprice <= mean_val + 2 * sd_val)

plot(density(filtered_data$contractprice), main = "Density Plot without Outliers", 
     xlab = "Contract price distribution (per tonne CO2)")

summary(df_regtest$contract_quantity)
mean_val <- mean(df_regtest$contract_quantity, na.rm=TRUE)
sd_val <- sd(df_regtest$contract_quantity, na.rm=TRUE)

filtered_data <- df_regtest %>%
  filter(contract_quantity <= mean_val + 2 * sd_val)

plot(density(filtered_data$contract_quantity), main = "Density Plot without Outliers", 
     xlab = "Contract quantity distribution (total tonnes)")


mean_val <- mean(df_regtest$price, na.rm=TRUE)
sd_val <- sd(df_regtest$price, na.rm=TRUE)

filtered_data <- df_regtest %>%
  filter(price <= mean_val + 2 * sd_val)

plot(density(filtered_data$price), main = "Density Plot without Outliers", xlab = "Offer Price for CO2 tonne")


# compare with out outliers to the one with -- you can see we get more information
boxplot(df_regtest$price, col = "lightgreen", main = "Boxplot of Variable")
hist(filtered_data$price, col = "skyblue", main = "Offer Price Histogram", xlab="Price per tonne (no outliers)") 



#### to turn the summary density plots into a function --- in progress
list_cols <- colnames(df_regtest)
num_cols <- df_regtest %>% select_if(is.numeric)
num_cols <- colnames(num_cols)
looplist <- c("capex", "opex", "lifetime mtco2e", "gross project emissions", "offer_quantity", "realised_cost_decrease", "sum_patents", "post_app_patents")
plot_list = list()

# loop through the list of numeric values to see outcomes
for (x in looplist) {
  summary(df_regtest[[x]])
  mean_val <- mean(df_regtest[[x]], na.rm=TRUE)
  sd_val <- sd(df_regtest[[x]], na.rm=TRUE)

  filtereddf<- df_regtest %>%
    filter(price <= mean_val + 2 * sd_val)
  
  plot <- plot(density(filtereddf[[x]]), main = "Density Plot without Outliers", xlab = "Variable")
  plot_list[[x]] <- plot 
}


ggplot(filtered_data, aes(x = offer_quantity, y = price, color = awarded_factor)) +
  geom_point() +
  labs(x = "Offer Quantity", y = "Offer price", color = "Awarded") +
  scale_color_discrete(name = "Awarded", labels = c("Not Awarded", "Awarded"))

filtered_data_sub1 <- subset(filtered_data, awarded==1)
filtered_data_sub2 <- subset(filtered_data, awarded==0)

ggplot(filtered_data, aes(x = current_cost_per_tonne, y = price, color = awarded_factor)) +
  geom_point() +
  labs(x = "Current cost per tonne", y = "Offer price", color = "Awarded") +
  scale_color_discrete(name = "Awarded", labels = c("Not Awarded", "Awarded")) + 
  geom_smooth(method = "lm", se=FALSE)

# Plot 1: Contracted price vs. contracted quantity by awarded applicants (previous, novel applications)
ggplot(filtered_data, aes(x = contract_quantity, y = contractprice, color=as.factor(previous_application_stripe))) +
  geom_point() +
  labs(x = "Contract quantity (tonnes)", y = "Contract price ($ per tonne)", color="Previous Applicant?") +
  scale_color_discrete(name = "Previous application", labels = c("No", "Yes"))

# Plot 2: Contracted price vs. contracted quantity by count of post-application patent
ggplot(filtered_data, aes(x = contract_quantity, y = contractprice, color=post_app_patents)) +
  geom_point() +
  labs(x = "Contract quantity (tonnes)", y = "Contract price ($ per tonne)", color="Patent count (after award)") 

# Plot 3: 
colstosum <- c("year", "quarter", "awarded", "lifetime mtco2e", "contractprice", "current_cost_per_tonne", "gross project emissions", "offer_quantity",
               "contract_quantity", "realised_cost_decrease", "price_contract_diff", "quantity_contract_diff", "sum_patents",
               "post_app_patents")
new_names <- c("year", "quarter", "awarded", "lifetimeabate", "contractprice", "estcost", "totem", "offerq",
                            "contractq", "costdecrease", "pdiff", "qdiff", "totpatents",
                            "postpatents")

tot_award_sum_2 <- filtered_data_sub1 %>% select(all_of(colstosum)) %>% 
  rename_with(~ new_names, everything()) %>%
  group_by(year, quarter) %>%
  summarise(across(everything(), list(mean = mean, median = median, sd = sd), na.rm=TRUE))

awarded_sum_long <- tot_award_sum_2 %>%
  pivot_longer(-c(year, quarter), names_to = c(".value", "stat"), names_sep = "_") 

############ WANT TO MAKE MORE GRAPHS SHOWING SUMMARY STATISTICS IN FACET GRAPHS! 

#setwd("C:/Users/SAGGESE/Documents/GitHub/amc_ccs/clean_outputs")
#write.csv(awarded_sum_long, "summary_stats.csv")
#write.csv(df_regtest, "firm_level_data.csv")



########################## REORGANIZE FIRM LEVEL DATA FOR REGRESSION (DID) ################################

# make date columns
current_date <- Sys.Date()
dates_col <- seq(as.Date("2007-01-01"), current_date, by = "month")
dates_col_2 <- as.yearmon(dates_col)

#create treatment date column 
full_df_sum <- full_df_sum %>% mutate(treat_date = case_when(
  (quarter==3 & year == 2023) ~ as.Date("2023-09-01"),
  (quarter==2 & year == 2021) ~ as.Date("2021-05-17"),
  (quarter==4 & year == 2021) ~ as.Date("2021-12-05"),
  (quarter==2 & year == 2022) ~ as.Date("2022-06-30"),
  (quarter==4 & year == 2022) ~ as.Date("2022-12-15"),
  (quarter==1 & year == 2020) ~ as.Date("2020-05-18"), 
  TRUE ~ NA
)) %>% select(-c("assignee", "classification_6", "classification_5"))

df_did_format <- full_df_sum
df_did_format$m_y <- paste(year(df_did_format$file_date), month(df_did_format$file_date), sep = "-")
df_did_format$m_y <- as.yearmon(df_did_format$m_y)
df_did_format$key = paste0(df_did_format$applicant_name, df_did_format$year, df_did_format$quarter)
  
# take unique firms and create firms for each month in the project period
firmsonly <- full_df_sum %>% 
  mutate(key = paste0(applicant_name, year, quarter)) %>%
  distinct(key, .keep_all = TRUE)
firm_month_interval <- expand.grid(key = firmsonly$key, m_y = dates_col_2)
#firm_month_interval$m_y <- paste(year(firm_month_interval$date), month(firm_month_interval$date), sep = "-")

# before cleaning - need to QA the treat_date column
summary(df_did_format$treat_date)
qadf <- df_did_format %>% filter(is.na(treat_date))
qadf2 <- df_did_format %>% filter(is.na(m_y))# issue is I kept the patent data of non-patented firms

joined_panel_2 <- df_did_format %>% group_by(key, m_y) %>%
  mutate(patents_in_period = sum(!is.na(file_date))) %>% # create col for the number of patents in each period
  ungroup() %>% filter(!sum_patents ==0) # drop where there are no patents

qadf3 <- joined_panel_2 %>% filter(is.na(m_y))
rm(qadf3)

# drop the duplicate patents (i.e. filed in two locations)
joined_panel_3 <- joined_panel_2 %>% filter(!is.na(patents_in_period)) %>%
  group_by(key, m_y, patent_name) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(key, m_y) %>%
  mutate(unique_patents_in_period = n())

jp2 <- joined_panel_2 %>% select(c(patents_in_period, key, m_y, file_date, 
                                   file_year, patent_name, application_number,
                                   inventor_1, quarter, year, treat_date))

jp3 <- joined_panel_3 %>% select(c(key, m_y, patent_name, unique_patents_in_period))

# summarise patent level data (count of total, count of unique)
jp_to_add <- merge(jp2, jp3, by=c("key", "m_y", "patent_name")) %>% filter(file_date > as.Date("2020-05-01"))
jp_to_add_unique <- jp_to_add %>% group_by(key, m_y) %>% #may need to check the texts from google patent if some are too similar 
  slice(1)
#remerge patent data back 
did_merged <- left_join(firm_month_interval, jp_to_add_unique, by = c("m_y", "key")) %>%
  arrange("m_y", "key")

qa_count <- jp_to_add_unique %>% ungroup() %>% group_by(key) %>%
  slice(1)

# we ignore the pre-period of observation patents
pre_period_patents <- merge(jp2, jp3, by=c("key", "m_y", "patent_name")) %>% 
  filter(file_date <= as.Date("2007-01-01")) # 34 patents pre-observation period

# 30 unique patents in the pre-period
pre_period_unique <- pre_period_patents %>% group_by(key, m_y) %>% 
  slice(1) %>%
  ungroup %>%
  group_by(key) %>%
  mutate(pre_period_patents_sum = n()) # new column for all the pre-treatment timeframe patents

############ merge back in firm-level data to did_merged
df_regtest$key = paste0(df_regtest$applicant_name, df_regtest$year, df_regtest$quarter)
clean_firm_level <- df_regtest %>% 
  select(-c(applicant_name, patent_name, patent_region,
            patent_no, patent_no_list, application_number, file_date, assignee, abandoned, citations,
            classification_1, classification_2, classification_3, classification_4, classification_5,
            classification_6, classification_7, classification_8, inventor_1, post_app_patents, file_year,
            quarter, year))

# merge firm level data back in
full_did_df <- did_merged %>% select(-c(patent_name, application_number, inventor_1)) %>%
  left_join(clean_firm_level, by="key")

# write the firm-month data
write.csv(full_df_sum, "patent_level_df.csv")
write.csv(full_did_df, "did_df.csv")





# summary statistics on STRIPE data 
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


########### REGRESSION ANALYSIS: FIRM CHARACTERISTICS ON WINNING/LOSING ####################
# Step 1: Early analysis 

# test logit regressions 
no_control <- glm(formula = awarded_factor ~ price, data = stripedf, 
                  family = binomial(link="logit"))
# price is not a significant determinant of choice of award/not awarded
summary(no_control) 

diff_realised_price <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year, data = stripedf, family = binomial(link="logit"))

summary(diff_realised_price)


# consider 
diff_2 <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year 
              + offer_quantity, data = stripedf, family = binomial(link="logit"))
summary(diff_2)

diff_3 <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year 
              + stripedf$`lifetime mtco2e`, data = stripedf, 
              family = binomial(link="logit"))

summary(diff_3)

long_reg <- glm(formula = awarded_factor ~ price + offer_quantity 
                + price*offer_quantity + year, data = stripedf, 
                family = binomial(link="logit"))

summary(long_reg)


# summary statisticsa
sum_1 <- stripedf %>% group_by(year) %>% 
  summarise(total_count=n(),.groups='drop') %>%
  as.data.frame()

stripedf$awardednum <- as.numeric(stripedf$awarded)
award_onlydf <- stripedf %>% 
  filter(awarded>0)
prop_onlydf <- stripedf %>% filter(awarded==0)


sum_award_group <- stripedf %>% filter(stripedf$awarded>0) %>% group_by(year) %>% 
  summarise(total_count=n(),.groups='drop') %>%
  as.data.frame()

tot_award_sum <- stripedf %>% filter(stripedf$awarded>0) %>% group_by(year) %>%
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE)) %>%
  mutate(awarded <- "1")

tot_losses_sum <- stripedf %>% filter(stripedf$awarded==0) %>% group_by(year) %>%
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE)) %>%
  mutate(awarded <- "0")

sum_2 <- stripedf %>% group_by(year) %>% 
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE))


#combine tot_award_sum
summary_join <- full_join(tot_award_sum, tot_losses_sum)



########### SUMMARY STATISTICS OF GIVEN DATA ####################
# Step 1: plot price and quantity

#ggsummarystats(stripedf, x="price", y = "offer_quantity", color="awarded")

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

sum_awarded <- stripedf %>% group_by(awarded) %>% 
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE))


#### READ IN GOOGLE PATENT DATA ############

gpatent_data <-  read_csv("data/stripe_data/cleaned_outputs/google_patent_data_manual_CSV.csv", 
                          col_types = cols(file_date = col_date(format = "%d/%m/%Y")))

full_df <- merge(stripedf, gpatent_data, by=c("applicant_name", "year", "quarter"))

full_df_sum <- full_df %>% group_by(applicant_name, year, quarter) %>%
  mutate(sum_patents = n()) %>%
  arrange(file_date) 

post_prize_patent <- full_df_sum %>%
  summarise(count = sum(as.integer(format(file_date, "%Y")) > year, na.rm = TRUE))

full_df_sum <- full_df_sum %>% ungroup()

df_regtest <- merge(full_df_sum, post_prize_patent, by = c("applicant_name", "year", "quarter")) %>%
  rename(post_app_patents = "count") %>% distinct(applicant_name, year, quarter, .keep_all = TRUE) %>%
  arrange(applicant_name, year) %>% ungroup()

# fill in missing data
df_regtest$year <- as.numeric(format(df_regtest$file_date, "%Y"))
# convert quarters to month - this way we can better estimate pre- and post-award patents


any_na <- any(is.na(df_regtest$sum_patents))
any_na1 <- any(is.na(df_regtest$post_app_patents))

# Output the result
if (any_na1) {
  cat("The column contains NA values.")
} else {
  cat("The column does not contain NA values.")
}
  
############## reg on patent output

patent_outcome <- plm(post_app_patents ~ awarded_factor + year, data = df_regtest, index=c("applicant_name", "year", "quarter"))

patent_outcome_1 <- glm(post_app_patents ~ awarded_factor + year, data = df_regtest)

patent_outcome_2 <- lm(post_app_patents ~ awarded_factor + year, data = df_regtest)

patent_outcome_3 <- lm(post_app_patents ~ awarded_factor + year, data = df_regtest)

# some results showing when you control for previous patents
patent_outcome_4 <- lm(post_app_patents ~ awarded_factor + year + (sum_patents - post_app_patents), data = df_regtest)

patent_outcome_5 <- lm(post_app_patents ~ awarded_factor + year + (sum_patents - post_app_patents) + offer_quantity, data = df_regtest)

# not seeing anything here 
patent_outcome_6 <- lm(post_app_patents ~ awarded_factor + year + (sum_patents - post_app_patents) + offer_quantity + price, data = df_regtest)
patent_outcome_7 <- lm(post_app_patents ~ awarded_factor + year + df_regtest$'lifetime mtco2e' + realised_cost_decrease, data = df_regtest)


# running the opposite- direction --- no effect
award_outcome <- glm(awarded_factor ~ (sum_patents - post_app_patents), data = df_regtest, family = binomial(link="logit"))



####################### SUMMARY STATISTICS AGAIN ########################
# plot the variables by award/not award

# summary stat on some of the variables --- to functionalize
summary(df_regtest$price)
mean_val <- mean(df_regtest$price, na.rm=TRUE)
sd_val <- sd(df_regtest$price, na.rm=TRUE)

filtered_data <- df_regtest %>%
  filter(price <= mean_val + 2 * sd_val)

plot(density(filtered_data$price), main = "Density Plot without Outliers", xlab = "Offer Price for CO2 tonne")
# compare with out outliers to the one with -- you can see we get more information
boxplot(df_regtest$price, col = "lightgreen", main = "Boxplot of Variable")
hist(df_regtest$price, col = "skyblue", main = "Histogram of Variable")

variable_plots <- function(var, list) {
  
}

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


ggplot(df_regtest, aes(x = offer_quantity, y = price, color = awarded_factor)) +
  geom_point() +
  labs(x = "X-axis Label", y = "Y-axis Label", color = "Awarded") +
  scale_color_discrete(name = "Awarded", labels = c("Not Awarded", "Awarded"))


ggplot(df_regtest, aes(x = current_cost_per_tonne, y = price, color = awarded_factor)) +
  geom_point() +
  labs(x = "X-axis Label", y = "Y-axis Label", color = "Awarded") +
  scale_color_discrete(name = "Awarded", labels = c("Not Awarded", "Awarded"))

# Plot 1: Patent count vs. Awarded/not awarded

ggplot(df_regtest, aes(x = current_cost_per_tonne, y = price, color = awarded_factor)) +
  geom_point() +
  labs(x = "X-axis Label", y = "Y-axis Label", color = "Awarded") +
  scale_color_discrete(name = "Awarded", labels = c("Not Awarded", "Awarded"))


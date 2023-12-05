# summary statistics on STRIPE data 
library(readr)
library(readxl)
library(plm)
library(tidyr)
library(tidyverse)
library(dplyr)
library(survival)
library(ggplot2)
library(ggpubr)


# Read in data
stripedf <- read_excel("C:/Users/SAGGESE/Documents/GitHub/amc_ccs/data/stripe_data/cleaned_outputs/manual-aggregated-firmlist.xlsx", 
                       col_types = c("text", "numeric", "numeric", 
                                     "numeric", "numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
# quick cleaning
stripedf$price <- as.numeric(as.character(stripedf$price))
#change awarded to factor var for regression
stripedf$awarded_factor <- as.factor(stripedf$awarded) 




########### REGRESSION ANALYSIS: FIRM CHARACTERISTICS ON WINNING/LOSING ####################
# Step 1: Early analysis 

# test logit regressions 
no_control <- glm(formula = awarded_factor ~ price, data = stripedf, family = binomial(link="logit"))
summary(no_control) # no impact of price

stripedf$realised_cost_decrease <- (stripedf$current_cost_per_tonne-stripedf$price)*(-1) # create var for difference in cost

diff_realised_price <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year, data = stripedf, family = binomial(link="logit"))
summary(diff_realised_price)


# consider 
diff_2 <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year + offer_quantity, data = stripedf, family = binomial(link="logit"))
summary(diff_2)

diff_3 <- glm(formula = awarded_factor ~ (realised_cost_decrease) + year + stripedf$`lifetime mtco2e`, data = stripedf, family = binomial(link="logit"))
summary(diff_3)

long_reg <- glm(formula = awarded_factor ~ price + offer_quantity + price*offer_quantity + year, data = stripedf, family = binomial(link="logit"))
summary(long_reg)


# summary statistics
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

full_df_sum <- 



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

#change awarded to factor var for regression
stripedf[,'awarded'] <- as.factor(stripedf[,'awarded']) 

# test logit regressions 
no_control <- glm(formula = awarded ~ price, data = stripedf, family = binomial(link="logit"))
summary(no_control) # no impact of price

stripedf$realised_cost_decrease <- (stripedf$current_cost_per_tonne-stripedf$price) # create var for difference in cost

diff_realised_price <- glm(formula = awarded ~ (realised_cost_decrease) + year, data = stripedf, family = binomial(link="logit"))
summary(diff_realised_price)

diff_2 <- glm(formula = awarded ~ (realised_cost_decrease) + year + offer_quantity, data = stripedf, family = binomial(link="logit"))
summary(diff_2)

long_reg <- glm(formula = awarded ~ price + offer_quantity + price*offer_quantity + year, data = stripedf, family = binomial(link="logit"))
summary(long_reg)


# summary statistics
sum_1 <- stripedf %>% group_by(year) %>% 
  summarise(total_count=n(),.groups='drop') %>%
  as.data.frame()

stripedf$awardednum <- as.numeric(stripedf$awarded)
award_onlydf <- stripedf %>% filter(awarded>0)
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


# ggplot 
#ggsummarystats(stripedf, x="price", y = "offer_quantity", color="awarded")

plotdata <- stripedf %>% filter(price<1000000)
ggplot(data=plotdata, aes(x=year, y=price))+ geom_point()


price_histo <- ggplot(data=plotdata, aes(x=price))+ geom_histogram(bins=20)
price_histo_winners <- ggplot(data=award_onlydf, aes(x=price))+ geom_histogram(bins=20)
price_histo_losers <- ggplot(data=prop_onlydf, aes(x=price))+ geom_histogram(bins=20)


#make awarded numeric again
stripedf[,'awarded'] <- as.numeric(stripedf[,'awarded']) 

sum_awarded <- stripedf %>% group_by(awarded) %>% 
  summarise(across(price:offer_quantity, .f = list(sum = sum, mean = mean, max = max, sd = sd), na.rm = TRUE))



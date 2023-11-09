# summary statistics on STRIPE data 

library(readr)
library(readxl)

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

stripedf[,'awarded'] <- as.factor(stripedf[,'awarded'])

# test logit regression
no_control <- glm(formula = awarded ~ price, data = stripedf, family = binomial(link="logit"))
summary(no_control) # no impact of price

stripedf$realised_cost_decrease <- (stripedf$current_cost_per_tonne-stripedf$price)
diff_realised_price <- glm(formula = awarded ~ (current_cost_per_tonne-price) + year, data = stripedf, family = binomial(link="logit"))
summary(diff_realised_price)

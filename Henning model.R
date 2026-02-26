setwd()
# Libraries
library(tidyverse)
library(dplyr)
library(mFilter)
library(stargazer)
library(lmtest)
library(ggplot2)

# Load Data
gdp <- read.csv("gdp.csv")
pb_finance <- read.csv("public_finance.csv")
remittance <- read.csv("remittance.csv")
str(gdp)
str(pb_finance)
str(remittance)

# Creating required dataframe
merged_df <- gdp %>%
  full_join(pb_finance, by = "year") %>%
  full_join(remittance, by = "year") %>%
  arrange(year) %>%
  mutate(debt_gdp = round(outstanding_debt/current_gdp,4),
         pb_gdp = round((revenue - recurrent_exp - capital_exp)/current_gdp,4),
         debt_gdp_lag = lag(debt_gdp,1),
         debt_gdp_lag_sq = debt_gdp_lag^2,
         spend = recurrent_exp + capital_exp,
         spend_gdp = round(spend/current_gdp,4),
         remittance_gdp = round(remittance/current_gdp,4)) %>%
  filter(year >= 1976 & year <= 2025)

# output gap using HP filter
log_real_gdp <- log(merged_df$rebased_real_gdp)
hp_gdp <- mFilter::hpfilter(log_real_gdp, freq = 100, type = "lambda")
merged_df$output_gap <- 100*(log_real_gdp - hp_gdp$trend)
merged_df$potential_gdp <- exp(log(merged_df$rebased_real_gdp)-merged_df$output_gap/100)

# Temporary Government Spending
hp_spend <- mFilter::hpfilter(merged_df$spend_gdp, freq = 100, type = "lambda")
merged_df$temp_spend <- merged_df$spend_gdp-hp_spend$trend

# Quadratic Model
henning_quad <- lm(pb_gdp ~ debt_gdp_lag + debt_gdp_lag_sq + temp_spend + output_gap + remittance_gdp, data = merged_df)
summary(henning_quad)
nw_vcov <- NeweyWest(henning_quad, lag = 1, prewhite = FALSE)
nw_test <- coeftest(henning_quad, vcov = nw_vcov)
print(nw_test[, "t value"])
dwtest(henning_quad)

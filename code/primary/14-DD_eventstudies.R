library(here) # set working directory
library(modelsummary)
library(tidyverse)
library(webshot2)

# set loc
here::i_am("code/primary/14-DD_eventstudies.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E1980.Rda"))
load(here("data", "clean", "DD_O1980.Rda"))
load(here("data", "clean", "DD_E2010.Rda"))
load(here("data", "clean", "DD_O2010.Rda"))

# drop post treatment periods
DD_E1980 <- DD_E1980 %>% filter(dPost == 0)
DD_O1980 <- DD_O1980 %>% filter(dPost == 0)
DD_E2010 <- DD_E2010 %>% filter(dPost == 0)
DD_O2010 <- DD_O2010 %>% filter(dPost == 0)

# regression model evaluating dIR (covariates based on best model evaluation)
# - 1980 
eventE_1980 <- lm(standard_ct ~ dIR + Observer, DD_E1980)
eventO_1980 <- lm(standard_ct ~ dIR + lagPDO + Observer, DD_O1980)

# - 2010 
eventE_2010 <- lm(standard_ct ~ dIR + lagPDO + Observer , DD_E2010)
eventO_2010 <- lm(standard_ct ~ dIR + Observer, DD_O2010)

# check mods
models <- list("Even, 1980" = eventE_1980, 
               "Odd, 1980" = eventO_1980,
               "Even, 2010" = eventE_2010,
               "Odd, 2010" = eventO_2010)

# build results tabls
modelsummary(models,
             coef_omit = c(1, 3:10),
             coef_rename = c("dIR" = "Indian River Effect"),
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik|RMSE|F',
             title = "Event studies: Indian River pink salmon difference-in-difference models"
             , output = here("output", "figures", "eventstudies_table.png")
)

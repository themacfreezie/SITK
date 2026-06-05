library(here) # set working directory
library(modelsummary)
library(tidyverse)
library(webshot2)

# set loc
here::i_am("code/primary/b05-DD_eventstudies.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E1975.Rda"))
load(here("data", "clean", "DD_O1975.Rda"))
load(here("data", "clean", "DD_E2010.Rda"))
load(here("data", "clean", "DD_O2010.Rda"))

# drop post treatment periods
DD_E1975 <- DD_E1975 %>% filter(dPost == 0)
DD_O1975 <- DD_O1975 %>% filter(dPost == 0)
DD_E2010 <- DD_E2010 %>% filter(dPost == 0)
DD_O2010 <- DD_O2010 %>% filter(dPost == 0)

# regression model evaluating dIR (covariates based on best model evaluation)
# - 1975 
eventE_1975 <- lm(standard_ct ~ dIR + Observer, DD_E1975)
eventO_1975 <- lm(standard_ct ~ dIR + lagPDO + Observer, DD_O1975)
# eventO_1975 <- lm(standard_ct ~ dIR + Observer, DD_O1975)

# - 2010 
eventE_2010 <- lm(standard_ct ~ dIR + lagPDO + Observer , DD_E2010)
# eventE_2010 <- lm(standard_ct ~ dIR + Observer , DD_E2010)
eventO_2010 <- lm(standard_ct ~ dIR + Observer, DD_O2010)

# check mods
models <- list("Even, 1975" = eventE_1975, 
               "Odd, 1975" = eventO_1975,
               "Even, 2010" = eventE_2010,
               "Odd, 2010" = eventO_2010)

# build results tabls
modelsummary(models,
             coef_omit = c(1, 3:10),
             coef_rename = c("dIR" = "Indian River Effect"),
             stars = c("*" = 0.05),
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik|RMSE|F',
             title = "Event studies: Indian River pink salmon difference-in-difference models"
             # , output = here("output", "figures", "eventstudies_table.png")
)

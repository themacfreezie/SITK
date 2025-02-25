library(here) # set workind directory

# set loc
here::i_am("code/development/DD/03-DD_regression.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E.Rda"))
load(here("data", "clean", "DD_O.Rda"))

# basic DD regression model
DDmodE_base <- lm(standard_ct ~ dPost + dIR + dIR*dPost , DD_E)
DDmodO_base <- lm(standard_ct ~ dPost + dIR + dIR*dPost , DD_O)

# DD regression model + environmental covariates
DDmodE_envi <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO , DD_E)
DDmodO_envi <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO , DD_O)

# DD regression model + observer covariates
DDmodE_obsv <- lm(standard_ct ~ dPost + dIR + dIR*dPost + Observer , DD_E)
DDmodO_obsv <- lm(standard_ct ~ dPost + dIR + dIR*dPost + Observer , DD_O)

# DD regression model + environmental and observer covariates
DDmodE_both <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO + Observer , DD_E)
DDmodO_both <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO + Observer , DD_O)

# compare models - even years
summary(DDmodE_base)
summary(DDmodE_envi)
summary(DDmodE_obsv)
summary(DDmodE_both)

anova(DDmodE_base, DDmodE_envi)
anova(DDmodE_base, DDmodE_obsv)
anova(DDmodE_envi, DDmodE_both)
  # pdo model is stronger than base
  # observer doesn't seem to make any difference to base model
  # lagged pdo may be capturing other year-level fixed effects

# compare models - odd years
summary(DDmodO_base)
summary(DDmodO_envi)
summary(DDmodO_obsv)
summary(DDmodO_both)

anova(DDmodO_base, DDmodO_envi)
anova(DDmodO_base, DDmodO_obsv)
anova(DDmodO_envi, DDmodO_both)
anova(DDmodO_obsv, DDmodO_both)
  # observer model and pdo model are both stronger than base
  # combined model is stronger than each single covariate model
  # lagged pdo may be capturing other year-level fixed effects

# seems like these are best based on anova
summary(DDmodE_envi)
summary(DDmodO_both)

# what about aic? 
AIC(DDmodE_base, k=2)
AIC(DDmodE_envi, k=2) # best
AIC(DDmodE_obsv, k=2)
AIC(DDmodE_both, k=2)

AIC(DDmodO_base, k=2)
AIC(DDmodO_envi, k=2)
AIC(DDmodO_obsv, k=2)
AIC(DDmodO_both, k=2) # best

## SET WORKING DIR & PACKAGES
library(here)

# set loc
here::i_am("code/primary/17-DD_modeleval.R")
options(max.print=2000)

# load in ss models
DDmodE_base1980 <- readRDS(file=here("data", "clean", "DDmodE_base1980.rds"))
DDmodO_base1980 <- readRDS(file=here("data", "clean", "DDmodO_base1980.rds"))
DDmodE_base2010 <- readRDS(file=here("data", "clean", "DDmodE_base2010.rds"))
DDmodO_base2010 <- readRDS(file=here("data", "clean", "DDmodO_base2010.rds"))

DDmodE_envi1980 <- readRDS(file=here("data", "clean", "DDmodE_envi1980.rds"))
DDmodO_envi1980 <- readRDS(file=here("data", "clean", "DDmodO_envi1980.rds"))
DDmodE_envi2010 <- readRDS(file=here("data", "clean", "DDmodE_envi2010.rds"))
DDmodO_envi2010 <- readRDS(file=here("data", "clean", "DDmodO_envi2010.rds"))

DDmodE_obsv1980 <- readRDS(file=here("data", "clean", "DDmodE_obsv1980.rds"))
DDmodO_obsv1980 <- readRDS(file=here("data", "clean", "DDmodO_obsv1980.rds"))
DDmodE_obsv2010 <- readRDS(file=here("data", "clean", "DDmodE_obsv2010.rds"))
DDmodO_obsv2010 <- readRDS(file=here("data", "clean", "DDmodO_obsv2010.rds"))

DDmodE_both1980 <- readRDS(file=here("data", "clean", "DDmodE_both1980.rds"))
DDmodO_both1980 <- readRDS(file=here("data", "clean", "DDmodO_both1980.rds"))
DDmodE_both2010 <- readRDS(file=here("data", "clean", "DDmodE_both2010.rds"))
DDmodO_both2010 <- readRDS(file=here("data", "clean", "DDmodO_both2010.rds"))

# compare models - even years
summary(DDmodE_base1980)
summary(DDmodE_envi1980)
summary(DDmodE_obsv1980)
summary(DDmodE_both1980)

summary(DDmodE_base2010)
summary(DDmodE_envi2010)
summary(DDmodE_obsv2010)
summary(DDmodE_both2010)

# compare models - odd years
summary(DDmodO_base1980)
summary(DDmodO_envi1980)
summary(DDmodO_obsv1980)
summary(DDmodO_both1980)

summary(DDmodO_base2010)
summary(DDmodO_envi2010)
summary(DDmodO_obsv2010)
summary(DDmodO_both2010)



anova(DDmodE_base, DDmodE_envi)
anova(DDmodE_base, DDmodE_obsv)
anova(DDmodE_envi, DDmodE_both)
anova(DDmode_obsv, DDmode_both)
# pdo model is stronger than base
# observer doesn't seem to make any difference to base model
# lagged pdo may be capturing other year-level fixed effects

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

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

# ANOVA

# 1980 - even
anova(DDmodE_base1980, DDmodE_envi1980)
anova(DDmodE_base1980, DDmodE_obsv1980)
anova(DDmodE_envi1980, DDmodE_both1980)
anova(DDmodE_obsv1980, DDmodE_both1980)
# pdo is significantly stronger than base
  # could lagged pdo may be capturing other year-level fixed effects?
# observer is significantly stronger than base
# full is significantly stronger than pdo
# full is not significantly stronger than observer 
# observer is best?

# 2010 - even
anova(DDmodE_base2010, DDmodE_envi2010)
anova(DDmodE_base2010, DDmodE_obsv2010)
anova(DDmodE_envi2010, DDmodE_both2010)
anova(DDmodE_obsv2010, DDmodE_both2010)
# pdo is not significantly stronger than base
# observer is significantly stronger than base
# full is significantly stronger than pdo
# full is (weakly) significantly stronger than observer
# full is best?

anova(DDmodO_base1980, DDmodO_envi1980)
anova(DDmodO_base1980, DDmodO_obsv1980)
anova(DDmodO_envi1980, DDmodO_both1980)
anova(DDmodO_obsv1980, DDmodO_both1980)
# pdo is significantly stronger than base
  # could lagged pdo may be capturing other year-level fixed effects?
# observer is significantly stronger than base
# full is significantly stronger than pdo
# full is not significantly stronger than observer 
# full is best?

anova(DDmodO_base2010, DDmodO_envi2010)
anova(DDmodO_base2010, DDmodO_obsv2010)
anova(DDmodO_envi2010, DDmodO_both2010)
anova(DDmodO_obsv2010, DDmodO_both2010)
# pdo is (weakly) significantly stronger than base
# observer is significantly stronger than base
# full is significantly stronger than pdo
# full is not significantly stronger than observer 
# full is best?

# seems like these are best based on anova
summary(DDmodE_obsv1980) # hmm... are we sure here?
summary(DDmodO_both2010)
summary(DDmodE_both1980)
summary(DDmodO_both2010)

# what about aic? 
AIC(DDmodE_base1980, k=2)
AIC(DDmodE_envi1980, k=2)
AIC(DDmodE_obsv1980, k=2) # best (< 2)
AIC(DDmodE_both1980, k=2) # 2nd best

AIC(DDmodE_base2010, k=2)
AIC(DDmodE_envi2010, k=2)
AIC(DDmodE_obsv2010, k=2)
AIC(DDmodE_both2010, k=2) # best

AIC(DDmodO_base1980, k=2)
AIC(DDmodO_envi1980, k=2)
AIC(DDmodO_obsv1980, k=2) # 2nd best
AIC(DDmodO_both1980, k=2) # best (<2)

AIC(DDmodO_base2010, k=2)
AIC(DDmodO_envi2010, k=2)
AIC(DDmodO_obsv2010, k=2) # best
AIC(DDmodO_both2010, k=2) # 2nd best

# seems like these are best based on aic
summary(DDmodE_obsv1980) 
summary(DDmodO_both2010)
summary(DDmodE_obsv1980) # deltAIC < 2 & simpler model
summary(DDmodO_obsv2010)
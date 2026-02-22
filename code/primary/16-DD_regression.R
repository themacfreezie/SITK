library(here) # set working directory

# set loc
here::i_am("code/primary/16-DD_regression.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E.Rda"))
load(here("data", "clean", "DD_O.Rda"))

# basic DD regression model 
# - 1980 
DDmodE_base1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980, DD_E)
DDmodO_base1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980, DD_O)
saveRDS(DDmodE_base1980, file=here("data", "clean", "DDmodE_base1980.rds"))
saveRDS(DDmodO_base1980, file=here("data", "clean", "DDmodO_base1980.rds"))


# - 2010 
DDmodE_base2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010, DD_E)
DDmodO_base2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010, DD_O)
saveRDS(DDmodE_base2010, file=here("data", "clean", "DDmodE_base2010.rds"))
saveRDS(DDmodO_base2010, file=here("data", "clean", "DDmodO_base2010.rds"))


# DD regression model + environmental covariates
# - 1980
DDmodE_envi1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980 + lagPDO, DD_E)
DDmodO_envi1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980 + lagPDO, DD_O) 
saveRDS(DDmodE_envi1980, file=here("data", "clean", "DDmodE_envi1980.rds"))
saveRDS(DDmodO_envi1980, file=here("data", "clean", "DDmodO_envi1980.rds"))

# - 2010
DDmodE_envi2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010 + lagPDO, DD_E)
DDmodO_envi2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010 + lagPDO, DD_O) 
saveRDS(DDmodE_envi2010, file=here("data", "clean", "DDmodE_envi2010.rds"))
saveRDS(DDmodO_envi2010, file=here("data", "clean", "DDmodO_envi2010.rds"))


# DD regression model + observer covariates
# - 1980
DDmodE_obsv1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980 + Observer, DD_E)
DDmodO_obsv1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980 + Observer, DD_O)
saveRDS(DDmodE_obsv1980, file=here("data", "clean", "DDmodE_obsv1980.rds"))
saveRDS(DDmodO_obsv1980, file=here("data", "clean", "DDmodO_obsv1980.rds"))

# - 2010
DDmodE_obsv2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010 + Observer, DD_E)
DDmodO_obsv2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010 + Observer, DD_O)
saveRDS(DDmodE_obsv2010, file=here("data", "clean", "DDmodE_obsv2010.rds"))
saveRDS(DDmodO_obsv2010, file=here("data", "clean", "DDmodO_obsv2010.rds"))


# DD regression model + environmental and observer covariates
# - 1980
DDmodE_both1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980 + lagPDO + Observer , DD_E)
DDmodO_both1980 <- lm(standard_ct ~ dPost1980 + dIR + dIR*dPost1980+ lagPDO + Observer , DD_O)
saveRDS(DDmodE_both1980, file=here("data", "clean", "DDmodE_both1980.rds"))
saveRDS(DDmodO_both1980, file=here("data", "clean", "DDmodO_both1980.rds"))

# - 2010
DDmodE_both2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010 + lagPDO + Observer , DD_E)
DDmodO_both2010 <- lm(standard_ct ~ dPost2010 + dIR + dIR*dPost2010+ lagPDO + Observer , DD_O)
saveRDS(DDmodE_both2010, file=here("data", "clean", "DDmodE_both2010.rds"))
saveRDS(DDmodO_both2010, file=here("data", "clean", "DDmodO_both2010.rds"))
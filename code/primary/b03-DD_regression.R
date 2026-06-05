library(here) # set working directory

# set loc
here::i_am("code/primary/b03-DD_regression.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E1975.Rda"))
load(here("data", "clean", "DD_O1975.Rda"))
load(here("data", "clean", "DD_E2010.Rda"))
load(here("data", "clean", "DD_O2010.Rda"))

# basic DD regression model 
# - 1975 
DDmodE_base1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_E1975)
DDmodO_base1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_O1975)
saveRDS(DDmodE_base1975, file=here("data", "clean", "DDmodE_base1975.rds"))
saveRDS(DDmodO_base1975, file=here("data", "clean", "DDmodO_base1975.rds"))


# - 2010 
DDmodE_base2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_E2010)
DDmodO_base2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_O2010)
saveRDS(DDmodE_base2010, file=here("data", "clean", "DDmodE_base2010.rds"))
saveRDS(DDmodO_base2010, file=here("data", "clean", "DDmodO_base2010.rds"))


# DD regression model + environmental covariates
# - 1975
DDmodE_envi1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO, DD_E1975)
DDmodO_envi1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO, DD_O1975) 
saveRDS(DDmodE_envi1975, file=here("data", "clean", "DDmodE_envi1975.rds"))
saveRDS(DDmodO_envi1975, file=here("data", "clean", "DDmodO_envi1975.rds"))

# - 2010
DDmodE_envi2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO, DD_E2010)
DDmodO_envi2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO, DD_O2010) 
saveRDS(DDmodE_envi2010, file=here("data", "clean", "DDmodE_envi2010.rds"))
saveRDS(DDmodO_envi2010, file=here("data", "clean", "DDmodO_envi2010.rds"))


# DD regression model + observer covariates
# - 1975
DDmodE_obsv1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + Observer, DD_E1975)
DDmodO_obsv1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + Observer, DD_O1975)
saveRDS(DDmodE_obsv1975, file=here("data", "clean", "DDmodE_obsv1975.rds"))
saveRDS(DDmodO_obsv1975, file=here("data", "clean", "DDmodO_obsv1975.rds"))

# - 2010
DDmodE_obsv2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + Observer, DD_E2010)
DDmodO_obsv2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + Observer, DD_O2010)
saveRDS(DDmodE_obsv2010, file=here("data", "clean", "DDmodE_obsv2010.rds"))
saveRDS(DDmodO_obsv2010, file=here("data", "clean", "DDmodO_obsv2010.rds"))


# DD regression model + environmental and observer covariates
# - 1975
DDmodE_both1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO + Observer , DD_E1975)
DDmodO_both1975 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO + Observer , DD_O1975)
saveRDS(DDmodE_both1975, file=here("data", "clean", "DDmodE_both1975.rds"))
saveRDS(DDmodO_both1975, file=here("data", "clean", "DDmodO_both1975.rds"))

# - 2010
DDmodE_both2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost + lagPDO + Observer , DD_E2010)
DDmodO_both2010 <- lm(standard_ct ~ dPost + dIR + dIR*dPost+ lagPDO + Observer , DD_O2010)
saveRDS(DDmodE_both2010, file=here("data", "clean", "DDmodE_both2010.rds"))
saveRDS(DDmodO_both2010, file=here("data", "clean", "DDmodO_both2010.rds"))
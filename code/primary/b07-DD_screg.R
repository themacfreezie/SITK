library(here) # set working directory

# set loc
here::i_am("code/primary/b07-DD_screg.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "synthcontrol_E1975.Rda"))
# load(here("data", "clean", "synthcontrol_E1980.Rda"))
load(here("data", "clean", "synthcontrol_O1975.Rda"))
load(here("data", "clean", "DD_E1975.Rda"))
load(here("data", "clean", "DD_O1975.Rda"))

# must filter for just Indian River (to regress w/ synth control)
DD_E1975 <- DD_E1975 %>%
  filter(STREAMID == "113-41-019")
DD_O1975 <- DD_O1975 %>%
  filter(STREAMID == "113-41-019")

# drop variables that won't match
DD_E1975 <- DD_E1975[-c(2,6,7)]
DD_O1975 <- DD_O1975[-c(2,6,7)]

# bind datesets
DD_E1975 <- rbind(DD_E1975, synthcontrol_E1975)
DD_O1975 <- rbind(DD_O1975, synthcontrol_E1975)

# check linear trends assumption w/ synthetic data
DD_E1975pre <- DD_E1975 %>% filter(dPost == 0)
DD_O1975pre <- DD_O1975 %>% filter(dPost == 0)

# regression model evaluating dIR (no covariates due to sc)
# - 1975 
eventE_1975 <- lm(standard_ct ~ dIR, DD_E1975pre)
summary(eventE_1975)
  # passes check!
eventO_1975 <- lm(standard_ct ~ dIR, DD_O1975pre)
summary(eventO_1975)
  # passes check!

# basic DD regression model (no covariates due to sc)
DDmodE_base1975synth <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_E1975)
summary(DDmodE_base1975synth)
saveRDS(DDmodE_base1975synth, file=here("data", "clean", "DDmodE_base1975synth.rds"))

DDmodO_base1975synth <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_O1975)
summary(DDmodO_base1975synth)
saveRDS(DDmodO_base1975synth, file=here("data", "clean", "DDmodO_base1975synth.rds"))

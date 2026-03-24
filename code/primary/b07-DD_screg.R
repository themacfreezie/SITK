library(here) # set working directory

# set loc
here::i_am("code/primary/b07-DD_screg.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "synthcontrol_E1980.Rda"))
load(here("data", "clean", "DD_E1980.Rda"))

# must filter for just Indian River (to regress w/ synth control)
DD_E1980 <- DD_E1980 %>%
  filter(STREAMID == "113-41-019")

# drop variables that won't match
DD_E1980 <- DD_E1980[-c(2,6,7)]

# bind datesets
DD_E1980 <- rbind(DD_E1980, synthcontrol_E1980)

# check linear trends assumption w/ synthetic data
DD_E1980pre <- DD_E1980 %>% filter(dPost == 0)

# regression model evaluating dIR (no covariates due to sc)
# - 1980 
eventE_1980 <- lm(standard_ct ~ dIR, DD_E1980pre)
summary(eventE_1980)
  # passes check!

# basic DD regression model (no covariates due to sc)
# - 1980 
DDmodE_base1980synth <- lm(standard_ct ~ dPost + dIR + dIR*dPost, DD_E1980)
summary(DDmodE_base1980synth)
saveRDS(DDmodE_base1980synth, file=here("data", "clean", "DDmodE_base1980synth.rds"))

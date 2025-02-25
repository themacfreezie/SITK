library(here) # set workind directory

# set loc
here::i_am("code/development/DD/03-DD_regression.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_pinksE_scst.Rda"))
load(here("data", "clean", "DD_pinksO_scst.Rda"))

# basic DD regression model
DDmod_E <- lm(standard_ct ~ dPost + dIR + dIR*dPost , DD_pinksE_scst.df)
summary(DDmod_E)

DDmod_O <- lm(standard_ct ~ dPost + dIR + dIR*dPost , DD_pinksO_scst.df)
summary(DDmod_O)

## SET WORKING DIR & PACKAGES

# import packages
library(dplyr, warn.conflicts = FALSE)
library(here)
library(panelr)
library(readxl)

# create working dir and output folder
here::i_am("code/development/Betas/02-WIDEstandardize.R")
options(max.print=10000)

# pull in data
load(here("data", "clean", "beta_dataE.Rda"))
load(here("data", "clean", "beta_dataO.Rda"))

# split each df into three (for each measure of hatchery returns)
betaE_AMPr.df <- beta_dataE.df[-c(3,4)]
betaO_AMPr.df <- beta_dataO.df[-c(3,4)]

betaE_DFGr.df <- beta_dataE.df[-c(2,3)]
betaO_DFGr.df <- beta_dataO.df[-c(2,3)]

betaE_DFGe.df <- beta_dataE.df[-c(2,4)]
betaO_DFGe.df <- beta_dataO.df[-c(2,4)]

# set them all long 
betaE_AMPr.df <- cbind(
  year = rep(betaE_AMPr.df$return_year, 2),
  stack(betaE_AMPr.df[, c("AMPreturners", "IRpeak_count")])
)
names(betaE_AMPr.df)[names(betaE_AMPr.df) == "ind"] <- "measure"
names(betaE_AMPr.df)[names(betaE_AMPr.df) == "values"] <- "ct"

betaO_AMPr.df <- cbind(
  year = rep(betaO_AMPr.df$return_year, 2),
  stack(betaO_AMPr.df[, c("AMPreturners", "IRpeak_count")])
)
names(betaO_AMPr.df)[names(betaO_AMPr.df) == "ind"] <- "measure"
names(betaO_AMPr.df)[names(betaO_AMPr.df) == "values"] <- "ct"

betaE_DFGr.df <- cbind(
  year = rep(betaE_DFGr.df$return_year, 2),
  stack(betaE_DFGr.df[, c("DFGreturners", "IRpeak_count")])
)
names(betaE_DFGr.df)[names(betaE_DFGr.df) == "ind"] <- "measure"
names(betaE_DFGr.df)[names(betaE_DFGr.df) == "values"] <- "ct"

betaO_DFGr.df <- cbind(
  year = rep(betaO_DFGr.df$return_year, 2),
  stack(betaO_DFGr.df[, c("DFGreturners", "IRpeak_count")])
)
names(betaO_DFGr.df)[names(betaO_DFGr.df) == "ind"] <- "measure"
names(betaO_DFGr.df)[names(betaO_DFGr.df) == "values"] <- "ct"

betaE_DFGe.df <- cbind(
  year = rep(betaE_DFGe.df$return_year, 2),
  stack(betaE_DFGe.df[, c("DFGescape", "IRpeak_count")])
)
names(betaE_DFGe.df)[names(betaE_DFGe.df) == "ind"] <- "measure"
names(betaE_DFGe.df)[names(betaE_DFGe.df) == "values"] <- "ct"

betaO_DFGe.df <- cbind(
  year = rep(betaO_DFGe.df$return_year, 2),
  stack(betaO_DFGe.df[, c("DFGescape", "IRpeak_count")])
)
names(betaO_DFGe.df)[names(betaO_DFGe.df) == "ind"] <- "measure"
names(betaO_DFGe.df)[names(betaO_DFGe.df) == "values"] <- "ct"

# natural log of count variable
betaE_AMPr.df$ct <- betaE_AMPr.df$ct + 1
betaO_AMPr.df$ct <- betaO_AMPr.df$ct + 1
betaE_DFGr.df$ct <- betaE_DFGr.df$ct + 1
betaO_DFGr.df$ct <- betaO_DFGr.df$ct + 1
betaE_DFGe.df$ct <- betaE_DFGe.df$ct + 1
betaO_DFGe.df$ct <- betaO_DFGe.df$ct + 1

betaE_AMPr.df$ct <- log(betaE_AMPr.df$ct)
betaO_AMPr.df$ct <- log(betaO_AMPr.df$ct)
betaE_DFGr.df$ct <- log(betaE_DFGr.df$ct)
betaO_DFGr.df$ct <- log(betaO_DFGr.df$ct)
betaE_DFGe.df$ct <- log(betaE_DFGe.df$ct)
betaO_DFGe.df$ct <- log(betaO_DFGe.df$ct)

# should these data be standardized at the level of each state? 
# they are different measures...

# set data wide (rows = IDs, columns = year)
betaE_AMPr.df <- panel_data(betaE_AMPr.df, id = measure, wave = year)
WbetaE_AMPr.df <- widen_panel(betaE_AMPr.df, separator = "_")

betaO_AMPr.df <- panel_data(betaO_AMPr.df, id = measure, wave = year)
WbetaO_AMPr.df <- widen_panel(betaO_AMPr.df, separator = "_")

betaE_DFGr.df <- panel_data(betaE_DFGr.df, id = measure, wave = year)
WbetaE_DFGr.df <- widen_panel(betaE_DFGr.df, separator = "_")

betaO_DFGr.df <- panel_data(betaO_DFGr.df, id = measure, wave = year)
WbetaO_DFGr.df <- widen_panel(betaO_DFGr.df, separator = "_")

betaE_DFGe.df <- panel_data(betaE_DFGe.df, id = measure, wave = year)
WbetaE_DFGe.df <- widen_panel(betaE_DFGe.df, separator = "_")

betaO_DFGe.df <- panel_data(betaO_DFGe.df, id = measure, wave = year)
WbetaO_DFGe.df <- widen_panel(betaO_DFGe.df, separator = "_")

# save wide dataframes
save(WbetaE_AMPr.df, file=here("data", "clean", "WbetaE_AMPr.Rda"))
save(WbetaO_AMPr.df, file=here("data", "clean", "WbetaO_AMPr.Rda"))
save(WbetaE_DFGr.df, file=here("data", "clean", "WbetaE_DFGr.Rda"))
save(WbetaO_DFGr.df, file=here("data", "clean", "WbetaO_DFGr.Rda"))
save(WbetaE_DFGe.df, file=here("data", "clean", "WbetaE_DFGe.Rda"))
save(WbetaO_DFGe.df, file=here("data", "clean", "WbetaO_DFGe.Rda"))


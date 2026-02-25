library(dplyr)
library(fastDummies)
library(imputeTS)
library(here) # set working directory
library(tidysynth)

# set loc
here::i_am("code/primary/22-DD_syntheticcontrols.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E1980.Rda"))

# dummy variables for factor (observer)
DD_E1980test <- dummy_cols(
  DD_E1980
)
DD_E1980 <- DD_E1980test[-c(7:43, 54)]

# missing data for Indian River
  # 1960, 1968, 1970, 1974, 1976 (oy)

# add missing rows
new_ob1960 <- data.frame(YEAR = 1960, 
                     STREAMID = "113-41-019", 
                     standard_ct = NA, 
                     dIR = 1,
                     dPost = 0,
                     lagPDO = NA,
                     Observer_1 = 0,
                     Observer_2 = 0, 
                     Observer_3 = 0,
                     Observer_4 = 0,
                     Observer_5 = 0,
                     Observer_6 = 0,
                     Observer_7 = 0,
                     Observer_8 = 0,
                     Observer_9 = 0,
                     Observer_10 = 0)
new_ob1968 <- data.frame(YEAR = 1968, 
                     STREAMID = "113-41-019", 
                     standard_ct = NA, 
                     dIR = 1,
                     dPost = 0,
                     lagPDO = NA,
                     Observer_1 = 0,
                     Observer_2 = 0, 
                     Observer_3 = 0,
                     Observer_4 = 0,
                     Observer_5 = 0,
                     Observer_6 = 0,
                     Observer_7 = 0,
                     Observer_8 = 0,
                     Observer_9 = 0,
                     Observer_10 = 0)
new_ob1970 <- data.frame(YEAR = 1970, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 0,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_5 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
new_ob1974 <- data.frame(YEAR = 1974, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 0,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_5 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
new_ob1976 <- data.frame(YEAR = 1976, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 0,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_5 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
new_ob1992 <- data.frame(YEAR = 1992, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 0,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_5 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
DD_E1980 <- rbind(DD_E1980, new_ob1960, new_ob1968, new_ob1970, new_ob1974, new_ob1976, new_ob1992)

# filter IR
DD_IR <- DD_E1980 %>%
  filter(STREAMID == "113-41-019")

DD_IR <- DD_IR %>% 
  arrange(YEAR)

# interpolate
DD_IR$standard_ct <- na_interpolation(DD_IR$standard_ct, option = "linear")
DD_IR$lagPDO <- na_interpolation(DD_IR$lagPDO, option = "linear")

# bind new data 
DD_E1980 <- DD_E1980 %>%
  filter(STREAMID != "113-41-019")
DD_E1980 <- rbind(DD_E1980, DD_IR)

# no variation in some observers before 1980
# 1, 2, 4, 6, 8, 10
DD_E1980 <- DD_E1980[-c(7, 8, 10, 12, 14, 16)]

# generate index
DD_E1980$index <- ((DD_E1980$YEAR - 1960)/2)+1

# set up synthetic control
synth_out <- DD_E1980 %>% 
  # 1. Initiate: Define outcome, treatment unit/time
  synthetic_control(
    outcome = standard_ct,
    unit = STREAMID,
    time = index,
    i_unit = "113-41-019",
    i_time = 11,
    generate_placebos = TRUE
  ) %>% 
  # 2. Predictors: Generate covariates for pre-intervention
  generate_predictor(time_window = 1:11,
                     # lagPDO = mean(lagPDO),
                     Observer_3 = mean(Observer_3),
                     Observer_5 = mean(Observer_5),
                     Observer_7 = mean(Observer_7),
                     Observer_9 = mean(Observer_9)
  ) %>% 
  # 3. Weights: Optimize donor unit weights  
  generate_weights(optimization_window = 1:11) %>% 
  generate_control()

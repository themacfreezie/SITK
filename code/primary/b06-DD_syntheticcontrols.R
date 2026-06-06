library(dplyr)
library(fastDummies)
library(here) # set working directory
library(imputeTS)
library(tidysynth)

# set loc
here::i_am("code/primary/b06-DD_syntheticcontrols.R")
options(max.print=2000)

# load data 
load(here("data", "clean", "DD_E1975.Rda"))
load(here("data", "clean", "DD_O1975.Rda"))

# dummy variables for factor (observer)
DD_E1975test <- dummy_cols(
  DD_E1975
)
DD_E1975 <- DD_E1975test[-c(7:43, 54)]

DD_O1975test <- dummy_cols(
  DD_O1975
)
DD_O1975 <- DD_O1975test[-c(7:43, 53)]

# missing data for Indian River - even years
  # 1960, 1968, 1970, 1974, 1976, 1992 (oy)

# missing data for Indian River - odd years
  # 1961, 1975, 1989, 1991

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
new_ob1961 <- data.frame(YEAR = 1961, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 0,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
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
new_ob1975 <- data.frame(YEAR = 1975, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 0,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
new_ob1976 <- data.frame(YEAR = 1976, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 1,
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
new_ob1989 <- data.frame(YEAR = 1989, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 1,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
new_ob1991 <- data.frame(YEAR = 1991, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 1,
                         lagPDO = NA,
                         Observer_1 = 0,
                         Observer_2 = 0, 
                         Observer_3 = 0,
                         Observer_4 = 0,
                         Observer_6 = 0,
                         Observer_7 = 0,
                         Observer_8 = 0,
                         Observer_9 = 0,
                         Observer_10 = 0)
new_ob1992 <- data.frame(YEAR = 1992, 
                         STREAMID = "113-41-019", 
                         standard_ct = NA, 
                         dIR = 1,
                         dPost = 1,
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
DD_E1975 <- rbind(DD_E1975, new_ob1960, new_ob1968, new_ob1970, new_ob1974, new_ob1976, new_ob1992)
DD_O1975 <- rbind(DD_O1975, new_ob1961, new_ob1975, new_ob1989, new_ob1991)

# filter IR - even
DD_IR <- DD_E1975 %>%
  filter(STREAMID == "113-41-019")

DD_IR <- DD_IR %>% 
  arrange(YEAR)

# interpolate
DD_IR$standard_ct <- na_interpolation(DD_IR$standard_ct, option = "linear")
DD_IR$lagPDO <- na_interpolation(DD_IR$lagPDO, option = "linear")

# bind new data 
DD_E1975 <- DD_E1975 %>%
  filter(STREAMID != "113-41-019")
DD_E1975 <- rbind(DD_E1975, DD_IR)

# filter IR - odd
DD_IR <- DD_O1975 %>%
  filter(STREAMID == "113-41-019")

DD_IR <- DD_IR %>% 
  arrange(YEAR)

# interpolate
DD_IR$standard_ct <- na_interpolation(DD_IR$standard_ct, option = "linear")
DD_IR$lagPDO <- na_interpolation(DD_IR$lagPDO, option = "linear")

# bind new data 
DD_O1975 <- DD_O1975 %>%
  filter(STREAMID != "113-41-019")
DD_O1975 <- rbind(DD_O1975, DD_IR)

# no variation in some observers before treatment period
# even -  1, 2, 3, 4, 6, 8, 10 (1975)
# even -  1, 2, 4, 6, 8, 10 (1980)
DD_E1975 <- DD_E1975[-c(7:10, 12, 14, 16)] # 1975
# DD_E1980 <- DD_E1980[-c(7, 8, 10, 12, 14, 16)] # 1980
# odd - 1, 2, 3, 4, 6, 8, 10 (1975)
DD_O1975 <- DD_O1975[-c(7:11, 13, 15)] #1975

# drop years greater than 2022
DD_E1975 <- DD_E1975 %>%
  filter(YEAR != 2024)

# generate indices
DD_E1975$index <- ((DD_E1975$YEAR - 1960)/2)+1
DD_O1975$index <- ((DD_O1975$YEAR - 1961)/2)+1

# # even - 1980
# if(!file.exists(file=here("data", "clean", "synthcontrol_E1980.Rda"))){
# # set up synthetic control
# synth_out <- DD_E1980 %>% 
#   # 1. Initiate: Define outcome, treatment unit/time
#   synthetic_control(
#     outcome = standard_ct,
#     unit = STREAMID,
#     time = index,
#     i_unit = "113-41-019",
#     i_time = 11,
#     generate_placebos = TRUE
#   ) %>% 
#   # 2. Predictors: Generate covariates for pre-intervention
#   generate_predictor(time_window = 1:11,
#                      # lagPDO = mean(lagPDO),
#                      # Observer_3 = mean(Observer_3),
#                      Observer_5 = mean(Observer_5),
#                      Observer_7 = mean(Observer_7),
#                      Observer_9 = mean(Observer_9)
#   ) %>% 
#   # 3. Weights: Optimize donor unit weights  
#   generate_weights(optimization_window = 1:11) %>% 
#   generate_control()
# 
# # check it out
# plot_trends(synth_out)
# plot_differences(synth_out)
# plot_weights(synth_out)
# 
# # synth_out seems best
# synth_control <- synth_out %>%
#   grab_synthetic_control()
# 
# # generate year and dummies and rename count
# synth_control$YEAR <- (synth_control$time_unit - 1)*2 + 1960
# synthcontrol_E1980 <- synth_control[-c(1,2)]
# synthcontrol_E1980$dPost <- ifelse(synthcontrol_E1980$YEAR>1980, 1, 0)
# synthcontrol_E1980$dIR <- 0
# names(synthcontrol_E1980)[names(synthcontrol_E1980) == "synth_y"] <- "standard_ct"
# 
# save(synthcontrol_E1980, file=here("data", "clean", "synthcontrol_E1980.Rda"))
# }

# even - 1975
if(!file.exists(file=here("data", "clean", "synthcontrol_E1975.Rda"))){
  # set up synthetic control
  synth_out <- DD_E1975 %>% 
    # 1. Initiate: Define outcome, treatment unit/time
    synthetic_control(
      outcome = standard_ct,
      unit = STREAMID,
      time = index,
      i_unit = "113-41-019",
      i_time = 10,
      generate_placebos = TRUE
    ) %>% 
    # 2. Predictors: Generate covariates for pre-intervention
    generate_predictor(time_window = 1:10,
                       # lagPDO = mean(lagPDO),
                       # Observer_3 = mean(Observer_3),
                       Observer_5 = mean(Observer_5),
                       Observer_7 = mean(Observer_7),
                       Observer_9 = mean(Observer_9)
    ) %>% 
    # 3. Weights: Optimize donor unit weights  
    generate_weights(optimization_window = 1:10) %>% 
    generate_control()
  
  # check it out
  plot_trends(synth_out)
  plot_differences(synth_out)
  plot_weights(synth_out)
  
  # synth_out seems best
  synth_control <- synth_out %>%
    grab_synthetic_control()
  
  # generate year and dummies and rename count
  synth_control$YEAR <- (synth_control$time_unit - 1)*2 + 1960
  synthcontrol_E1975 <- synth_control[-c(1,2)]
  synthcontrol_E1975$dPost <- ifelse(synthcontrol_E1975$YEAR>1976, 1, 0)
  synthcontrol_E1975$dIR <- 0
  names(synthcontrol_E1975)[names(synthcontrol_E1975) == "synth_y"] <- "standard_ct"
  
  save(synthcontrol_E1975, file=here("data", "clean", "synthcontrol_E1975.Rda"))
}

# odd - 1975
if(!file.exists(file=here("data", "clean", "synthcontrol_O1975.Rda"))){
  # set up synthetic control
  synth_out <- DD_O1975 %>% 
    # 1. Initiate: Define outcome, treatment unit/time
    synthetic_control(
      outcome = standard_ct,
      unit = STREAMID,
      time = index,
      i_unit = "113-41-019",
      i_time = 9,
      generate_placebos = TRUE
    ) %>% 
    # 2. Predictors: Generate covariates for pre-intervention
    generate_predictor(time_window = 1:9,
                       # lagPDO = mean(lagPDO),
                       # Observer_3 = mean(Observer_3),
                       # Observer_5 = mean(Observer_5),
                       Observer_7 = mean(Observer_7),
                       Observer_9 = mean(Observer_9)
    ) %>% 
    # 3. Weights: Optimize donor unit weights  
    generate_weights(optimization_window = 1:9) %>% 
    generate_control()
  
  # check it out
  plot_trends(synth_out)
  plot_differences(synth_out)
  plot_weights(synth_out)
  
  # synth_out seems best
  synth_control <- synth_out %>%
    grab_synthetic_control()
  
  # generate year and dummies and rename count
  synth_control$YEAR <- (synth_control$time_unit - 1)*2 + 1961
  synthcontrol_O1975 <- synth_control[-c(1,2)]
  synthcontrol_O1975$dPost <- ifelse(synthcontrol_O1975$YEAR>1975, 1, 0)
  synthcontrol_O1975$dIR <- 0
  names(synthcontrol_O1975)[names(synthcontrol_O1975) == "synth_y"] <- "standard_ct"
  
  save(synthcontrol_O1975, file=here("data", "clean", "synthcontrol_O1975.Rda"))
}

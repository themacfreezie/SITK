# load libraries
library(dplyr)
library(rjags)
library(readxl)

# create working dir and output folder
WD1 <- "C:/Users/thema/OneDrive/Documents/dissertation/ch1/data"
setwd(WD1)

options(max.print=10000)

# pull in data
ir.df <- read_excel("IndianRiver.xlsx", sheet = "Stopha 2015 - Table 6", col_names = TRUE)
# hs.df <- read_excel("HSL Annual Weir Counts Pink - 1980 to present.xlsx", col_names = TRUE)
hs.df <- read_excel("adfg_pink.xlsx", sheet = "Sheet2", col_names = TRUE)

# we'll grab sitka sound index data
timeI <- ir.df$YEAR
yI <- ir.df$`Sitka Sound Index Escapement`

# split hugh smith odd/even
hsE.df <- hs.df %>% filter(YEAR %% 2 == 0)
hsO.df <- hs.df %>% filter(YEAR %% 2 != 0)

# data transform - drop obs before 1993
ir.df <- ir.df %>% filter(YEAR > 1992)

# data transform - split into odd/even runs
irE.df <- ir.df %>% filter(YEAR %% 2 == 0)
irO.df <- ir.df %>% filter(YEAR %% 2 != 0)

# set time and observed variable
timeA <- ir.df$YEAR
timeE <- irE.df$YEAR
timeO <- irO.df$YEAR
timeIE <- timeE

timehsE <- hsE.df$YEAR
timehsO <- hsO.df$YEAR

yA <- ir.df$`Indian River Peak Escapement`
yE <- irE.df$`Indian River Peak Escapement`
yIE <- irE.df$`Sitka Sound Index Escapement`
yO <- irO.df$`Indian River Peak Escapement`

yhsE <- hsE.df$PEAK_COUNT
yhsO <- hsO.df$PEAK_COUNT 

# plot time series
plot(timeE, yE, type = 'l')
plot(timeO, yO, type = 'l')
plot(timeA, yA, type = 'l')

plot(timehsE, yhsE, type = 'l')
plot(timehsO, yhsO, type = 'l')


## EVEN YEARS
# jags code - building components
random_walkE <- "
model{

  #### Observation Model
  for (t in 1:n){
    yE[t] ~ dnorm(xE[t], tauE_obs)
  }

  #### Process Model
  for (t in 2:n){
    xE[t] ~ dnorm(xE[t-1], tauE_proc)
  }

  #### Priors
  xE[1] ~ dnorm(xE_ic, tauE_ic)
  tauE_obs ~ dgamma(aE_obs, rE_obs)
  tauE_proc ~ dgamma(aE_proc, rE_proc)
}
"

dataE <- list(
  yE = yE,
  n = length(yE),
  xE_ic = 75000,
  tauE_ic = 1,
  aE_obs = 1,
  rE_obs = 1,
  aE_proc = 1,
  rE_proc = 1
)

# to test prediction ability
# data$y[(length(y)-51):length(y)] = NA

initE <- list(list(tauE_proc = 1/var(diff(yE)), tauE_obs = 1/var(yE)))

# jags code - building jags model
j_modelE <- jags.model(file = textConnection(random_walkE),
                      data = dataE,
                      inits = initE,
                      n.chains = 1)

# 'burn-in' step
jags_outE <- coda.samples(model = j_modelE,
                         variable.names = c("tauE_proc", "tauE_obs"),
                         n.iter = 10000)
plot(jags_outE)
  # process error seems to be distributed around zero..

# derive x values (latent variables)
jags_outE <- coda.samples(model = j_modelE,
                         variable.names = c("xE",
                                            "tauE_proc",
                                            "tauE_obs"),
                         n.iter = 10000)

# retrieve x values
outputE <- as.matrix(jags_outE)
xsE = outputE[,3:ncol(outputE)]

# point estimates = averages of these 10K samples, uncertainty = variance among them
predictionsE <- colMeans(xsE)
plot(timeE, predictionsE, type = 'l')
points(timeE, yE)

pred_intervalE <- apply(xsE, 2, quantile, c(0.025, 0.975))
lines(timeE, pred_intervalE[1,], lty = "dashed", col = "blue")
lines(timeE, pred_intervalE[2,], lty = "dashed", col = "blue")

# check estimated variance
hist(1/sqrt(outputE[,1])) #observation error variance
hist(1/sqrt(outputE[,2])) #process error variance
plot(1/sqrt(outputE[,1]), 1/sqrt(outputE[,2]))
  # this is all fucked up


## ODD YEARS
# jags code - building components
random_walkO <- "
model{

  #### Observation Model
  for (t in 1:n){
    yO[t] ~ dnorm(xO[t], tauO_obs)
  }

  #### Process Model
  for (t in 2:n){
    xO[t] ~ dnorm(xO[t-1], tauO_proc)
  }

  #### Priors
  xO[1] ~ dnorm(xO_ic, tauO_ic)
  tauO_obs ~ dgamma(aO_obs, rO_obs)
  tauO_proc ~ dgamma(aO_proc, rO_proc)
}
"

dataO <- list(
  yO = yO,
  n = length(yO),
  xO_ic = 75000,
  tauO_ic = 1,
  aO_obs = 1,
  rO_obs = 1,
  aO_proc = 1,
  rO_proc = 1
)

# to test prediction ability
# data$y[(length(y)-51):length(y)] = NA

initO <- list(list(tauO_proc = 1/var(diff(yO)), tauO_obs = 1/var(yO)))

# jags code - building jags model
j_modelO <- jags.model(file = textConnection(random_walkO),
                       data = dataO,
                       inits = initO,
                       n.chains = 1)

# 'burn-in' step
jags_outO <- coda.samples(model = j_modelO,
                          variable.names = c("tauO_proc", "tauO_obs"),
                          n.iter = 10000)
plot(jags_outO)
# process error seems to be distributed around zero..

# derive x values (latent variables)
jags_outO <- coda.samples(model = j_modelO,
                          variable.names = c("xO",
                                             "tauO_proc",
                                             "tauO_obs"),
                          n.iter = 10000)

# retrieve x values
outputO <- as.matrix(jags_outO)
xsO = outputO[,3:ncol(outputO)]

# point estimates = averages of these 10K samples, uncertainty = variance among them
predictionsO <- colMeans(xsO)
plot(timeO, predictionsO, type = 'l')
points(timeO, yO)

pred_intervalO <- apply(xsO, 2, quantile, c(0.025, 0.975))
lines(timeO, pred_intervalO[1,], lty = "dashed", col = "blue")
lines(timeO, pred_intervalO[2,], lty = "dashed", col = "blue")

# check estimated variance
hist(1/sqrt(outputO[,1])) #observation error variance
hist(1/sqrt(outputO[,2])) #process error variance
plot(1/sqrt(outputO[,1]), 1/sqrt(outputO[,2]))
  # this is also all fucked up
  # maybe too few observations?
# 
# 
# ## ALL YEARS
# # jags code - building components
# random_walkA <- "
# model{
#   
#   #### Observation Model
#   for (t in 1:n){
#     yA[t] ~ dnorm(xA[t], tauA_obs)
#   }
#   
#   #### Process Model
#   for (t in 2:n){
#     xA[t] ~ dnorm(xA[t-1], tauA_proc)
#   }
#   
#   #### Priors
#   xA[1] ~ dnorm(xA_ic, tauA_ic)
#   tauA_obs ~ dgamma(aA_obs, rA_obs)
#   tauA_proc ~ dgamma(aA_proc, rA_proc)
# }
# "
# 
# dataA <- list(
#   yA = yA,
#   n = length(yA),
#   xA_ic = 75000,
#   tauA_ic = 1,
#   aA_obs = 1,
#   rA_obs = 1,
#   aA_proc = 1,
#   rA_proc = 1
# )
# 
# # to test prediction ability
# # data$y[(length(y)-51):length(y)] = NA
# 
# initA <- list(list(tauA_proc = 1/var(diff(yA)), tauA_obs = 1/var(yA)))
# 
# # jags code - building jags model
# j_modelA <- jags.model(file = textConnection(random_walkA),
#                        data = dataA,
#                        inits = initA,
#                        n.chains = 1)
# 
# # 'burn-in' step
# jags_outA <- coda.samples(model = j_modelA,
#                           variable.names = c("tauA_proc", "tauA_obs"),
#                           n.iter = 10000)
# plot(jags_outA)
# # process error seems to be distributed around zero.. 
# 
# # derive x values (latent variables)
# jags_outA <- coda.samples(model = j_modelA,
#                           variable.names = c("xA", 
#                                              "tauA_proc", 
#                                              "tauA_obs"),
#                           n.iter = 10000)
# 
# # retrieve x values
# outputA <- as.matrix(jags_outA)
# xsA = outputA[,3:ncol(outputA)]
# 
# # point estimates = averages of these 10K samples, uncertainty = variance among them
# predictionsA <- colMeans(xsA)
# plot(timeA, predictionsA, type = 'l')
# points(timeA, yA)
# 
# pred_intervalA <- apply(xsA, 2, quantile, c(0.025, 0.975))
# lines(timeA, pred_intervalA[1,], lty = "dashed", col = "blue")
# lines(timeA, pred_intervalA[2,], lty = "dashed", col = "blue")
# 
# # check estimated variance
# hist(1/sqrt(outputA[,1])) #observation error variance
# hist(1/sqrt(outputA[,2])) #process error variance
# plot(1/sqrt(outputA[,1]), 1/sqrt(outputA[,2]))
#   # this is still fucked up


# ## SITKA SOUND INDEX - for comparison
# # jags code - building components
# random_walkI <- "
# model{
#   
#   #### Observation Model
#   for (t in 1:n){
#     yI[t] ~ dnorm(xI[t], tauI_obs)
#   }
#   
#   #### Process Model
#   for (t in 2:n){
#     xI[t] ~ dnorm(xI[t-1], tauI_proc)
#   }
#   
#   #### Priors
#   xI[1] ~ dnorm(xI_ic, tauI_ic)
#   tauI_obs ~ dgamma(aI_obs, rI_obs)
#   tauI_proc ~ dgamma(aI_proc, rI_proc)
# }
# "
# 
# dataI <- list(
#   yI = yI,
#   n = length(yI),
#   xI_ic = 75000,
#   tauI_ic = 1,
#   aI_obs = 1,
#   rI_obs = 1,
#   aI_proc = 1,
#   rI_proc = 1
# )
# 
# # to test prediction ability
# # data$y[(length(y)-51):length(y)] = NA
# 
# initI <- list(list(tauI_proc = 1/var(diff(yI)), tauI_obs = 1/var(yI)))
# 
# # jags code - building jags model
# j_modelI <- jags.model(file = textConnection(random_walkI),
#                        data = dataI,
#                        inits = initI,
#                        n.chains = 1)
# 
# # 'burn-in' step
# jags_outI <- coda.samples(model = j_modelI,
#                           variable.names = c("tauI_proc", "tauI_obs"),
#                           n.iter = 10000)
# plot(jags_outI)
# # process error seems to be distributed around zero.. 
# 
# # derive x values (latent variables)
# jags_outI <- coda.samples(model = j_modelI,
#                           variable.names = c("xI", 
#                                              "tauI_proc", 
#                                              "tauI_obs"),
#                           n.iter = 10000)
# 
# # retrieve x values
# outputI <- as.matrix(jags_outI)
# xsI = outputI[,3:ncol(outputI)]
# 
# # point estimates = averages of these 10K samples, uncertainty = variance among them
# predictionsI <- colMeans(xsI)
# plot(timeI, predictionsI, type = 'l')
# points(timeI, yI)
# 
# pred_intervalI <- apply(xsI, 2, quantile, c(0.025, 0.975))
# lines(timeI, pred_intervalI[1,], lty = "dashed", col = "blue")
# lines(timeI, pred_intervalI[2,], lty = "dashed", col = "blue")
# 
# # check estimated variance
# hist(1/sqrt(outputI[,1])) #observation error variance
# hist(1/sqrt(outputI[,2])) #process error variance
# plot(1/sqrt(outputI[,1]), 1/sqrt(outputI[,2]))
#   # way less fucked
# 
# ## SITKA SOUND INDEX (only even years after 1993) - for comparison
# # jags code - building components
# random_walkIE <- "
# model{
#   
#   #### Observation Model
#   for (t in 1:n){
#     yIE[t] ~ dnorm(xIE[t], tauIE_obs)
#   }
#   
#   #### Process Model
#   for (t in 2:n){
#     xIE[t] ~ dnorm(xIE[t-1], tauIE_proc)
#   }
#   
#   #### Priors
#   xIE[1] ~ dnorm(xIE_ic, tauIE_ic)
#   tauIE_obs ~ dgamma(aIE_obs, rIE_obs)
#   tauIE_proc ~ dgamma(aIE_proc, rIE_proc)
# }
# "
# 
# dataIE <- list(
#   yIE = yIE,
#   n = length(yIE),
#   xIE_ic = 75000,
#   tauIE_ic = 1,
#   aIE_obs = 1,
#   rIE_obs = 1,
#   aIE_proc = 1,
#   rIE_proc = 1
# )
# 
# # to test prediction ability
# # data$y[(length(y)-51):length(y)] = NA
# 
# initIE <- list(list(tauIE_proc = 1/var(diff(yIE)), tauIE_obs = 1/var(yIE)))
# 
# # jags code - building jags model
# j_modelIE <- jags.model(file = textConnection(random_walkIE),
#                        data = dataIE,
#                        inits = initIE,
#                        n.chains = 1)
# 
# # 'burn-in' step
# jags_outIE <- coda.samples(model = j_modelIE,
#                           variable.names = c("tauIE_proc", "tauIE_obs"),
#                           n.iter = 10000)
# plot(jags_outIE)
# # process error seems to be distributed around zero.. 
# 
# # derive x values (latent variables)
# jags_outIE <- coda.samples(model = j_modelIE,
#                           variable.names = c("xIE", 
#                                              "tauIE_proc", 
#                                              "tauIE_obs"),
#                           n.iter = 10000)
# 
# # retrieve x values
# outputIE <- as.matrix(jags_outIE)
# xsIE = outputIE[,3:ncol(outputIE)]
# 
# # point estimates = averages of these 10K samples, uncertainty = variance among them
# predictionsIE <- colMeans(xsIE)
# plot(timeIE, predictionsIE, type = 'l')
# points(timeIE, yIE)
# 
# pred_intervalIE <- apply(xsIE, 2, quantile, c(0.025, 0.975))
# lines(timeIE, pred_intervalIE[1,], lty = "dashed", col = "blue")
# lines(timeIE, pred_intervalIE[2,], lty = "dashed", col = "blue")
# 
# # check estimated variance
# hist(1/sqrt(outputIE[,1])) #observation error variance
# hist(1/sqrt(outputIE[,2])) #process error variance
# plot(1/sqrt(outputIE[,1]), 1/sqrt(outputIE[,2]))


## Hugh Smith Weir - for comparison, even
# jags code - building components
random_walkhsE <- "
model{
  
  #### Observation Model
  for (t in 1:n){
    yhsE[t] ~ dnorm(xhsE[t], tauhsE_obs)
  }
  
  #### Process Model
  for (t in 2:n){
    xhsE[t] ~ dnorm(xhsE[t-1], tauhsE_proc)
  }
  
  #### Priors
  xhsE[1] ~ dnorm(xhsE_ic, tauhsE_ic)
  tauhsE_obs ~ dgamma(ahsE_obs, rhsE_obs)
  tauhsE_proc ~ dgamma(ahsE_proc, rhsE_proc)
}
"

datahsE <- list(
  yhsE = yhsE,
  n = length(yhsE),
  xhsE_ic = 10000,
  tauhsE_ic = 1,
  ahsE_obs = 1,
  rhsE_obs = 1,
  ahsE_proc = 1,
  rhsE_proc = 1
)

# to test prediction ability
# data$y[(length(y)-51):length(y)] = NA

inithsE <- list(list(tauhsE_proc = 1/var(diff(yhsE)), tauhsE_obs = 1/var(yhsE)))

# jags code - building jags model
j_modelhsE <- jags.model(file = textConnection(random_walkhsE),
                       data = datahsE,
                       inits = inithsE,
                       n.chains = 1)

# 'burn-in' step
jags_outhsE <- coda.samples(model = j_modelhsE,
                          variable.names = c("tauhsE_proc", "tauhsE_obs"),
                          n.iter = 10000)
plot(jags_outhsE)
# process error seems to be distributed around zero.. 

# derive x values (latent variables)
jags_outhsE <- coda.samples(model = j_modelhsE,
                          variable.names = c("xhsE", 
                                             "tauhsE_proc", 
                                             "tauhsE_obs"),
                          n.iter = 10000)

# retrieve x values
outputhsE <- as.matrix(jags_outhsE)
xshsE = outputhsE[,3:ncol(outputhsE)]

# point estimates = averages of these 10K samples, uncertainty = variance among them
predictionshsE <- colMeans(xshsE)
plot(timehsE, predictionshsE, type = 'l')
points(timehsE, yhsE)

pred_intervalhsE <- apply(xshsE, 2, quantile, c(0.025, 0.975))
lines(timehsE, pred_intervalhsE[1,], lty = "dashed", col = "blue")
lines(timehsE, pred_intervalhsE[2,], lty = "dashed", col = "blue")

# check estimated variance
hist(1/sqrt(outputhsE[,1])) #observation error variance
hist(1/sqrt(outputhsE[,2])) #process error variance
plot(1/sqrt(outputhsE[,1]), 1/sqrt(outputhsE[,2]))


## Hugh Smith Weir - for comparison, odd
# jags code - building components
random_walkhsO <- "
model{
  
  #### Observation Model
  for (t in 1:n){
    yhsO[t] ~ dnorm(xhsO[t], tauhsO_obs)
  }
  
  #### Process Model
  for (t in 2:n){
    xhsO[t] ~ dnorm(xhsO[t-1], tauhsO_proc)
  }
  
  #### Priors
  xhsO[1] ~ dnorm(xhsO_ic, tauhsO_ic)
  tauhsO_obs ~ dgamma(ahsO_obs, rhsO_obs)
  tauhsO_proc ~ dgamma(ahsO_proc, rhsO_proc)
}
"

datahsO <- list(
  yhsO = yhsO,
  n = length(yhsO),
  xhsO_ic = 10000,
  tauhsO_ic = 1,
  ahsO_obs = 1,
  rhsO_obs = 1,
  ahsO_proc = 1,
  rhsO_proc = 1
)

# to test prediction ability
# data$y[(length(y)-51):length(y)] = NA

inithsO <- list(list(tauhsO_proc = 1/var(diff(yhsO)), tauhsO_obs = 1/var(yhsO)))

# jags code - building jags model
j_modelhsO <- jags.model(file = textConnection(random_walkhsO),
                         data = datahsO,
                         inits = inithsO,
                         n.chains = 1)

# 'burn-in' step
jags_outhsO <- coda.samples(model = j_modelhsO,
                            variable.names = c("tauhsO_proc", "tauhsO_obs"),
                            n.iter = 10000)
plot(jags_outhsO)
# process error seems to be distributed around zero.. 

# derive x values (latent variables)
jags_outhsO <- coda.samples(model = j_modelhsO,
                            variable.names = c("xhsO", 
                                               "tauhsO_proc", 
                                               "tauhsO_obs"),
                            n.iter = 10000)

# retrieve x values
outputhsO <- as.matrix(jags_outhsO)
xshsO = outputhsO[,3:ncol(outputhsO)]

# point estimates = averages of these 10K samples, uncertainty = variance among them
predictionshsO <- colMeans(xshsO)
plot(timehsO, predictionshsO, type = 'l')
points(timehsO, yhsO)

pred_intervalhsO <- apply(xshsO, 2, quantile, c(0.025, 0.975))
lines(timehsO, pred_intervalhsO[1,], lty = "dashed", col = "blue")
lines(timehsO, pred_intervalhsO[2,], lty = "dashed", col = "blue")

# check estimated variance
hist(1/sqrt(outputhsO[,1])) #observation error variance
hist(1/sqrt(outputhsO[,2])) #process error variance
plot(1/sqrt(outputhsO[,1]), 1/sqrt(outputhsO[,2]))
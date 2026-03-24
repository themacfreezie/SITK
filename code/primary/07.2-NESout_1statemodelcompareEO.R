library(here)
library(MARSS)

# set loc
here::i_am("code/primary/07-NSEout_modeleval.R")
options(max.print=2000)

# build 1-state model

# load and clean data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinksE_scst.Rda"))
load(here("data", "clean", "NSEout_wpinksO_scst.Rda"))
# observer data - from NSEout_observer
load(here("data", "clean", "NSEout_wobserverE.Rda"))
load(here("data", "clean", "NSEout_wobserverO.Rda"))
# pdo data - from pdo_clean
load(here("data", "clean", "WpdoE.Rda"))
load(here("data", "clean", "WpdoO.Rda"))

# grab year lists, # of observations, & pure count data
wpinksE_scst.df <- wpinksE_scst.df[-c(34)]
yearsE <- names(wpinksE_scst.df)
yearsO <- names(wpinksO_scst.df)
yearsE <- yearsE[-1]
yearsE <- substring(yearsE, first=13, last=16)
yearsO <- yearsO[-1]
yearsO <- substring(yearsO, first=13, last=16)
n <- nrow(wpinksE_scst.df)

# drop pdo for years w/out complete data
WpdoE <- WpdoE[-c(33)]
WpdoO <- WpdoO[-c(33)]

# drop observer streams missing from peak count data
## why are these missing?
wobserverE.df <- wobserverE.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]
wobserverE.df <- wobserverE.df[-c(34)] #2024
wobserverO.df <- wobserverO.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

# replace missing values 
wobserverE.df[is.na(wobserverE.df)] <- 11
wobserverO.df[is.na(wobserverO.df)] <- 11

# convert counts to matrix
datE <- data.matrix(wpinksE_scst.df[2:ncol(wpinksE_scst.df)])
datO <- data.matrix(wpinksO_scst.df[2:ncol(wpinksO_scst.df)])

# convert observer ID and pdo data to matrix
pdoE <- data.matrix(WpdoE)
pdoO <- data.matrix(WpdoO)

obsE <- data.matrix(wobserverE.df[2:ncol(wobserverE.df)])
obsO <- data.matrix(wobserverO.df[2:ncol(wobserverO.df)])

levels <- sort(unique(as.vector(obsE)))
result <- do.call(rbind, lapply(1:nrow(obsE), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obsE[i,] == x)))))
obE <- result

levels <- sort(unique(as.vector(obsO)))
result <- do.call(rbind, lapply(1:nrow(obsO), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obsO[i,] == x)))))
obO <- result
  # no observer 5 in odd years

# setting up MARSS model inputs
# easy ones
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"

# U (underlying trend in process) 
#   **mark always says 'trend' isn't a good way to think about this
u.model <- "zero"

# Z models 
# 2-states (IR and region)
region_row <- c(1, 0)
indriv_row <- c(0, 1)
z.model_2state <- matrix(rep(region_row, 36), ncol = 2, byrow = TRUE)
z.model_2state[6, ] <- indriv_row

# 1-state
z.model_1state <- matrix(1, 36, 1)

# D (control for observer)
# even
dE.model <- matrix(list(0), n, 11*n)
num_rowsD <- nrow(dE.model)

obs_vector <- c("obs01",
                "obs02",
                "obs03",
                "obs04",
                "obs05",
                "obs06",
                "obs07",
                "obs08",
                "obs09",
                "obs10",
                "obs11")

for (i in 1:num_rowsD) {
  cc <- 11*i - 10
  dE.model[i, cc:(cc+10)] <- obs_vector
}

# odd - no observer 5
dO.model <- matrix(list(0), n, 10*n)
num_rowsD <- nrow(dO.model)

obs_vector <- c("obs01",
                "obs02",
                "obs03",
                "obs04",
                # "obs05",
                "obs06",
                "obs07",
                "obs08",
                "obs09",
                "obs10",
                "obs11")

for (i in 1:num_rowsD) {
  cc <- 10*i - 9
  dO.model[i, cc:(cc+9)] <- obs_vector
}

# C (effect of pdo)
c.model_1state <- "diagonal and equal"
c.model_2state <- matrix("pdo", 2, 1)
  # this should work for run level effect of pdo

# Q (process error)
q.model <- "diagonal and equal"
  # once again, capturing run level process varaince

# R (site level observation error)
r.model <- "diagonal and unequal"
  # captures site-specific impacts on observations
  # unfortunately does not glean information from off-year observations
  # therefore probably weaker than the all years model

# model lists 
# 1state, even
model.list_1stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoE, D = dE.model, d = obE)

# 2state, even
model.list_2stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoE, D = dE.model, d = obE)

# 1state, odd
model.list_1stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoO, D = dO.model, d = obO)

# 2state, odd
model.list_2stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoO, D = dO.model, d = obO)

# run models
# 1state, even
ssNSE_1stE <- MARSS(datE, 
                    model = model.list_1stE, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_1stE, file=here("data", "clean", "ssNSE_1stE.rds"))

# 2state, even
ssNSE_2stE <- MARSS(datE, 
                    model = model.list_2stE, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_2stE, file=here("data", "clean", "ssNSE_2stE.rds"))

# 1state, odd
ssNSE_1stO <- MARSS(datO, 
                    model = model.list_1stO, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_1stO, file=here("data", "clean", "ssNSE_1stO.rds"))

# 2state, odd
ssNSE_2stO <- MARSS(datO, 
                    model = model.list_2stO, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_2stO, file=here("data", "clean", "ssNSE_2stO.rds"))

# AICc comparison
ssNSE_1stE$AICc
  # 1678.991
ssNSE_2stE$AICc
  # 1669.17 - significantly better fit than 1-state
    # according to mark's paper re: info theory, ~148.4 evidence ratio
ssNSE_1stO$AICc
  # 2150.515
ssNSE_2stO$AICc
  # 2160.245 - significantly worse fit than 1-state
    # similar difference between models as even-runs, except running the other way
    # according to mark's paper re: info theory, ~148.4 evidence ratio

#####
# I don't necessarily trust these results because of the degenerate observation 
# variance associated with Indian River in the 2-state models
# I ran into this issue before with the observation variance, which prompted
# me to set up the double-run model
# can I pull the variance terms for IR from that model and "plug them in" to
# this one? do the other variance terms generally match up?
# or force the whole observation matrix from the two-run model into these models?
# it should have much more complete information.. is this appropriate?

ssNSE <- readRDS(file=here("data", "clean", "ssNSE.rds"))
ssNSE

# grab R matrix from 2-run model 
R_ssNSE <- coef(ssNSE, type = "vector")[grepl("^R", names(coef(ssNSE, type = "vector")))]
r.model_2run <- diag(R_ssNSE)

# model lists including forced 2run R matrix
# 1state, even
model.list_1stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model_2run,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoE, D = dE.model, d = obE)

# 2state, even
model.list_2stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model_2run,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoE, D = dE.model, d = obE)

# 1state, odd
model.list_1stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model_2run,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoO, D = dO.model, d = obO)

# 2state, odd
model.list_2stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model_2run,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoO, D = dO.model, d = obO)

# run models
# 1state, even
ssNSE_1stEr <- MARSS(datE, 
                    model = model.list_1stE, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_1stEr, file=here("data", "clean", "ssNSE_1stEr.rds"))

# 2state, even
ssNSE_2stEr <- MARSS(datE, 
                    model = model.list_2stE, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_2stEr, file=here("data", "clean", "ssNSE_2stEr.rds"))

# 1state, odd
ssNSE_1stOr <- MARSS(datO, 
                    model = model.list_1stO, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_1stOr, file=here("data", "clean", "ssNSE_1stOr.rds"))

# 2state, odd
ssNSE_2stOr <- MARSS(datO, 
                    model = model.list_2stO, 
                    method = "kem",
                    control = list(maxit = 1000))
saveRDS(ssNSE_2stOr, file=here("data", "clean", "ssNSE_2stOr.rds"))

# AICc comparison
ssNSE_1stEr$AICc
  # 1855.757
ssNSE_2stEr$AICc
  # 1652.238 - significantly better fit than 1-state
    # according to mark's paper re: info theory, evidence ratio >3 million 
ssNSE_1stOr$AICc
  # 2283.299
ssNSE_2stOr$AICc
  # 2154.305 - significantly better fit than 1-state
    # according to mark's paper re: info theory, evidence ratio >3 million 

#### I DON'T KNOW IF THIS APPROACH IS VALID AT ALL ####
  # I kind of don't think so?

# let's compare the R matrix from each model and ssNSE..
R_1stE <- coef(ssNSE_1stE, type = "vector")[grepl("^R", names(coef(ssNSE_1stE, type = "vector")))]
R_2stE <- coef(ssNSE_2stE, type = "vector")[grepl("^R", names(coef(ssNSE_2stE, type = "vector")))]
R_1stO <- coef(ssNSE_1stO, type = "vector")[grepl("^R", names(coef(ssNSE_1stO, type = "vector")))]
R_2stO <- coef(ssNSE_2stO, type = "vector")[grepl("^R", names(coef(ssNSE_2stO, type = "vector")))]

R_estimates <- data.frame(R_ssNSE, R_1stE, R_2stE, R_1stO, R_2stO)
  # some of these are fairly dissimilar...

## what about just plugging in a value for Indian River and not the whole matrix
# set up r matrix to be fit except for Indian River
r.model_2runIR <- matrix(list(0), 36, 36)
for (i in 1:36) {
  r.model_2runIR[i,i] <- paste0("r",i)
}
r.model_2runIR[6, 6] <- R_ssNSE[6]

# model lists including forced 2run R matrix
# 1state, even
model.list_1stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model_2runIR,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoE, D = dE.model, d = obE)

# 2state, even
model.list_2stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model_2runIR,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoE, D = dE.model, d = obE)

# 1state, odd
model.list_1stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model_2runIR,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoO, D = dO.model, d = obO)

# 2state, odd
model.list_2stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model_2runIR,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoO, D = dO.model, d = obO)

# run models
# 1state, even
ssNSE_1stErIR <- MARSS(datE, 
                     model = model.list_1stE, 
                     method = "kem",
                     control = list(maxit = 1000))
saveRDS(ssNSE_1stErIR, file=here("data", "clean", "ssNSE_1stErIR.rds"))

# 2state, even
ssNSE_2stErIR <- MARSS(datE, 
                     model = model.list_2stE, 
                     method = "kem",
                     control = list(maxit = 1000))
saveRDS(ssNSE_2stErIR, file=here("data", "clean", "ssNSE_2stErIR.rds"))

# 1state, odd
ssNSE_1stOrIR <- MARSS(datO, 
                     model = model.list_1stO, 
                     method = "kem",
                     control = list(maxit = 1000))
saveRDS(ssNSE_1stOrIR, file=here("data", "clean", "ssNSE_1stOrIR.rds"))

# 2state, odd
ssNSE_2stOrIR <- MARSS(datO, 
                     model = model.list_2stO, 
                     method = "kem",
                     control = list(maxit = 1000))
saveRDS(ssNSE_2stOrIR, file=here("data", "clean", "ssNSE_2stOrIR.rds"))

# AICc comparison
ssNSE_1stErIR$AICc
  # 1890.877
ssNSE_2stErIR$AICc
  # 1667.203 - significantly better fit than 1-state
    # according to mark's paper re: info theory, evidence ratio >3 million 
ssNSE_1stOrIR$AICc
  # 2245.14
ssNSE_2stOrIR$AICc
  # 2158.24 - significantly better fit than 1-state
    # according to mark's paper re: info theory, evidence ratio >3 million
    # these are really similar to the AICc behavior with the full R-matrix
    # from the 2-run model
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

z.model_2state <- matrix(
  c(1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    0, 1,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0,
    1, 0),
  nrow = 36,
  ncol = 2,
  byrow = TRUE
)

z.model_1state <- matrix(
  c(1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1),
  nrow = 36,
  ncol = 1,
  byrow = TRUE
)

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
# 2state, even
model.list_2stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoE, D = dE.model, d = obE)

# 1state, even
model.list_1stE <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoE, D = dE.model, d = obE)

# 2state, odd
model.list_2stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_2state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdoO, D = dO.model, d = obO)

# 1state, odd
model.list_1stO <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model_1state, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_1state, c = pdoO, D = dO.model, d = obO)

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

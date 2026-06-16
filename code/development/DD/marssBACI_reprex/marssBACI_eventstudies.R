# packages
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/DD/marssBACI_reprex/marssBACI_eventstudies.R")
options(max.print=2000)

# load data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinks_scst.Rda"))
# observer data - from NSEout_observer
load(here("data", "clean", "NSEout_wobserver.Rda"))
# pdo data - from pdo_clean
load(here("data", "clean", "Wpdo.Rda"))

# packages
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/DD/marssBACI_reprex/marssBACI_1975.R")
options(max.print=2000)

# load data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinks_scst.Rda"))
# observer data - from NSEout_observer
load(here("data", "clean", "NSEout_wobserver.Rda"))
# pdo data - from pdo_clean
load(here("data", "clean", "Wpdo.Rda"))

# grab year lists, # of observations, & pure count data
wpinks_scst.df <- wpinks_scst.df[-c(66)]
years <- names(wpinks_scst.df)
years <- years[-1]
years <- substring(years, first=13, last=16)
n <- nrow(wpinks_scst.df)

# drop pdo for years w/out complete data
Wpdo <- Wpdo[-c(65, 66)]

# drop observer streams missing from peak count data
## why are these missing?
wobserver.df <- wobserver.df[c(2:6, 9:12, 14:21, 23:28, 30:42), ]

# replace missing values 
wobserver.df[is.na(wobserver.df)] <- 11

# gonna have to stack up salmon counts
streamID <- wpinks_scst.df[, 1]
oddCol_wpinks <- wpinks_scst.df[, seq(ncol(wpinks_scst.df)) %% 2 == 1]
oddCol_wpinks <- oddCol_wpinks[, -c(1)]
eveCol_wpinks <- wpinks_scst.df[, seq(ncol(wpinks_scst.df)) %% 2 != 1]

newnames <- c(paste0("t", seq(32)))

colnames(oddCol_wpinks) <- newnames
oddCol_wpinks <- cbind(STREAMID = streamID, oddCol_wpinks)
colnames(eveCol_wpinks) <- newnames
eveCol_wpinks <- cbind(STREAMID = streamID, eveCol_wpinks)

STACKwpinks_scst.df <- rbind(eveCol_wpinks, oddCol_wpinks)
# even on top

# observers
streamID <- wobserver.df[, 1]
wobserver.df <- wobserver.df[-c(66)]
oddCol_wobs <- wobserver.df[, seq(ncol(wobserver.df)) %% 2 == 1]
oddCol_wobs <- oddCol_wobs[, -c(1)]
eveCol_wobs <- wobserver.df[, seq(ncol(wobserver.df)) %% 2 != 1]

colnames(oddCol_wobs) <- newnames
oddCol_wobs <- cbind(STREAMID = streamID, oddCol_wobs)
colnames(eveCol_wobs) <- newnames
eveCol_wobs <- cbind(STREAMID = streamID, eveCol_wobs)

STACKwobs.df <- rbind(eveCol_wobs, oddCol_wobs)
# even on top

# pdo
eveCol_wpdo <- Wpdo[, seq(ncol(Wpdo)) %% 2 == 1]
oddCol_wpdo <- Wpdo[, seq(ncol(Wpdo)) %% 2 != 1]

colnames(oddCol_wpdo) <- newnames
colnames(eveCol_wpdo) <- newnames

STACKwpdo.df <- rbind(eveCol_wpdo, oddCol_wpdo)
# even on top

# convert counts, observer ID, and pdo data to matrix
dat <- data.matrix(STACKwpinks_scst.df[2:ncol(STACKwpinks_scst.df)])
dat1975 <- dat[, -c(10:32)]
dat2010 <- dat[, -c(27:32)]
obs <- data.matrix(STACKwobs.df[2:ncol(STACKwobs.df)])
obs1975 <- obs[, -c(10:32)]
obs2010 <- obs[, -c(27:32)]
pdo <- data.matrix(STACKwpdo.df)
pdo1975 <- pdo[, -c(10:32)]
pdo2010 <- pdo[, -c(27:32)]

# levels for observation data
levels <- sort(unique(as.vector(obs1975)))
result <- do.call(rbind, lapply(1:nrow(obs1975), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obs1975[i,] == x)))))
obD1975 <- result

levels <- sort(unique(as.vector(obs2010)))
result <- do.call(rbind, lapply(1:nrow(obs2010), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obs2010[i,] == x)))))
obD2010 <- result

# setting up MARSS model inputs
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"
u.model <- "zero"

# Z (underlying states)
z.model1 <- matrix(0, nrow = 72, ncol = 4) # separate IR odd & even years
z.model1[1:36, 1]  <- 1
z.model1[6, 1:2]   <- c(0, 1)
z.model1[37:72, 3] <- 1
z.model1[42, 3:4]  <- c(0, 1)

z.model2 <- matrix(0, nrow = 72, ncol = 2) # no separate IR
z.model2[1:36, 1]  <- 1
z.model2[37:72, 2] <- 1

z.model3 <- matrix(0, nrow = 72, ncol = 3) # separate IR even years
z.model3[1:36, 1]  <- 1
z.model3[6, 1:2]   <- c(0, 1)
z.model3[37:72, 3] <- 1

z.model4 <- matrix(0, nrow = 72, ncol = 3) # separate IR odd years
z.model4[1:36, 1]  <- 1
z.model4[37:72, 2] <- 1
z.model4[42, 2:3]  <- c(0, 1)

# D (control for observer)
d.model <- matrix(list(0), 2*n, (2*11)*n)
num_rowsD <- nrow(d.model)

obs_vector <- c(paste0("obs", seq(11)))

for (i in 1:num_rowsD) {
  cc <- 11*i - 10
  d.model[i, cc:(cc+10)] <- obs_vector
}

# Q (process error)
q.model <- matrix(list(0), 4, 4)
q.model[1,1] <- "qE"
q.model[2,2] <- "qE"
q.model[3,3] <- "qO"
q.model[4,4] <- "qO"

# C (effect of covariates)
c.model <- matrix(list(0), 4, 2)
c.model[1,1] <- "pE"
c.model[2,1] <- "pE"
c.model[3,2] <- "pO"
c.model[4,2] <- "pO"

## three looks at observation error
# R (diag and eq)
rDE.model <- "diagonal and equal"
# R (equal var cov)
rVC.model <- "equalvarcov"
# R (site level observation error)
rSite.model <- matrix(list(0), 72, 72)
for (i in 1:36) {
  rSite.model[i,i] <- paste0("r",i)
}
for (i in 1:36) {
  rSite.model[i+36,i+36] <- paste0("r",i)
}
rSite.model[6,6] <- "rIndianRiver"
rSite.model[42,42] <- "rIndianRiver"

# put Z specs into a list for easy looping
Z_models <- list(z.model1, z.model2, z.model3, z.model4)

# put R specs into a list for easy looping
R_models <- list(rDE.model, rVC.model, rSite.model)

# empty list to store the results
marssEventstudies_1975 <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)

# loopy models!
# 3 types of R
for (R_spec in 1:length(R_models)) {
  
  # 4 Z specs
  for (Z_spec in names(Z_models)) {
    
    z.model <- Z_models[[Z_spec]]
    r.model <- R_models[[R_spec]]

    # mod list
    model.list <- list(
      B = b.model, U = u.model, Q = q.model,
      Z = z.model, A = a.model, R = r.model,
      x0 = x.model, V0 = v.model, tinitx = 0,
      C = c.model,
      c = pdo1975
      #, D = d.model, d = obD1975
    )
    
    # unique id, e.g., "C1_treat_E_1_O_1"
    model_id <- paste0(Z_spec, "_", R_spec)
    
    # run MARSS 
    fit <- MARSS(
      dat1975, 
      model = model.list, 
      method = "kem",
      control = list(maxit = 1000)
    )
    
    # extract AICc
    current_aicc <- fit$AICc
    
    # append results
    marssEventstudies_1975 <- rbind(marssEventstudies_1975, data.frame(
      Model_Name = model_id,
      AICc = current_aicc
    ))
  }
}

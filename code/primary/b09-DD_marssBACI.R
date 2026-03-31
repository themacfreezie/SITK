# packages
library(grid)
library(here)
library(MARSS)
library(patchwork)
library(tidyverse)

# set loc
here::i_am("code/primary/b09-DD_marssBACI.R")
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

# gonna have to stack up them dataframes
# salmon counts
streamID <- wpinks_scst.df[, 1]
oddCol_wpinks <- wpinks_scst.df[, seq(ncol(wpinks_scst.df)) %% 2 == 1]
oddCol_wpinks <- oddCol_wpinks[, -c(1)]
eveCol_wpinks <- wpinks_scst.df[, seq(ncol(wpinks_scst.df)) %% 2 != 1]

newnames <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8",
              "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", 
              "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24", 
              "t25", "t26", "t27", "t28", "t29", "t30", "t31", "t32")

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

# convert counts, observer ID, and pdo data to matrix
dat <- data.matrix(STACKwpinks_scst.df[2:ncol(STACKwpinks_scst.df)])
obs <- data.matrix(STACKwobs.df[2:ncol(STACKwobs.df)])
pdo <- data.matrix(STACKwpdo.df)

levels <- sort(unique(as.vector(obs)))
result <- do.call(rbind, lapply(1:nrow(obs), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obs[i,] == x)))))
obD <- result

# matrix for pre/post treatment 
post1980 <- matrix(0, nrow = 4, ncol = 32)
post1980[, 11:32] <- 1

post2010 <- matrix(0, nrow = 4, ncol = 32)
post2010[, 26:32] <- 1

# matrix for treatment/control groups
treat <- matrix(0, nrow = 4, ncol = 32)
treat[c(2, 4), ] <- 1

# matrix for treatment group post
int1980 <- matrix(0, nrow = 4, ncol = 32)
int1980[c(2, 4), 11:32] <- 1

int2010 <- matrix(0, nrow = 4, ncol = 32)
int2010[c(2, 4), 26:32] <- 1

# stack c_t covariate data
c1980PDO.data <- rbind(pdo, post1980, treat, int1980)
c1980.data <- rbind(post1980, treat, int1980)

c2010PDO.data <- rbind(pdo, post2010, treat, int2010)
c2010.data <- rbind(post2010, treat, int2010)

# setting up MARSS model inputs
# easy ones
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"
u.model <- "zero"

# Z (underlying states)
z.model <- matrix(
  c(1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    0, 1, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0),
  nrow = 72,
  ncol = 4,
  byrow = TRUE
)

# D (control for observer)
d.model <- matrix(list(0), 2*n, (2*11)*n)
num_rowsD <- nrow(d.model)

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
  d.model[i, cc:(cc+10)] <- obs_vector
}

# C (effect of covariates)
cPDO.model <- matrix(list(0), 4, 14)
cPDO.model[1,1] <- "pE"
cPDO.model[2,1] <- "pE"
cPDO.model[3,2] <- "pO"
cPDO.model[4,2] <- "pO"
cPDO.model[1,3] <- "postE"
cPDO.model[2,4] <- "postE"
cPDO.model[3,5] <- "postO"
cPDO.model[4,6] <- "postO"
cPDO.model[1,7] <- "treaE"
cPDO.model[2,8] <- "treaE"
cPDO.model[3,9] <- "treaO"
cPDO.model[4,10] <- "treaO"
cPDO.model[1,11] <- "intrE"
cPDO.model[2,12] <- "intrE"
cPDO.model[3,13] <- "intrO"
cPDO.model[4,14] <- "intrO"

c.model <- matrix(list(0), 4, 12)
c.model[1,1] <- "postE"
c.model[2,2] <- "postE"
c.model[3,3] <- "postO"
c.model[4,4] <- "postO"
c.model[1,5] <- "treaE"
c.model[2,6] <- "treaE"
c.model[3,7] <- "treaO"
c.model[4,8] <- "treaO"
c.model[1,9] <- "intrE"
c.model[2,10] <- "intrE"
c.model[3,11] <- "intrO"
c.model[4,12] <- "intrO"

# Q (process error)
q.model <- matrix(list(0), 4, 4)
q.model[1,1] <- "qE"
q.model[2,2] <- "qE"
q.model[3,3] <- "qO"
q.model[4,4] <- "qO"

# R (site level observation error)
r.model <- matrix(list(0), 72, 72)
for (i in 1:36) {
  r.model[i,i] <- paste0("r",i)
}
for (i in 1:36) {
  r.model[i+36,i+36] <- paste0("r",i)
}
r.model[6,6] <- "rIndianRiver"
r.model[42,42] <- "rIndianRiver"

# model builds - 1980
# no covariates
model.list_DD1980nul <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = c1980.data)

# observer, no PDO
model.list_DD1980obs <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = c1980.data, D = d.model, d = obD)

# PDO, no observer
model.list_DD1980pdo <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model, c = c1980PDO.data)

# PDO & observer
model.list_DD1980ful <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model, c = c1980PDO.data, D = d.model, d = obD)

# model builds - 2010
# no covariates
model.list_DD2010nul <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = c2010.data)

# observer, no PDO
model.list_DD2010obs <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = c2010.data, D = d.model, d = obD)

# PDO, no observer
model.list_DD2010pdo <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model, c = c2010PDO.data)

# PDO & observer
model.list_DD2010ful <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model, c = c2010PDO.data, D = d.model, d = obD)

# if(!file.exists(here("data", "clean", "boot_ssNSE_DD1980.rds"))){
# model run - 1980
ssNSE_DD1980nul <- MARSS(dat, 
                      model = model.list_DD1980nul, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD1980nul, file=here("data", "clean", "ssNSE_DD1980nul.rds"))

ssNSE_DD1980obs <- MARSS(dat, 
                      model = model.list_DD1980obs, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD1980obs, file=here("data", "clean", "ssNSE_DD1980obs.rds"))

ssNSE_DD1980pdo <- MARSS(dat, 
                      model = model.list_DD1980pdo, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD1980pdo, file=here("data", "clean", "ssNSE_DD1980pdo.rds"))

ssNSE_DD1980ful <- MARSS(dat, 
                      model = model.list_DD1980ful, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD1980ful, file=here("data", "clean", "ssNSE_DD1980ful.rds"))

# AICc comparison
ssNSE_DD1980obs$AICc
  # 3792.056
    # best fit
ssNSE_DD1980ful$AICc
  # 3793.633
    # deltAICc = 1.577, evidence ratio = 2.2
    # equivalent to 68.75% confidence this model is a worse fit
ssNSE_DD1980nul$AICc
  # 3857.415
    # deltAICc = 65.36, evidence ratio =  > 6*e^15
    # equivalent to 99.999999% confidence this model is a worse fit
ssNSE_DD1980pdo$AICc
  # 3858.997
    # deltAICc = 66.94, evidence ratio =  > 2*e^16
    # equivalent to 99.999999% confidence this model is a worse fit

# proceed with model averaging for bootstrap (representative draws from each model)
# find akaike weights
AICc1 <- ssNSE_DD1980obs$AICc
AICc2 <- ssNSE_DD1980ful$AICc

aicc_table <- c(AICc1, AICc2)
delta.aicc <- aicc_table - min(aicc_table)
rel.lik <- exp(-0.5 * delta.aicc)
weights <- rel.lik / sum(rel.lik)

# bootstrap parameter estimation - 1980
runs = 10
# boot_ssNSE_DD1980obs <- MARSSboot(ssNSE_DD1980obs
#                                  , nboot = runs
#                                  , output="parameters"
#                                  , sim = "parametric"
#                                  # , param.gen="hessian"
# )
# boot_ssNSE_DD1980ful <- MARSSboot(ssNSE_DD1980ful
#                                   , nboot = runs
#                                   , output="parameters"
#                                   , sim = "parametric"
#                                   # , param.gen="hessian"
# )
boot_ssNSE_DD1980obs <- MARSSboot(ssNSE_DD1980obs
                                  , nboot = runs
                                  , output="all"
                                  , sim = "parametric"
                                  # , param.gen="hessian"
)$boot.params
boot_ssNSE_DD1980ful <- MARSSboot(ssNSE_DD1980ful
                                  , nboot = runs
                                  , output="parameters"
                                  , sim = "parametric"
                                  # , param.gen="hessian"
)$boot.params

# Get estimates of DD effects from both
bootDD1980obs_postE <- boot_ssNSE_DD1980obs["U.postE", ]
bootDD1980ful_postE <- boot_ssNSE_DD1980ful["U.postE", ]
avg_boot1980postE <- (weights[1]*bootDD1980obs_postE ) + (weights[2]*bootDD1980ful_postE)

bootDD1980obs_treaE <- boot_ssNSE_DD1980obs["U.treaE", ]
bootDD1980ful_treaE <- boot_ssNSE_DD1980ful["U.treaE", ]
avg_boot1980treaE <- (weights[1]*bootDD1980obs_treaE ) + (weights[2]*bootDD1980ful_treaE)

bootDD1980obs_intrE <- boot_ssNSE_DD1980obs["U.intrE", ]
bootDD1980ful_intrE <- boot_ssNSE_DD1980ful["U.intrE", ]
avg_boot1980intrE <- (weights[1]*bootDD1980obs_intrE ) + (weights[2]*bootDD1980ful_intrE)

bootDD1980obs_postO <- boot_ssNSE_DD1980obs["U.postO", ]
bootDD1980ful_postO <- boot_ssNSE_DD1980ful["U.postO", ]
avg_boot1980postO <- (weights[1]*bootDD1980obs_postO ) + (weights[2]*bootDD1980ful_postO)

bootDD1980obs_treaO <- boot_ssNSE_DD1980obs["U.treaO", ]
bootDD1980ful_treaO <- boot_ssNSE_DD1980ful["U.treaO", ]
avg_boot1980treaO <- (weights[1]*bootDD1980obs_treaO ) + (weights[2]*bootDD1980ful_treaO)

bootDD1980obs_intrO <- boot_ssNSE_DD1980obs["U.intrO", ]
bootDD1980ful_intrO <- boot_ssNSE_DD1980ful["U.intrO", ]
avg_boot1980intrO <- (weights[1]*bootDD1980obs_intrO ) + (weights[2]*bootDD1980ful_intrO)

df1980 <- rbind(avg_boot1980postE, avg_boot1980treaE, avg_boot1980intrE,
                avg_boot1980postO, avg_boot1980treaO, avg_boot1980intrO)



# ssNSE_DD1980 <- ssNSE_DD1980obs
# saveRDS(ssNSE_DD1980, file=here("data", "clean", "ssNSE_DD1980.rds"))
# }
# ssNSE_DD1980 <- readRDS(file=here("data", "clean", "ssNSE_DD1980.rds"))

# if(!file.exists(here("data", "clean", "ssNSE_DD2010.rds"))){
# model run - 2010
ssNSE_DD2010nul <- MARSS(dat, 
                      model = model.list_DD2010nul, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD2010nul, file=here("data", "clean", "ssNSE_DD2010nul.rds"))

ssNSE_DD2010obs <- MARSS(dat, 
                      model = model.list_DD2010obs, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD2010obs, file=here("data", "clean", "ssNSE_DD2010obs.rds"))

ssNSE_DD2010pdo <- MARSS(dat, 
                      model = model.list_DD2010pdo, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD2010pdo, file=here("data", "clean", "ssNSE_DD2010pdo.rds"))

ssNSE_DD2010ful <- MARSS(dat, 
                      model = model.list_DD2010ful, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_DD2010ful, file=here("data", "clean", "ssNSE_DD2010ful.rds"))

# AICc comparison
ssNSE_DD2010obs$AICc
  # 3792.188
    # best fit
ssNSE_DD2010ful$AICc
  # 3793.148
    # deltAICc = 0.96, evidence ratio = 1.62
    # equivalent to 61.77% confidence this model is a worse fit
ssNSE_DD2010nul$AICc
  # 3857.26
    # deltAICc = 65.07, evidence ratio =  > 7*e^15
    # equivalent to 99.999999% confidence this model is a worse fit
ssNSE_DD2010pdo$AICc
  # 3857.816
    # deltAICc = 68.78, evidence ratio =  > 1*e^16
    # equivalent to 99.999999% confidence this model is a worse fit

# proceed with model averaging for bootstrap (representative draws from each model)
# find akaike weights
# find akaike weights
AICc1 <- ssNSE_DD2010obs$AICc
AICc2 <- ssNSE_DD2010ful$AICc

aicc_table <- c(AICc1, AICc2)
delta.aicc <- aicc_table - min(aicc_table)
rel.lik <- exp(-0.5 * delta.aicc)
weights <- rel.lik / sum(rel.lik)

boot_ssNSE_DD2010obs <- MARSSboot(ssNSE_DD2010obs
                                  , nboot = runs
                                  , output="all"
                                  , sim = "parametric"
                                  # , param.gen="hessian"
)$boot.params
boot_ssNSE_DD2010ful <- MARSSboot(ssNSE_DD2010ful
                                  , nboot = runs
                                  , output="parameters"
                                  , sim = "parametric"
                                  # , param.gen="hessian"
)$boot.params

# Get estimates of DD effects from both
bootDD2010obs_postE <- boot_ssNSE_DD2010obs["U.postE", ]
bootDD2010ful_postE <- boot_ssNSE_DD2010ful["U.postE", ]
avg_boot2010postE <- (weights[1]*bootDD2010obs_postE ) + (weights[2]*bootDD2010ful_postE)

bootDD2010obs_treaE <- boot_ssNSE_DD2010obs["U.treaE", ]
bootDD2010ful_treaE <- boot_ssNSE_DD2010ful["U.treaE", ]
avg_boot2010treaE <- (weights[1]*bootDD2010obs_treaE ) + (weights[2]*bootDD2010ful_treaE)

bootDD2010obs_intrE <- boot_ssNSE_DD2010obs["U.intrE", ]
bootDD2010ful_intrE <- boot_ssNSE_DD2010ful["U.intrE", ]
avg_boot2010intrE <- (weights[1]*bootDD2010obs_intrE ) + (weights[2]*bootDD2010ful_intrE)

bootDD2010obs_postO <- boot_ssNSE_DD2010obs["U.postO", ]
bootDD2010ful_postO <- boot_ssNSE_DD2010ful["U.postO", ]
avg_boot2010postO <- (weights[1]*bootDD2010obs_postO ) + (weights[2]*bootDD2010ful_postO)

bootDD2010obs_treaO <- boot_ssNSE_DD2010obs["U.treaO", ]
bootDD2010ful_treaO <- boot_ssNSE_DD2010ful["U.treaO", ]
avg_boot2010treaO <- (weights[1]*bootDD2010obs_treaO ) + (weights[2]*bootDD2010ful_treaO)

bootDD2010obs_intrO <- boot_ssNSE_DD2010obs["U.intrO", ]
bootDD2010ful_intrO <- boot_ssNSE_DD2010ful["U.intrO", ]
avg_boot2010intrO <- (weights[1]*bootDD2010obs_intrO ) + (weights[2]*bootDD2010ful_intrO)

df2010 <- rbind(avg_boot2010postE, avg_boot2010treaE, avg_boot2010intrE,
                avg_boot2010postO, avg_boot2010treaO, avg_boot2010intrO)




# ssNSE_DD2010 <- ssNSE_DD2010obs
# saveRDS(ssNSE_DD2010, file=here("data", "clean", "ssNSE_DD2010.rds"))
# }
# ssNSE_DD2010 <- readRDS(file=here("data", "clean", "ssNSE_DD2010.rds"))

# # bootstrap for confidence intervals
# ptm <- proc.time()
# # bootstrap parameter estimation - 1980
# if(!file.exists(here("data", "clean", "boot_ssNSE_DD1980.rds"))){
#   boot_ssNSE_DD1980 <- MARSSboot(ssNSE_DD1980
#                                  , nboot=1000
#                                  , output="parameters"
#                                  , sim = "parametric"
#                                  # , param.gen="hessian"
#   )
#   saveRDS(boot_ssNSE_DD1980, file=here("data", "clean", "boot_ssNSE_DD1980.rds"))
# }
# 
# # bootstrap parameter estimation - 2010
# if(!file.exists(here("data", "clean", "boot_ssNSE_DD2010.rds"))){
#   boot_ssNSE_DD2010 <- MARSSboot(ssNSE_DD2010
#                                  , nboot=1000
#                                  , output="parameters"
#                                  , sim = "parametric"
#                                  # , param.gen="hessian"
#   )
#   saveRDS(boot_ssNSE_DD2010, file=here("data", "clean", "boot_ssNSE_DD2010.rds"))
# }
# 
# boot_ssNSE_DD1980 <- readRDS(file=here("data", "clean", "boot_ssNSE_DD1980.rds"))
# boot_ssNSE_DD2010 <- readRDS(file=here("data", "clean", "boot_ssNSE_DD2010.rds"))
# 
# proc.time()[3] - ptm

# grab bootstrap parameter estimates for interaction term
# df1980 <- boot_ssNSE_DD1980$boot.params
df1980 <- data.frame(t(df1980))
# df1980 <- df1980[, -c(1:47, 54:59)]
df1980_E <- df1980[-c(4:6)]
df1980_O <- df1980[-c(1:3)]

# df2010 <- boot_ssNSE_DD2010$boot.params
df2010 <- data.frame(t(df2010))
# df2010 <- df2010[, -c(1:47, 54:59)]
df2010_E <- df2010[-c(4:6)]
df2010_O <- df2010[-c(1:3)]

# rename columns
names(df1980_E)[names(df1980_E) == "avg_boot1980postE"] <- "Effect of treatment period"
names(df1980_E)[names(df1980_E) == "avg_boot1980treaE"] <- "Effect of treatment group"
names(df1980_E)[names(df1980_E) == "avg_boot1980intrE"] <- "Interaction effect" # parameter of interest

names(df1980_O)[names(df1980_O) == "avg_boot1980postO"] <- "Effect of treatment period"
names(df1980_O)[names(df1980_O) == "avg_boot1980treaO"] <- "Effect of treatment group"
names(df1980_O)[names(df1980_O) == "avg_boot1980intrO"] <- "Interaction effect" # parameter of interest

names(df2010_E)[names(df2010_E) == "U.postE"] <- "Effect of treatment period"
names(df2010_E)[names(df2010_E) == "U.treaE"] <- "Effect of treatment group" 
names(df2010_E)[names(df2010_E) == "U.intrE"] <- "Interaction effect" # parameter of interest

names(df2010_O)[names(df2010_O) == "U.postO"] <- "Effect of treatment period"
names(df2010_O)[names(df2010_O) == "U.treaO"] <- "Effect of treatment group"
names(df2010_O)[names(df2010_O) == "U.intrO"] <- "Interaction effect" # parameter of interest

# plotting results
# set data real long - 1980 
wdf1980_E <- df1980_E |>
  pivot_longer(
    cols = c(colnames(df1980_E)),
    names_to = "effect",
    values_to = "value"
  )

wdf1980_O <- df1980_O |>
  pivot_longer(
    cols = c(colnames(df1980_O)),
    names_to = "effect",
    values_to = "value"
  )

wdf1980_E$run <- "Even-year"
wdf1980_O$run <- "Odd-year"

df1980 <- bind_rows(wdf1980_E, wdf1980_O)

# set data real long - 2010
wdf2010_E <- df2010_E |>
  pivot_longer(
    cols = c(colnames(df2010_E)),
    names_to = "effect",
    values_to = "value"
  )

wdf2010_O <- df2010_O |>
  pivot_longer(
    cols = c(colnames(df2010_O)),
    names_to = "effect",
    values_to = "value"
  )

wdf2010_E$run <- "Even-year"
wdf2010_O$run <- "Odd-year"

df2010 <- bind_rows(wdf2010_E, wdf2010_O)

# plot - 1980
bplot1980 <- ggplot(df1980, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = "1980",
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) + 
  annotate("text", x = Inf, y = Inf, label = "1980", 
            hjust = 1, vjust = 1, size = 10) 
bplot1980

# plot - 2010
bplot2010 <- ggplot(df2010, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = "2010",
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) + 
  annotate("text", x = Inf, y = Inf, label = "2010", 
           hjust = 1, vjust = 1, size = 10) 
bplot2010

# necessary to undertake event studies here
# augment data to drop post treatment period
dat1980 <- dat[, -c(11:32)]
dat2010 <- dat[, -c(26:32)]

obs1980 <- obs[, -c(11:32)]
levels <- sort(unique(as.vector(obs1980)))
result <- do.call(rbind, lapply(1:nrow(obs1980), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obs1980[i,] == x)))))
obD1980 <- result

obs2010 <- obs[, -c(26:32)]
levels <- sort(unique(as.vector(obs2010)))
result <- do.call(rbind, lapply(1:nrow(obs2010), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obs2010[i,] == x)))))
obD2010 <- result

# speficy model params
# matrix for treatment/control groups
treat1980 <- matrix(0, nrow = 4, ncol = 10)
treat1980[c(2, 4), ] <- 1

treat2010 <- matrix(0, nrow = 4, ncol = 25)
treat2010[c(2, 4), ] <- 1

# stack c_t covariate data
c1980.data <- treat1980
c2010.data <- treat2010

# setting up MARSS model inputs
# easy ones
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"
u.model <- "zero"

# Z (underlying states)
z.model <- matrix(
  c(1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    0, 1, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    1, 0, 0, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0,
    0, 0, 1, 0),
  nrow = 72,
  ncol = 4,
  byrow = TRUE
)

# D (control for observer)
d1980.model <- matrix(list(0), 2*n, (2*4)*n)
num_rowsD <- nrow(d1980.model)

obs_vector <- c("obs05",
                "obs07",
                "obs09",
                "obs11")

for (i in 1:num_rowsD) {
  cc <- 4*i - 3
  d1980.model[i, cc:(cc+3)] <- obs_vector
}

d2010.model <- matrix(list(0), 2*n, (2*8)*n)
num_rowsD <- nrow(d2010.model)

obs_vector <- c("obs01",
                "obs02",
                "obs03",
                "obs05",
                "obs06",
                "obs07",
                "obs09",
                "obs11")

for (i in 1:num_rowsD) {
  cc <- 8*i - 7
  d2010.model[i, cc:(cc+7)] <- obs_vector
}

# C (effect of covariates)
c.model <- matrix(list(0), 4, 4)
c.model[1,1] <- "treaE"
c.model[2,2] <- "treaE"
c.model[3,3] <- "treaO"
c.model[4,4] <- "treaO"

# Q (process error)
q.model <- matrix(list(0), 4, 4)
q.model[1,1] <- "qE"
q.model[2,2] <- "qE"
q.model[3,3] <- "qO"
q.model[4,4] <- "qO"

# R (site level observation error)
r.model <- matrix(list(0), 72, 72)
for (i in 1:36) {
  r.model[i,i] <- paste0("r",i)
}
for (i in 1:36) {
  r.model[i+36,i+36] <- paste0("r",i)
}
r.model[6,6] <- "rIndianRiver"
r.model[42,42] <- "rIndianRiver"

# model lists
# 1980 observer, no PDO
model.list_DD1980event <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = c1980.data, D = d1980.model, d = obD1980)

# 2010 observer, no PDO
model.list_DD2010event <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = c2010.data, D = d2010.model, d = obD2010)

# models
if(!file.exists(here("data", "clean", "ssNSE_DD1980event.rds"))){
ssNSE_DD1980event <- MARSS(dat1980, 
                         model = model.list_DD1980event, 
                         method = "kem",
                         control = list(maxit = 1000))
saveRDS(ssNSE_DD1980event, file=here("data", "clean", "ssNSE_DD1980event.rds"))
}

if(!file.exists(here("data", "clean", "ssNSE_DD2010event.rds"))){
ssNSE_DD2010event <- MARSS(dat2010, 
                         model = model.list_DD2010event, 
                         method = "kem",
                         control = list(maxit = 1000))
saveRDS(ssNSE_DD2010event, file=here("data", "clean", "ssNSE_DD2010event.rds"))
}

ssNSE_DD1980event <- readRDS(file=here("data", "clean", "ssNSE_DD1980event.rds"))
ssNSE_DD2010event <- readRDS(file=here("data", "clean", "ssNSE_DD2010event.rds"))

# bootstrap for treatment effect
ptm <- proc.time()
# bootstrap parameter estimation - 1980
if(!file.exists(here("data", "clean", "boot_ssNSE_DD1980event.rds"))){
  boot_ssNSE_DD1980event <- MARSSboot(ssNSE_DD1980event
                                 , nboot=100
                                 , output="parameters"
                                 , sim = "parametric"
                                 # , param.gen="hessian"
  )
  saveRDS(boot_ssNSE_DD1980event, file=here("data", "clean", "boot_ssNSE_DD1980event.rds"))
}

# bootstrap parameter estimation - 2010
if(!file.exists(here("data", "clean", "boot_ssNSE_DD2010event.rds"))){
  boot_ssNSE_DD2010event <- MARSSboot(ssNSE_DD2010event
                                 , nboot=100
                                 , output="parameters"
                                 , sim = "parametric"
                                 # , param.gen="hessian"
  )
  saveRDS(boot_ssNSE_DD2010event, file=here("data", "clean", "boot_ssNSE_DD2010event.rds"))
}
boot_ssNSE_DD1980event <- readRDS(file=here("data", "clean", "boot_ssNSE_DD1980event.rds"))
boot_ssNSE_DD2010event <- readRDS(file=here("data", "clean", "boot_ssNSE_DD2010event.rds"))
proc.time()[3] - ptm

# grab bootstrap parameter estimates for treatment term
df1980 <- boot_ssNSE_DD1980event$boot.params
df1980 <- data.frame(t(df1980))
df1980 <- df1980[, -c(1:40, 43:48)]
df1980_E <- df1980[-c(2)]
df1980_O <- df1980[-c(1)]

df2010 <- boot_ssNSE_DD2010event$boot.params
df2010 <- data.frame(t(df2010))
df2010 <- df2010[, -c(1:44, 47:52)]
df2010_E <- df2010[-c(2)]
df2010_O <- df2010[-c(1)]

# rename columns
names(df1980_E)[names(df1980_E) == "U.treaE"] <- "Pre-1980" 
names(df1980_O)[names(df1980_O) == "U.treaO"] <- "Pre-1980" 
names(df2010_E)[names(df2010_E) == "U.treaE"] <- "Pre-2010" 
names(df2010_O)[names(df2010_O) == "U.treaO"] <- "Pre-2010" 

# plotting results
# set data real long - 1980 
wdf1980_E <- df1980_E |>
  pivot_longer(
    cols = c(colnames(df1980_E)),
    names_to = "effect",
    values_to = "value"
  )

wdf1980_O <- df1980_O |>
  pivot_longer(
    cols = c(colnames(df1980_O)),
    names_to = "effect",
    values_to = "value"
  )

wdf1980_E$run <- "Even-year"
wdf1980_O$run <- "Odd-year"

df1980 <- bind_rows(wdf1980_E, wdf1980_O)

# set data real long - 2010
wdf2010_E <- df2010_E |>
  pivot_longer(
    cols = c(colnames(df2010_E)),
    names_to = "effect",
    values_to = "value"
  )

wdf2010_O <- df2010_O |>
  pivot_longer(
    cols = c(colnames(df2010_O)),
    names_to = "effect",
    values_to = "value"
  )

wdf2010_E$run <- "Even-year"
wdf2010_O$run <- "Odd-year"

df2010 <- bind_rows(wdf2010_E, wdf2010_O)

df <- bind_rows(df1980, df2010)

# plot - 1980
bplot1980Event <- ggplot(df1980, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) 
bplot1980Event

# plot - 2010
bplot2010Event <- ggplot(df2010, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank())
bplot2010Event

# plot - both
bplotEvent <- ggplot(df, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.55), width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = "Effect of Indian River in pre-treatment period",
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0, unit = "pt")),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.title = element_blank())
bplotEvent
  # event studies show no significant difference at Indian River in pretreatment period
  # parellel trends assumption holds

ggsave(here("output", "figures", "bplotEvent.png"), plot=bplotEvent, device="png", dpi=300) 

bplot1980
bplot2010

# stacking figs
# Shared label
# bplot1980$labels$y <- bplot2010$labels$y <- " "
# ylab <- wrap_elements(
#   full = textGrob(
#     "State Estimate (standardized)",
#     rot = 90,
#     gp = gpar(fontsize = 18)
#   )
# )

# combine
bplot1980$labels$y <- bplot2010$labels$y <- " "
STKbplotDD <- (bplot1980/bplot2010) +
  plot_layout(widths = c(0.01, 1)) +
  plot_layout(guides = "collect")
STKbplotDD

ggsave(here("output", "figures", "STKbplotDD.png"), plot=STKbplotDD, device="png", dpi=300)

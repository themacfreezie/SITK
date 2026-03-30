# packages
library(here)
library(MARSS)
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
oddCol_wpdo <- Wpdo[, seq(ncol(Wpdo)) %% 2 == 1]
eveCol_wpdo <- Wpdo[, seq(ncol(Wpdo)) %% 2 != 1]

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
ssNSE_DD1980obs$AICc
  # 3792.056
    # best fit
ssNSE_DD1980ful$AICc
  # 3796.116
    # deltAICc = 4.06, evidence ratio = 7.614
    # equivalent to 88.39% confidence this model is a worse fit
ssNSE_DD1980nul$AICc
  # 3857.415
    # deltAICc = 65.36, evidence ratio =  > 6*e^15
    # equivalent to 99.999999% confidence this model is a worse fit
ssNSE_DD1980pdo$AICc
  # 3861.515
    # deltAICc = 69.46, evidence ratio =  > 8*e^16
    # equivalent to 99.999999% confidence this model is a worse fit

ssNSE_DD2010obs$AICc
  # 3792.188
    # best fit
ssNSE_DD2010ful$AICc
  # 3795.871
    # deltAICc = 3.68, evidence ratio = 6.31
    # equivalent to 86.31% confidence this model is a worse fit
ssNSE_DD2010nul$AICc
  # 3857.26
    # deltAICc = 65.07, evidence ratio =  > 7*e^15
    # equivalent to 99.999999% confidence this model is a worse fit
ssNSE_DD2010pdo$AICc
  # 3860.971
    # deltAICc = 68.78, evidence ratio =  > 1*e^16
    # equivalent to 99.999999% confidence this model is a worse fit

# for now we'll proceed with best-fit models
ssNSE_DD1980 <- ssNSE_DD1980obs
ssNSE_DD2010 <- ssNSE_DD2010obs

saveRDS(ssNSE_DD1980, file=here("data", "clean", "ssNSE_DD1980.rds"))
saveRDS(ssNSE_DD2010, file=here("data", "clean", "ssNSE_DD2010.rds"))

# bootstrap for confidence intervals
ptm <- proc.time()
# bootstrap parameter estimation - 1980
if(!file.exists(here("data", "clean", "boot_ssNSE_DD1980.rds"))){
  boot_ssNSE_DD1980 <- MARSSboot(ssNSE_DD1980
                                 , nboot=1000
                                 , output="parameters"
                                 , sim = "parametric"
                                 # , param.gen="hessian"
  )
  saveRDS(boot_ssNSE_DD1980, file=here("data", "clean", "boot_ssNSE_DD1980.rds"))
}

# bootstrap parameter estimation - 2010
if(!file.exists(here("data", "clean", "boot_ssNSE_DD2010.rds"))){
  boot_ssNSE_DD2010 <- MARSSboot(ssNSE_DD2010
                                 , nboot=1000
                                 , output="parameters"
                                 , sim = "parametric"
                                 # , param.gen="hessian"
  )
  saveRDS(boot_ssNSE_DD2010, file=here("data", "clean", "boot_ssNSE_DD2010.rds"))
}

boot_ssNSE_DD1980 <- readRDS(file=here("data", "clean", "boot_ssNSE_DD1980.rds"))
boot_ssNSE_DD2010 <- readRDS(file=here("data", "clean", "boot_ssNSE_DD2010.rds"))

proc.time()[3] - ptm

# grab bootstrap parameter estimates for interaction term
df1980 <- boot_ssNSE_DD1980$boot.params
df1980 <- data.frame(t(df1980))
df1980 <- df1980[, -c(1:47, 54:59)]
df1980_E <- df1980[-c(2,4,6)]
df1980_O <- df1980[-c(1,3,5)]

df2010 <- boot_ssNSE_DD2010$boot.params
df2010 <- data.frame(t(df2010))
df2010 <- df2010[, -c(1:47, 54:59)]
df2010_E <- df2010[-c(2,4,6)]
df2010_O <- df2010[-c(1,3,5)]

# rename columns
names(df1980_E)[names(df1980_E) == "U.postE"] <- "Post-1980"
names(df1980_E)[names(df1980_E) == "U.treaE"] <- "Treatment group" 
names(df1980_E)[names(df1980_E) == "U.intrE"] <- "Interaction effect" # parameter of interest

names(df1980_O)[names(df1980_O) == "U.postO"] <- "Post-1980"
names(df1980_O)[names(df1980_O) == "U.treaO"] <- "Treatment group" 
names(df1980_O)[names(df1980_O) == "U.intrO"] <- "Interaction effect" # parameter of interest

names(df2010_E)[names(df2010_E) == "U.postE"] <- "Post-2010"
names(df2010_E)[names(df2010_E) == "U.treaE"] <- "Treatment group" 
names(df2010_E)[names(df2010_E) == "U.intrE"] <- "Interaction effect" # parameter of interest

names(df2010_O)[names(df2010_O) == "U.postO"] <- "Post-2010"
names(df2010_O)[names(df2010_O) == "U.treaO"] <- "Treatment group" 
names(df2010_O)[names(df2010_O) == "U.intrO"] <- "Interaction effect" # parameter of interest
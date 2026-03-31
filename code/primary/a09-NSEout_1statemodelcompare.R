library(here)
library(MARSS)

# set loc
here::i_am("code/primary/a09-NSEout_1statemodelcompare.R")
options(max.print=2000)

# load most parsimonious 2-state model
ssNSE_4state <- readRDS(here("data", "clean", "ssNSE.rds"))

# build 1-state model

# load and clean data 
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

# setting up MARSS model inputs
# easy ones
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"

# U (underlying trend in process) 
#   **mark always says 'trend' isn't a good way to think about this
uZERO.model <- "zero"

# Z (underlying states)
# Z 2 state
top_half <- matrix(c(1, 0), nrow = 36, ncol = 2, byrow = TRUE)
bottom_half <- matrix(c(0, 1), nrow = 36, ncol = 2, byrow = TRUE)
z.model_2state <- rbind(top_half, bottom_half)

# Z 3 state: 2 state even, 1 state odd
top_half <- matrix(c(1, 0, 0), nrow = 36, ncol = 3, byrow = TRUE)
indriv_row <- c(0, 1, 0)
top_half[6, ] <- indriv_row
bottom_half <- matrix(c(0, 0, 1), nrow = 36, ncol = 3, byrow = TRUE)
z.model_3state_2E1O <- rbind(top_half, bottom_half)

# Z 3 state: 1 state even, 2 state odd
top_half <- matrix(c(1, 0, 0), nrow = 36, ncol = 3, byrow = TRUE)
bottom_half <- matrix(c(0, 1, 0), nrow = 36, ncol = 3, byrow = TRUE)
indriv_row <- c(0, 0, 1)
bottom_half[6, ] <- indriv_row
z.model_3state_1E2O <- rbind(top_half, bottom_half)


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


# C (effect of pdo) 
# C 2 states
c.model_2state <- matrix(list(0), 2, 2)
c.model_2state[1,1] <- "pE"
c.model_2state[2,2] <- "pO"

# C 3 state: 2 state even, 1 state odd
c.model_3state_2E1O <- matrix(list(0), 3, 2)
c.model_3state_2E1O[1,1] <- "pE"
c.model_3state_2E1O[2,1] <- "pE"
c.model_3state_2E1O[3,2] <- "pO"

# C 3 state: 1 state even, 2 state odd
c.model_3state_1E2O <- matrix(list(0), 3, 2)
c.model_3state_1E2O[1,1] <- "pE"
c.model_3state_1E2O[2,2] <- "pO"
c.model_3state_1E2O[3,2] <- "pO"


# Q (process error)
# Q 2 states
q.model_2state <- matrix(list(0), 2, 2)
q.model_2state[1,1] <- "qE"
q.model_2state[2,2] <- "qO"

# Q 3 state: 2 state even, 1 state odd
q.model_3state_2E1O <- matrix(list(0), 3, 3)
q.model_3state_2E1O[1,1] <- "qE"
q.model_3state_2E1O[2,2] <- "qE"
q.model_3state_2E1O[3,3] <- "qO"

# Q 3 state: 1 state even, 2 state odd
q.model_3state_1E2O <- matrix(list(0), 3, 3)
q.model_3state_1E2O[1,1] <- "qE"
q.model_3state_1E2O[2,2] <- "qO"
q.model_3state_1E2O[3,3] <- "qO"


# R (site level observation error)
rSITE.model <- matrix(list(0), 72, 72)
for (i in 1:36) {
  rSITE.model[i,i] <- paste0("r",i)
}
for (i in 1:36) {
  rSITE.model[i+36,i+36] <- paste0("r",i)
}
rSITE.model[6,6] <- "rIndianRiver"
rSITE.model[42,42] <- "rIndianRiver"

# renaming to match (last check)
u1 <- uZERO.model
# q2 <- qRUN.model
r1 <- rSITE.model

model.list_2state <- list(
  B = b.model, U = u1, Q = q.model_2state,
  Z = z.model_2state, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_2state, c = pdo, D = d.model, d = obD)

model.list_3state_2E1O <- list(
  B = b.model, U = u1, Q = q.model_3state_2E1O,
  Z = z.model_3state_2E1O, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_3state_2E1O, c = pdo, D = d.model, d = obD)

model.list_3state_1E2O <- list(
  B = b.model, U = u1, Q = q.model_3state_1E2O,
  Z = z.model_3state_1E2O, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model_3state_1E2O, c = pdo, D = d.model, d = obD)

# run model
ssNSE_2state <- MARSS(dat, 
                      model = model.list_2state, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_2state, file=here("data", "clean", "ssNSE_2state.rds"))

ssNSE_3state_2E1O <- MARSS(dat, 
                      model = model.list_3state_2E1O, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_3state_2E1O, file=here("data", "clean", "ssNSE_3state_2E1O.rds"))

ssNSE_3state_1E2O <- MARSS(dat, 
                      model = model.list_3state_1E2O, 
                      method = "kem",
                      control = list(maxit = 1000))
saveRDS(ssNSE_3state_1E2O, file=here("data", "clean", "ssNSE_3state_1E2O.rds"))

# AICc comparison
ssNSE_3state_1E2O$AICc
  # 3770.176
    # best fit
ssNSE_4state$AICc
  # 3782.953
    # deltAICc = 12.777, evidence ratio = 594.96
    # meaning 3state_1E2O is 595 times more likely than a 4 state system
      # equivalent to 99.83% confidence this model is a worse fit
ssNSE_2state$AICc
  # 3797.115
    # also worse fit than the 4 state system
    # deltAICc = 26.939, evidence ratio = 707,505
      # basically 100% confidence this is a worse fit (99.99986%)
ssNSE_3state_2E1O$AICc
  # 3817.099
    # worst fit by a lot
    # deltAICc = 46.923, evidence ratio = 15,459,661,737
      # extreme confidence this is a worse fit (99.999999993%)

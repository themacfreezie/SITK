library(here) # set workind directory
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/modelexplore.R")
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


# binary_matrices <- lapply(levels, function(level) {
#   # Create a logical matrix: 1 if TRUE, 0 if FALSE
#   +(obs == level)
# })

# df <- STACKwobs.df[2:ncol(STACKwobs.df)]
# df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)],
#                                        as.factor)
# binary_df <- dummy_cols(df, remove_first_dummy = FALSE)

# grab IR data for output
IRdata <- as.numeric(as.vector(dat[6,]))
saveRDS(IRdata, file=here("data", "clean", "oIRdata.rds"))

IRdata <- as.numeric(as.vector(dat[42,]))
saveRDS(IRdata, file=here("data", "clean", "eIRdata.rds"))

# setting up MARSS model inputs
# easy ones
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"

# U (underlying trend in process)
uZERO.model <- "zero"
uTREND.model <- matrix(paste0("u", seq(4)))

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

# C (effect of pdo)
c.model <- matrix(list(0), 4, 2)
c.model[1,1] <- "pE"
c.model[2,1] <- "pE"
c.model[3,2] <- "pO"
c.model[4,2] <- "pO"

# Q (process error)
qRUN.model <- matrix(list(0), 4, 4)
qRUN.model[1,1] <- "qE"
qRUN.model[2,2] <- "qE"
qRUN.model[3,3] <- "qO"
qRUN.model[4,4] <- "qO"

qALL.model <- matrix(list(0), 4, 4)
qALL.model[1,1] <- "qEregion"
qALL.model[2,2] <- "qEir"
qALL.model[3,3] <- "qOregrion"
qALL.model[4,4] <- "qOir"

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

rEVC.model <- "equalvarcov" 

# CHECKING MODEL SPECS
u1 <- uZERO.model
u2 <- uTREND.model

q1 <- qALL.model
q2 <- qRUN.model

r1 <- rSITE.model
r2 <- rEVC.model

# model builds
model.list_u1q1r1 <- list(
  B = b.model, U = u1, Q = q1,
  Z = z.model, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u1q1r2 <- list(
  B = b.model, U = u1, Q = q1,
  Z = z.model, A = a.model, R = r2,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u2q1r1 <- list(
  B = b.model, U = u2, Q = q1,
  Z = z.model, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u2q1r2 <- list(
  B = b.model, U = u2, Q = q1,
  Z = z.model, A = a.model, R = r2,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u1q2r1 <- list(
  B = b.model, U = u1, Q = q2,
  Z = z.model, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u1q2r2 <- list(
  B = b.model, U = u1, Q = q2,
  Z = z.model, A = a.model, R = r2,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u2q2r1 <- list(
  B = b.model, U = u2, Q = q2,
  Z = z.model, A = a.model, R = r1,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

model.list_u2q2r2 <- list(
  B = b.model, U = u2, Q = q2,
  Z = z.model, A = a.model, R = r2,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= c.model, c = pdo, D = d.model, d = obD)

# run MARSS models
ptm <- proc.time()
if(!file.exists(here("data", "clean", "ssNSE.rds"))){
ssNSE_u1q1r1 <- MARSS(dat, 
               model = model.list_u1q1r1, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u1q1r2 <- MARSS(dat, 
               model = model.list_u1q1r2, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u2q1r1 <- MARSS(dat, 
               model = model.list_u2q1r1, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u2q1r2 <- MARSS(dat, 
               model = model.list_u2q1r2, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u1q2r1 <- MARSS(dat, 
               model = model.list_u1q2r1, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u1q2r2 <- MARSS(dat, 
               model = model.list_u1q2r2, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u2q2r1 <- MARSS(dat, 
               model = model.list_u2q2r1, 
               method = "kem",
               control = list(maxit = 1000))

ssNSE_u2q2r2 <- MARSS(dat, 
               model = model.list_u2q2r2, 
               method = "kem",
               control = list(maxit = 1000))

MARSSaic(ssNSE_u1q1r1, output = c("AIC"))
MARSSaic(ssNSE_u1q1r2, output = c("AIC"))
MARSSaic(ssNSE_u2q1r1, output = c("AIC"))
MARSSaic(ssNSE_u2q1r2, output = c("AIC"))
MARSSaic(ssNSE_u1q2r1, output = c("AIC"))
MARSSaic(ssNSE_u1q2r2, output = c("AIC"))
MARSSaic(ssNSE_u2q2r1, output = c("AIC"))
MARSSaic(ssNSE_u2q2r2, output = c("AIC"))

ssNSE <- ssNSE_u1q2r1
saveRDS(ssNSE, file=here("data", "clean", "ssNSE.rds"))
}
proc.time()[3] - ptm
ssNSE <- readRDS(here("data", "clean", "ssNSE.rds"))

# some plots - Indian River (data v. fitted)
states <- ssNSE$states
ytT <- ssNSE$call$data
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

df <- df[-c(1, 3, 5:9, 11:45, 47:76)]

names(df)[names(df) == "X2"] <- "IRe.x"
names(df)[names(df) == "X4"] <- "IRo.x"
names(df)[names(df) == "X.5"] <- "IRe.y"
names(df)[names(df) == "X.41"] <- "IRo.y"

df_E <- df[-c(2,4)]
df_O <- df[-c(1,3)]

df_E$index <- 1:nrow(df_E)
df_E <- df_E %>% 
  pivot_longer(
    cols = c("IRe.x", "IRe.y"),
    names_to = "measure",
    values_to = "value"
  )

df_O$index <- 1:nrow(df_O)
df_O <- df_O %>% 
  pivot_longer(
    cols = c("IRo.x", "IRo.y"),
    names_to = "measure",
    values_to = "value"
  )

ggplot(df_E, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="Even year runs",
       y=NULL) +
  theme_classic()

ggplot(df_O, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="Odd year runs",
       y=NULL) +
  theme_classic()

## some plots - all streams
# let's see those estimates
fitted(ssNSE)

#grabbing data for figures
statesNSE.est <- ssNSE$states
statesNSEse.est <- ssNSE$states.se
statesNSE.est
statesNSEse.est

# plot variance terms
statesNSE.df <- as.data.frame(statesNSE.est)
statesNSEse.df <- as.data.frame(statesNSEse.est)

statesLongNSE <- statesNSE.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="fitted")

yearsNSEe <- c(1960, 1962, 1964, 1966, 1968,
               1970, 1972, 1974, 1976, 1978,
               1980, 1982, 1984, 1986, 1988,
               1990, 1992, 1994, 1996, 1998,
               2000, 2002, 2004, 2006, 2008,
               2010, 2012, 2014, 2016, 2018,
               2020, 2022)
yearsNSEo <- c(1961, 1963, 1965, 1967, 1969,
               1971, 1973, 1975, 1977, 1979,
               1981, 1983, 1985, 1987, 1989,
               1991, 1993, 1995, 1997, 1999,
               2001, 2003, 2005, 2007, 2009,
               2011, 2013, 2015, 2017, 2019,
               2021, 2023)

statesLongNSE <- statesLongNSE %>% 
  mutate(Year = c(rep(yearsNSEe, 2), rep(yearsNSEo, 2)))

statesNSE <- c("NSE Outer", "Indian River")
statesLongNSE <- statesLongNSE %>% 
  mutate(state = c(rep(statesNSE[1], 32),
                   rep(statesNSE[2], 32),
                   rep(statesNSE[1], 32),
                   rep(statesNSE[2], 32)))

runNSE <- c("Even", "Odd")
statesLongNSE <- statesLongNSE %>% 
  mutate(run= c(rep(runNSE[1], 64),
                rep(runNSE[2], 64)))

statesseLongNSE <- statesNSEse.df %>% 
  pivot_longer("V1":"V32", names_to="Year", values_to="se")

statesLongNSE <- statesLongNSE %>% 
  mutate(se = statesseLongNSE$se)

statesLongNSE$ub = statesLongNSE$fitted + statesLongNSE$se
statesLongNSE$lb = statesLongNSE$fitted - statesLongNSE$se

# save long state estimates
save(statesLongNSE, file=here("data", "clean", "STKste_statesLongNSE.Rda"))

# split into odd and even runs
statesLongNSE_E <- statesLongNSE[statesLongNSE$run == 'Even', ]
statesLongNSE_O <- statesLongNSE[statesLongNSE$run != 'Even', ]

# plotting
STKsteNSE_E <- ggplot(data = statesLongNSE_E, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estimates of even-year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteNSE_E

STKsteNSE_O <- ggplot(data = statesLongNSE_O, aes(y=fitted, x=Year, color = state)) +
  geom_ribbon(aes(ymin=lb, ymax=ub, fill=state), alpha=0.35, linetype=0) +
  geom_line(show.legend = FALSE) +
  theme_classic() +  
  gghighlight(state == "Indian River", use_direct_label = FALSE, unhighlighted_params = list(color="grey70")) +
  labs(x = "", 
       y="State Estimate (standardized)",
       title='Estimates of odd-year pink salmon peak returns in 36 Southeast Alaska streams',
       subtitle='Indian River highlighted') +
  theme(legend.position="none") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
STKsteNSE_O
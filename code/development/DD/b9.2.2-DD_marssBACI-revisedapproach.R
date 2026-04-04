# packages
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/DD/b9.1-DD_marssBACI-minimum example.R")
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

# Define the start and end columns for the treatment
start_col <- 11
end_col <- 32

# Loop through each step to create a new matrix
for (i in start_col:end_col) {
  
  # 1. Create a blank 4x32 matrix of zeros
  temp_matrix <- matrix(0, nrow = 4, ncol = 32)
  
  # 2. Fill columns from the 11th up to the current index 'i' with 1s
  temp_matrix[, start_col:i] <- 1
  
  # 3. Calculate how many columns have 1s to create the name
  num_ones <- i - start_col + 1
  matrix_name <- paste0("treat_cols_", num_ones)
  
  temp_matrix <- rbind(pdo, temp_matrix)
  
  # 4. Assign the matrix to that name in the global environment
  assign(matrix_name, temp_matrix)
}

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

# C (effect of covariates)
cPDO.model <- matrix(list(0), 4, 6)
cPDO.model[1,1] <- "pE"
cPDO.model[2,1] <- "pE"
cPDO.model[3,2] <- "pO"
cPDO.model[4,2] <- "pO"
cPDO.model[1,3] <- "treatE_region"
cPDO.model[2,4] <- "treatE_ir"
cPDO.model[3,5] <- "treatO_region"
cPDO.model[4,6] <- "treatO_ir"

# Initialize an empty list to store the results
marss_summary <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)


# Loop through differenct specs
for (i in 1:22) {
  
  # 1. Get the specific matrix for this year from the workspace
  current_treat <- get(paste0("treat_cols_", i))
  
  # 2. Update the model list with the current treatment matrix
  model.list <- list(
    B = b.model, U = u.model, Q = q.model,
    Z = z.model, A = a.model, R = r.model,
    x0 = x.model, V0 = v.model, tinitx = 0,
    C = cPDO.model, 
    c = current_treat, # This replaces treat_cols_i
    D = d.model, d = obD
  )
  
  # 3. Run the MARSS model
  # 'try' ensures the loop continues even if one model fails to converge
  model_name <- paste0("ssNSE_DD_", i)
  
  fit <- MARSS(
    dat, 
    model = model.list, 
    method = "kem",
    control = list(maxit = 1000)
  )
  
  # 3. Extract AICc if the model ran successfully
  if (!inherits(fit, "try-error")) {
    current_aicc <- fit$AICc
  } else {
    current_aicc <- NA  # Assign NA if the model failed
  }
  
  # 4. Append the results to the data frame
  marss_summary <- rbind(marss_summary, data.frame(
    Model_Name = model_name,
    AICc = current_aicc
  ))
}

# C (effect of covariates)
cPDO.model1 <- matrix(list(0), 4, 6)
cPDO.model1[1,1] <- "pE"
cPDO.model1[2,1] <- "pE"
cPDO.model1[3,2] <- "pO"
cPDO.model1[4,2] <- "pO"
cPDO.model1[1,3] <- "treatE_region"
cPDO.model1[2,4] <- "treatE_ir"
cPDO.model1[3,5] <- "treatO_region"
cPDO.model1[4,6] <- "treatO_ir"

model.list_DD1980ful1 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C = cPDO.model, 
  c = treat_cols_9, # This replaces treat_cols_i
  D = d.model, d = obD
)

ssNSE_DD1980ful1 <- MARSS(dat, 
                          model = model.list_DD1980ful1,
                          method = "kem",
                          control = list(maxit = 1000))

# C (effect of covariates)
cPDO.model2 <- matrix(list(0), 4, 6)
cPDO.model2[1,1] <- "pE"
cPDO.model2[2,1] <- "pE"
cPDO.model2[3,2] <- "pO"
cPDO.model2[4,2] <- "pO"
cPDO.model2[1,3] <- "treatE"
cPDO.model2[2,4] <- "treatE"
cPDO.model2[3,5] <- "treatO"
cPDO.model2[4,6] <- "treatO"

# model builds - 1980
# PDO & observer
model.list_DD1980ful2 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model2, c = treat_cols_9, D = d.model, d = obD)

ssNSE_DD1980ful2 <- MARSS(dat, 
                          model = model.list_DD1980ful2, 
                          method = "kem",
                          control = list(maxit = 1000))

# C (effect of covariates)
cPDO.model3 <- matrix(list(0), 4, 6)
cPDO.model3[1,1] <- "pE"
cPDO.model3[2,1] <- "pE"
cPDO.model3[3,2] <- "pO"
cPDO.model3[4,2] <- "pO"
cPDO.model3[1,3] <- "treatE_region"
cPDO.model3[2,4] <- "treatE_ir"
cPDO.model3[3,5] <- "treatO"
cPDO.model3[4,6] <- "treatO"

# model builds - 1980
# PDO & observer
model.list_DD1980ful3 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model3, c = treat_cols_9, D = d.model, d = obD)

ssNSE_DD1980ful3 <- MARSS(dat, 
                          model = model.list_DD1980ful3, 
                          method = "kem",
                          control = list(maxit = 1000))

# C (effect of covariates)
cPDO.model4 <- matrix(list(0), 4, 6)
cPDO.model4[1,1] <- "pE"
cPDO.model4[2,1] <- "pE"
cPDO.model4[3,2] <- "pO"
cPDO.model4[4,2] <- "pO"
cPDO.model4[1,3] <- "treatE"
cPDO.model4[2,4] <- "treatE"
cPDO.model4[3,5] <- "treatO_region"
cPDO.model4[4,6] <- "treatO_ir"

# model builds - 1980
# PDO & observer
model.list_DD1980ful4 <- list(
  B = b.model, U = u.model, Q = q.model,
  Z = z.model, A = a.model, R = r.model,
  x0 = x.model, V0 = v.model, tinitx = 0,
  C= cPDO.model4, c = treat_cols_9, D = d.model, d = obD)

ssNSE_DD1980ful4 <- MARSS(dat, 
                          model = model.list_DD1980ful4, 
                          method = "kem",
                          control = list(maxit = 1000))

# AICc comparison
ssNSE_DD1980ful1$AICc
ssNSE_DD1980ful2$AICc
ssNSE_DD1980ful3$AICc
ssNSE_DD1980ful4$AICc

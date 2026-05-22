# packages
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/DD/marssBACI_reprex.R")
options(max.print=2000)

# load data 
# esc data - from NSEout_WIDEstandardize
load(here("data", "clean", "NSEout_wpinks_scst.Rda"))

# grab year lists, # of observations, & pure count data
wpinks_scst.df <- wpinks_scst.df[-c(66)]
years <- names(wpinks_scst.df)
years <- years[-1]
years <- substring(years, first=13, last=16)
n <- nrow(wpinks_scst.df)

# gonna have to stack up salmon counts
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

# convert counts, observer ID, and pdo data to matrix
dat <- data.matrix(STACKwpinks_scst.df[2:ncol(STACKwpinks_scst.df)])

# Define the start and end columns for the treatment
start_col <- 26 ## approx 2010
end_col <- 32

# Initialize an empty list to store the matrices related to treatment
treatment_matrices <- list()

# Outer loop for rows 1 & 2 (11 to 32)
for (i in start_col:end_col) {
  # Inner loop for rows 3 & 4 (11 to 32)
  for (j in start_col:end_col) {
    
    # Create blank 4x32 matrix
    temp_matrix <- matrix(0, nrow = 4, ncol = 32)
    
    # Fill rows 1 & 2 up to index 'i'
    temp_matrix[1:2, 26:i] <- 1
    
    # Fill rows 3 & 4 up to index 'j'
    temp_matrix[3:4, 26:j] <- 1
    
    # Create unique name (e.g., "treat_R12_1_R34_1")
    n_i <- i - 25
    n_j <- j - 25
    matrix_name <- paste0("treat_E_", n_i, "_O_", n_j)
    
    # store in the list
    treatment_matrices[[matrix_name]] <- temp_matrix
  }
}

# setting up MARSS model inputs
b.model <- "identity"
a.model <- "zero"
x.model <- "unequal"
v.model <- "zero"
u.model <- "zero"

# Z (underlying states)
z.model <- matrix(0, nrow = 72, ncol = 4)
z.model[1:36, 1]  <- 1
z.model[6, 1:2]   <- c(0, 1)
z.model[37:72, 3] <- 1
z.model[42, 3:4]  <- c(0, 1)

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
c.model1 <- matrix(list(0), 4, 4)
c.model1[1,1] <- "treatE_region"
c.model1[2,2] <- "treatE_ir"
c.model1[3,3] <- "treatO_region"
c.model1[4,4] <- "treatO_ir"

# C (effect of covariates)
c.model2 <- matrix(list(0), 4, 4)
c.model2[1,1] <- "treatE"
c.model2[2,2] <- "treatE"
c.model2[3,3] <- "treatO"
c.model2[4,4] <- "treatO"

# C (effect of covariates)
c.model3 <- matrix(list(0), 4, 4)
c.model3[1,1] <- "treatE_region"
c.model3[2,2] <- "treatE_ir"
c.model3[3,3] <- "treatO"
c.model3[4,4] <- "treatO"

# C (effect of covariates)
c.model4 <- matrix(list(0), 4, 4)
c.model4[1,1] <- "treatE"
c.model4[2,2] <- "treatE"
c.model4[3,3] <- "treatO_region"
c.model4[4,4] <- "treatO_ir"

# put specifications into a list for easy looping
c_models <- list(c.model1, c.model2, c.model3, c.model4)

# empty list to store the results
marss_summary <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)

if(!file.exists(here("data", "clean", "marss_summary2010.rds"))){
# 4 Covariate Specifications
for (c_idx in 1:length(c_models)) {
  
  # 49 Treatment Matrices
  for (t_name in names(treatment_matrices)) {
    
    current_treat <- treatment_matrices[[t_name]]
    current_c_spec <- c_models[[c_idx]]
    
    # mod list
    model.list <- list(
      B = b.model, U = u.model, Q = q.model,
      Z = z.model, A = a.model, R = r.model,
      x0 = x.model, V0 = v.model, tinitx = 0,
      C = current_c_spec,       # Current C specification
      c = current_treat        # Current treatment matrix
    )
    
    # unique id, e.g., "C1_treat_E_1_O_1"
    model_id <- paste0("C", c_idx, "_", t_name)
    
    # run MARSS 
    fit <- MARSS(
      dat, 
      model = model.list, 
      method = "kem",
      control = list(maxit = 1000)
    )
    
    # extract AICc
    current_aicc <- fit$AICc
    
    # append results
    marss_summary <- rbind(marss_summary, data.frame(
      Model_Name = model_id,
      AICc = current_aicc
    ))
  }
}
saveRDS(marss_summary, file=here("data", "clean", "marss_summary2010.rds"))
}
marss_summary <- readRDS(file=here("data", "clean", "marss_summary2010.rds"))

# find best model
min_aicc <- min(marss_summary$AICc, na.rm = TRUE)
best_models <- subset(marss_summary, AICc <= (min_aicc + 2))
best_models$C_spec <- substr(best_models$Model_Name, 2, 2)
best_models$C_spec <- as.numeric(best_models$C_spec)
# packages
library(here)
library(MARSS)

# set loc
here::i_am("code/development/DD/b9.2.2-DD_marssBACI-revisedapproachLOOPS.R")
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

# Initialize an empty list to store the results
treatment_matrices <- list()

# Outer loop for rows 1 & 2 (11 to 32)
for (i in start_col:end_col) {
  # Inner loop for rows 3 & 4 (11 to 32)
  for (j in start_col:end_col) {
    
    # Create blank 4x32 matrix
    temp_matrix <- matrix(0, nrow = 4, ncol = 32)
    
    # Fill rows 1 & 2 up to index 'i'
    temp_matrix[1:2, 11:i] <- 1
    
    # Fill rows 3 & 4 up to index 'j'
    temp_matrix[3:4, 11:j] <- 1
    
    # Create unique name (e.g., "treat_R12_1_R34_1")
    n_i <- i - 10
    n_j <- j - 10
    matrix_name <- paste0("treat_R12_", n_i, "_R34_", n_j)
    
    # Combine with pdo and store in the list
    treatment_matrices[[matrix_name]] <- rbind(pdo, temp_matrix)
  }
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
c.model1 <- matrix(list(0), 4, 6)
c.model1[1,1] <- "pE"
c.model1[2,1] <- "pE"
c.model1[3,2] <- "pO"
c.model1[4,2] <- "pO"
c.model1[1,3] <- "treatE_region"
c.model1[2,4] <- "treatE_ir"
c.model1[3,5] <- "treatO_region"
c.model1[4,6] <- "treatO_ir"

# C (effect of covariates)
c.model2 <- matrix(list(0), 4, 6)
c.model2[1,1] <- "pE"
c.model2[2,1] <- "pE"
c.model2[3,2] <- "pO"
c.model2[4,2] <- "pO"
c.model2[1,3] <- "treatE"
c.model2[2,4] <- "treatE"
c.model2[3,5] <- "treatO"
c.model2[4,6] <- "treatO"

# C (effect of covariates)
c.model3 <- matrix(list(0), 4, 6)
c.model3[1,1] <- "pE"
c.model3[2,1] <- "pE"
c.model3[3,2] <- "pO"
c.model3[4,2] <- "pO"
c.model3[1,3] <- "treatE_region"
c.model3[2,4] <- "treatE_ir"
c.model3[3,5] <- "treatO"
c.model3[4,6] <- "treatO"

# C (effect of covariates)
c.model4 <- matrix(list(0), 4, 6)
c.model4[1,1] <- "pE"
c.model4[2,1] <- "pE"
c.model4[3,2] <- "pO"
c.model4[4,2] <- "pO"
c.model4[1,3] <- "treatE"
c.model4[2,4] <- "treatE"
c.model4[3,5] <- "treatO_region"
c.model4[4,6] <- "treatO_ir"

# Put the four specifications into a list for easy looping
c_models <- list(c.model1, c.model2, c.model3, c.model4)

# Initialize an empty list to store the results
marss_summary <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)

# Outer Loop: The 4 Covariate Specifications
for (c_idx in 1:length(c_models)) {
  
  # Inner Loop: The 484 Treatment Matrices
  for (t_name in names(treatment_matrices)) {
    
    current_treat <- treatment_matrices[[t_name]]
    current_c_spec <- c_models[[c_idx]]
    
    # Update the model list
    model.list <- list(
      B = b.model, U = u.model, Q = q.model,
      Z = z.model, A = a.model, R = r.model,
      x0 = x.model, V0 = v.model, tinitx = 0,
      C = current_c_spec,       # Current C specification
      c = current_treat,        # Current treatment matrix
      D = d.model, d = obD
    )
    
    # Create a unique name: e.g., "C1_treat_R12_1_R34_1"
    model_id <- paste0("C", c_idx, "_", t_name)
    
    # Run the MARSS model (using try() to prevent loop breaks on errors)
    fit <- try(MARSS(
      dat, 
      model = model.list, 
      method = "kem",
      control = list(maxit = 1000, silent = TRUE) # Added silent to keep console clean
    ), silent = TRUE)
    
    # Extract AICc
    current_aicc <- if (!inherits(fit, "try-error")) fit$AICc else NA
    
    # Append results
    marss_summary <- rbind(marss_summary, data.frame(
      Model_Name = model_id,
      AICc = current_aicc
    ))
  }
}

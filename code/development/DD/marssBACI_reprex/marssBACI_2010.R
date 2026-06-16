# packages
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/DD/marssBACI_reprex/marssBACI_2010.R")
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
obs <- data.matrix(STACKwobs.df[2:ncol(STACKwobs.df)])
pdo <- data.matrix(STACKwpdo.df)

# levels for observation data
levels <- sort(unique(as.vector(obs)))
result <- do.call(rbind, lapply(1:nrow(obs), 
                                function(i) t(sapply(levels, 
                                                     function(x) as.integer(obs[i,] == x)))))
obD <- result

# Define the start and end columns for the treatment
start_col <- 27 ## 2012/13 - first years with returns from expanded production
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
    temp_matrix[1:2, start_col:i] <- 1
    
    # Fill rows 3 & 4 up to index 'j'
    temp_matrix[3:4, start_col:j] <- 1
    
    # Create unique name (e.g., "treat_R12_1_R34_1")
    n_i <- i - (start_col - 1)
    n_j <- j - (start_col - 1)
    matrix_name <- paste0("treat_E_", n_i, "_O_", n_j)
    
    # store in the list
    treatment_matrices[[matrix_name]] <- rbind(pdo, temp_matrix)
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

# put specifications into a list for easy looping
c_models <- list(c.model1, c.model2, c.model3, c.model4)

## three looks at observation error
# R (diag and eq)
r.model <- "diagonal and equal"

# empty list to store the results
marsssummary_Rde <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)

if(!file.exists(here("data", "clean", "MARSSsummary_Rde2010.rds"))){
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
        c = current_treat,        # Current treatment matrix
        D = d.model, d = obD
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
      marsssummary_Rde <- rbind(marsssummary_Rde, data.frame(
        Model_Name = model_id,
        AICc = current_aicc
      ))
    }
  }
  saveRDS(marsssummary_Rde, file=here("data", "clean", "MARSSsummary_Rde2010.rds"))
}
marsssummary_Rde <- readRDS(file=here("data", "clean", "MARSSsummary_Rde2010.rds"))

# find best model
min_aicc <- min(marsssummary_Rde$AICc, na.rm = TRUE)
bestmodels_Rde <- subset(marsssummary_Rde, AICc <= (min_aicc + 2))
bestmodels_Rde$C_spec <- substr(bestmodels_Rde$Model_Name, 2, 2)
bestmodels_Rde$C_spec <- as.numeric(bestmodels_Rde$C_spec)

# R (equal var cov)
r.model <- "equalvarcov"

# empty list to store the results
marsssummary_Rvc <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)

if(!file.exists(here("data", "clean", "MARSSsummary_Rvc2010.rds"))){
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
        c = current_treat,        # Current treatment matrix
        D = d.model, d = obD
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
      marsssummary_Rvc <- rbind(marsssummary_Rvc, data.frame(
        Model_Name = model_id,
        AICc = current_aicc
      ))
    }
  }
  saveRDS(marsssummary_Rvc, file=here("data", "clean", "MARSSsummary_Rvc2010.rds"))
}
marsssummary_Rvc <- readRDS(file=here("data", "clean", "MARSSsummary_Rvc2010.rds"))

# find best model
min_aicc <- min(marsssummary_Rvc$AICc, na.rm = TRUE)
bestmodels_Rvc <- subset(marsssummary_Rvc, AICc <= (min_aicc + 2))
bestmodels_Rvc$C_spec <- substr(bestmodels_Rvc$Model_Name, 2, 2)
bestmodels_Rvc$C_spec <- as.numeric(bestmodels_Rvc$C_spec)


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

# empty list to store the results
marsssummary_Rsite <- data.frame(
  Model_Name = character(),
  AICc = numeric(),
  stringsAsFactors = FALSE
)

if(!file.exists(here("data", "clean", "marsssummary_Rsite2010.rds"))){
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
        c = current_treat,        # Current treatment matrix
        D = d.model, d = obD
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
      marsssummary_Rsite <- rbind(marsssummary_Rsite, data.frame(
        Model_Name = model_id,
        AICc = current_aicc
      ))
    }
  }
  saveRDS(marsssummary_Rsite, file=here("data", "clean", "MARSSsummary_Rsite2010.rds"))
}
marsssummary_Rsite <- readRDS(file=here("data", "clean", "MARSSsummary_Rsite2010.rds"))

# find best model
min_aicc <- min(marsssummary_Rsite$AICc, na.rm = TRUE)
bestmodels_Rsite <- subset(marsssummary_Rsite, AICc <= (min_aicc + 2))
bestmodels_Rsite$C_spec <- substr(bestmodels_Rsite$Model_Name, 2, 2)
bestmodels_Rsite$C_spec <- as.numeric(bestmodels_Rsite$C_spec)
# packages
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/DD/b9.1-DD_marssBACI-minimum example.R")
options(max.print=2000)

# load data 
# pdo data - from pdo_clean
load(here("data", "clean", "Wpdo.Rda"))

# drop pdo for years w/out complete data
Wpdo <- Wpdo[-c(65, 66)]


newnames <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8",
              "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", 
              "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24", 
              "t25", "t26", "t27", "t28", "t29", "t30", "t31", "t32")

# pdo
eveCol_wpdo <- Wpdo[, seq(ncol(Wpdo)) %% 2 == 1]
oddCol_wpdo <- Wpdo[, seq(ncol(Wpdo)) %% 2 != 1]
colnames(oddCol_wpdo) <- newnames
colnames(eveCol_wpdo) <- newnames
STACKwpdo.df <- rbind(eveCol_wpdo, oddCol_wpdo)
pdo <- data.matrix(STACKwpdo.df)

# matrix for treatment 
treat1980 <- matrix(0, nrow = 4, ncol = 32)
treat1980[, 11] <- 1

c1980PDO.data <- rbind(pdo, treat1980)

# C
cPDO.model <- matrix(0, 4, 6)
cPDO.model[1,1] <- -2.5
cPDO.model[2,1] <- -2.5
cPDO.model[3,2] <- -4
cPDO.model[4,2] <- -4
cPDO.model[1,3] <- 10
cPDO.model[2,4] <- 15
cPDO.model[3,5] <- 8
cPDO.model[4,6] <- 17

dim(cPDO.model)
dim(c1980PDO.data)
Cc_t <- cPDO.model%*%c1980PDO.data

# W - error terms
row1 <- rnorm(32, mean = 0, sd = 2)
row2 <- rnorm(32, mean = 0, sd = 2)
row3 <- rnorm(32, mean = 0, sd = 3)
row4 <- rnorm(32, mean = 0, sd = 3)

W <- rbind(row1, row2, row3, row4)

# B
B <- diag(4)

# X0
x0 <- matrix(0, nrow = 4, ncol = 1)
x0[1,1] <- 6
x0[2,1] <- 5
x0[3,1] <- 8
x0[4,1] <- 8.5

xt <- matrix(0, nrow = 4, ncol = 32)
xt[1,1] <- x0[1,1] + Cc_t[1,1] + W[1,1]
xt[2,1] <- x0[2,1] + Cc_t[2,1] + W[2,1]
xt[3,1] <- x0[3,1] + Cc_t[3,1] + W[3,1]
xt[4,1] <- x0[4,1] + Cc_t[4,1] + W[4,1]

for (i in 1:31){
  xt[1,(i+1)] <- xt[1,i] + Cc_t[1,i] + W[1,i]
  xt[2,(i+1)] <- xt[2,i] + Cc_t[2,i] + W[2,i]
  xt[3,(i+1)] <- xt[3,i] + Cc_t[3,i] + W[3,i]
  xt[4,(i+1)] <- xt[4,i] + Cc_t[4,i] + W[4,i]
}

my_matrix <- xt
rownames(my_matrix) <- paste("Series", 1:4)

# convert to long data frame
plot_data <- as.data.frame(my_matrix) %>%
  mutate(row_id = rownames(.)) %>% # Add row identifier
  pivot_longer(
    cols = -row_id, 
    names_to = "time_step", 
    values_to = "value"
  ) %>%
  mutate(time_step = as.numeric(gsub("V", "", time_step))) # Clean time index

# plot
ggplot(plot_data, aes(x = time_step, y = value, color = row_id, group = row_id)) +
  geom_line() +
  labs(title = "Time Series by Matrix Row", x = "Time", y = "Value", color = "Series") +
  theme_minimal()
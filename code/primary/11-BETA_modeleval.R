## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/primary/11-BETA_modeleval.R")
options(max.print=2000)

# load in ss model
ssbeta_DFGob <- readRDS(file=here("data", "clean", "ssbeta_DFGob.rds"))

# data comparison
states <- ssbeta_DFGob$states
ytT <- ssbeta_DFGob$call$data
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ_E.x"
names(df)[names(df) == "X.Y2"] <- "IR_E.x"
names(df)[names(df) == "X.Y3"] <- "SJ_O.x"
names(df)[names(df) == "X.Y4"] <- "IR_O.x"
names(df)[names(df) == "X"] <- "SJ_E.y"
names(df)[names(df) == "X.1"] <- "IR_E.y"
names(df)[names(df) == "X.2"] <- "SJ_O.y"
names(df)[names(df) == "X.3"] <- "IR_O.y"

df_E <- df[-c(3,4,7,8)]
df_O <- df[-c(1,2,5,6)]

df_E$index <- 1:nrow(df_E)
df_E <- df_E %>% 
  pivot_longer(
    cols = c("SJ_E.x", "IR_E.x", "SJ_E.y", "IR_E.y"),
    names_to = "measure",
    values_to = "value"
  )
dfSJ_E <- df_E %>% filter(!(measure == "IR_E.x" | measure == "IR_E.y"))
dfIR_E <- df_E %>% filter(!(measure == "SJ_E.x" | measure == "SJ_E.y"))

df_O$index <- 1:nrow(df_O)
df_O <- df_O %>% 
  pivot_longer(
    cols = c("SJ_O.x", "IR_O.x", "SJ_O.y", "IR_O.y"),
    names_to = "measure",
    values_to = "value"
  )
dfSJ_O <- df_O %>% filter(!(measure == "IR_O.x" | measure == "IR_O.y"))
dfIR_O <- df_O %>% filter(!(measure == "SJ_O.x" | measure == "SJ_O.y"))

ggplot(dfIR_E, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="Even year runs",
       y=NULL) +
  theme_classic()

ggplot(dfSJ_E, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='SJH Data v. Fitted States',
       subtitle="Even year runs",
       y=NULL) +
  theme_classic()

ggplot(dfIR_O, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="Odd year runs",
       y=NULL) +
  theme_classic()

ggplot(dfSJ_O, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='SJH Data v. Fitted States',
       subtitle="Odd year runs",
       y=NULL) +
  theme_classic()
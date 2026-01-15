## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)
library(tidyverse)

# set loc
here::i_am("code/development/Betas/04-modeleval.R")
options(max.print=2000)

# load in ss models
ssEbeta_AMPr <- readRDS(file=here("data", "clean", "ssEbeta_AMPr.rds"))
ssEbeta_DFGr <- readRDS(file=here("data", "clean", "ssEbeta_DFGr.rds"))
ssObeta_AMPr <- readRDS(file=here("data", "clean", "ssObeta_AMPr.rds"))
ssObeta_DFGr <- readRDS(file=here("data", "clean", "ssObeta_DFGr.rds"))

# data comparison
mod <- ssEbeta_AMPr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

df$index <- 1:nrow(df)
df <- df %>% 
  pivot_longer(
    cols = c("SJ.x", "IR.x", "SJ.y", "IR.y"),
    names_to = "measure",
    values_to = "value"
  )
dfSJ <- df %>% filter(!(measure == "IR.x" | measure == "IR.y"))
dfIR <- df %>% filter(!(measure == "SJ.x" | measure == "SJ.y"))

ggplot(dfIR, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="E_AMPr",
       y=NULL) +
  theme_classic()

ggplot(dfSJ, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='SJH Data v. Fitted States',
       subtitle="E_AMPr",
       y=NULL) +
  theme_classic()

statesE_AMPr <- df

mod <- ssEbeta_DFGr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

df$index <- 1:nrow(df)
df <- df %>% 
  pivot_longer(
    cols = c("SJ.x", "IR.x", "SJ.y", "IR.y"),
    names_to = "measure",
    values_to = "value"
  )
dfSJ <- df %>% filter(!(measure == "IR.x" | measure == "IR.y"))
dfIR <- df %>% filter(!(measure == "SJ.x" | measure == "SJ.y"))

ggplot(dfIR, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="E_DFGr",
       y=NULL) +
  theme_classic()

ggplot(dfSJ, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='SJH Data v. Fitted States',
       subtitle="E_DFGr",
       y=NULL) +
  theme_classic()

statesE_DFGr <- df

mod <- ssObeta_AMPr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

df$index <- 1:nrow(df)
df <- df %>% 
  pivot_longer(
    cols = c("SJ.x", "IR.x", "SJ.y", "IR.y"),
    names_to = "measure",
    values_to = "value"
  )
dfSJ <- df %>% filter(!(measure == "IR.x" | measure == "IR.y"))
dfIR <- df %>% filter(!(measure == "SJ.x" | measure == "SJ.y"))

ggplot(dfIR, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="O_AMPr",
       y=NULL) +
  theme_classic()

ggplot(dfSJ, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='SJH Data v. Fitted States',
       subtitle="O_AMPr",
       y=NULL) +
  theme_classic()

statesO_AMPr <- df

mod <- ssObeta_DFGr

states <- mod$states
ytT <- mod$ytT
d <- list(states, ytT)
df <- data.frame(do.call(rbind,d))
df <- data.frame(t(df))

names(df)[names(df) == "X.Y1"] <- "SJ.x"
names(df)[names(df) == "X.Y2"] <- "IR.x"
names(df)[names(df) == "X"] <- "SJ.y"
names(df)[names(df) == "X.1"] <- "IR.y"

df$index <- 1:nrow(df)
df <- df %>% 
  pivot_longer(
    cols = c("SJ.x", "IR.x", "SJ.y", "IR.y"),
    names_to = "measure",
    values_to = "value"
  )
dfSJ <- df %>% filter(!(measure == "IR.x" | measure == "IR.y"))
dfIR <- df %>% filter(!(measure == "SJ.x" | measure == "SJ.y"))

ggplot(dfIR, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='IR Data v. Fitted States',
       subtitle="O_DFGr",
       y=NULL) +
  theme_classic()

ggplot(dfSJ, aes(x = index, y = value, color = measure)) +
  geom_point() +
  labs(x = NULL,
       title='SJH Data v. Fitted States',
       subtitle="O_DFGr",
       y=NULL) +
  theme_classic()

statesO_DFGr <- df

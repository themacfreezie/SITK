## SET WORKING DIR & PACKAGES
library(gridExtra)
library(here)
library(reshape2)
library(tidyverse)

# load model objects
bootE_AMPr <- readRDS(here("data", "clean", "bootE_AMPr.rds"))
bootE_DFGr <- readRDS(here("data", "clean", "bootE_DFGr.rds"))
bootE_DFGe <- readRDS(here("data", "clean", "bootE_DFGe.rds"))
bootO_AMPr <- readRDS(here("data", "clean", "bootO_AMPr.rds"))
bootO_DFGr <- readRDS(here("data", "clean", "bootO_DFGr.rds"))
bootO_DFGe <- readRDS(here("data", "clean", "bootO_DFGe.rds"))

# modular code
mod <- bootE_AMPr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:11)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
points <- data.frame(Beta = names, mean = mean, sd = sd)

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_AMPr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

bplot_tight <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_AMPr",
       y=NULL) +
  scale_y_continuous(limits = c(-2, 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot_tight

ggsave(here("output", "figures", "E_AMPr_bplot.png"), plot=bplot, device="png", dpi=300)
ggsave(here("output", "figures", "E_AMPr_bplottight.png"), plot=bplot_tight, device="png", dpi=300)

# modular code
mod <- bootE_DFGr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:11)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
points <- data.frame(Beta = names, mean = mean, sd = sd)

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_DFGr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

bplot_tight <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_DFGr",
       y=NULL) +
  scale_y_continuous(limits = c(-2, 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot_tight

ggsave(here("output", "figures", "E_DFGr_bplot.png"), plot=bplot, device="png", dpi=300)
ggsave(here("output", "figures", "E_DFGr_bplottight.png"), plot=bplot_tight, device="png", dpi=300)

# modular code
mod <- bootE_DFGe

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:11)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
points <- data.frame(Beta = names, mean = mean, sd = sd)

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_DFGe",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

bplot_tight <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="E_DFGe",
       y=NULL) +
  scale_y_continuous(limits = c(-2, 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot_tight

ggsave(here("output", "figures", "E_DFGe_bplot.png"), plot=bplot, device="png", dpi=300)
ggsave(here("output", "figures", "E_DFGe_bplottight.png"), plot=bplot_tight, device="png", dpi=300)

# modular code
mod <- bootO_AMPr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:11)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
points <- data.frame(Beta = names, mean = mean, sd = sd)

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_AMPr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

bplot_tight <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_AMPr",
       y=NULL) +
  scale_y_continuous(limits = c(-2, 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot_tight

ggsave(here("output", "figures", "O_AMPr_bplot.png"), plot=bplot, device="png", dpi=300)
ggsave(here("output", "figures", "O_AMPr_bplottight.png"), plot=bplot_tight, device="png", dpi=300)

# modular code
mod <- bootO_DFGr

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:11)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
  # let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
points <- data.frame(Beta = names, mean = mean, sd = sd)

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_DFGr",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

bplot_tight <- ggplot(stack(df), aes(x=ind, y=values)) + 
    geom_boxplot() +
    labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_DFGr",
       y=NULL) +
    scale_y_continuous(limits = c(-2, 2)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_classic()
bplot_tight

ggsave(here("output", "figures", "O_DFGr_bplot.png"), plot=bplot, device="png", dpi=300)
ggsave(here("output", "figures", "O_DFGr_bplottight.png"), plot=bplot_tight, device="png", dpi=300)

# modular code
mod <- bootO_DFGe

# grab bootstrap parameter estimates for b
df <- mod$boot.params
df <- data.frame(t(df))
df <- df[, -c(1,2, 7:11)]

# rename columns
names(df)[names(df) == "B..1.1."] <- "Hatchery(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..1.2."] <- "Hatchery(t)<<River(t-1)"
names(df)[names(df) == "B..2.1."] <- "River(t)<<Hatchery(t-1)"
names(df)[names(df) == "B..2.2."] <- "River(t)<<River(t-1)"
# let's doulbe check with mark and make sure I'm assigning hese correctly

# grab mean and sd
names <- colnames(df)
mean <- sapply(df, mean)
sd <- sapply(df, sd)
points <- data.frame(Beta = names, mean = mean, sd = sd)

# plots
bplot <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_DFGe",
       y=NULL) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot

bplot_tight <- ggplot(stack(df), aes(x=ind, y=values)) + 
  geom_boxplot() +
  labs(x = NULL,
       title='IR/SJH interactions',
       subtitle="O_DFGe",
       y=NULL) +
  scale_y_continuous(limits = c(-2, 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()
bplot_tight

ggsave(here("output", "figures", "O_DFGe_bplot.png"), plot=bplot, device="png", dpi=300)
ggsave(here("output", "figures", "O_DFGe_bplottight.png"), plot=bplot_tight, device="png", dpi=300)

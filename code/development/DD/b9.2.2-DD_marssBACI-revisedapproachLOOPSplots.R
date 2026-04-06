# packages
library(grid)
library(here)
library(patchwork)
library(tidyverse)

# set loc
here::i_am("code/development/DD/b9.2.2-DD_marssBACI-revisedapproachLOOPSplots.R")
options(max.print=2000)

# load bootstrap dfs
boot.1980 <- readRDS(file=here("data", "clean", "DDrevised_bestfitBOOT1980.rds"))
boot.2010 <- readRDS(file=here("data", "clean", "DDrevised_bestfitBOOT2010.rds"))

# set em wide bitch
boot1980.df <- data.frame(t(boot.1980))
boot1980.df <- boot1980.df[-c(1:49, 52:57)]
boot1980_E <- boot1980.df[-c(2)]
boot1980_O <- boot1980.df[-c(1)]

boot2010.df <- data.frame(t(boot.2010))
boot2010.df <- boot2010.df[-c(1:49, 52:57)]
boot2010_E <- boot2010.df[-c(2)]
boot2010_O <- boot2010.df[-c(1)]

# rename columns
names(boot1980_E)[names(boot1980_E) == "U.treatE"] <- "Effect of treatment period"
names(boot1980_O)[names(boot1980_O) == "U.treatO"] <- "Effect of treatment period"
names(boot2010_E)[names(boot2010_E) == "U.treatE"] <- "Effect of treatment period"
names(boot2010_O)[names(boot2010_O) == "U.treatO"] <- "Effect of treatment period"

# set data real long - 1980 
wboot1980_E <- boot1980_E |>
  pivot_longer(
    cols = c(colnames(boot1980_E)),
    names_to = "effect",
    values_to = "value"
  )

wboot1980_O <- boot1980_O |>
  pivot_longer(
    cols = c(colnames(boot1980_O)),
    names_to = "effect",
    values_to = "value"
  )

wboot1980_E$run <- "Even-year"
wboot1980_O$run <- "Odd-year"

wboot1980.df <- bind_rows(wboot1980_E, wboot1980_O)

wboot1980.df$period <- "1980"

# set data real long - 2010 
wboot2010_E <- boot2010_E |>
  pivot_longer(
    cols = c(colnames(boot2010_E)),
    names_to = "effect",
    values_to = "value"
  )

wboot2010_O <- boot2010_O |>
  pivot_longer(
    cols = c(colnames(boot2010_O)),
    names_to = "effect",
    values_to = "value"
  )

wboot2010_E$run <- "Even-year"
wboot2010_O$run <- "Odd-year"

wboot2010.df <- bind_rows(wboot2010_E, wboot2010_O)

wboot2010.df$period <- "2010"

# plot - 1980
bplot1980 <- ggplot(wboot1980.df, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = "1980",
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_blank(),
        # axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) + 
  annotate("text", x = Inf, y = Inf, label = "1980", 
           hjust = 1, vjust = 1, size = 10) 
bplot1980

# plot - 2010
bplot2010 <- ggplot(wboot2010.df, aes(x = effect, y = value, fill = run)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Even-year" = "#0072B2", "Odd-year" = "#E69F00")) +
  labs(
    x = NULL,
    y = "2010",
    fill = "Run"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) + 
  annotate("text", x = Inf, y = Inf, label = "2010", 
           hjust = 1, vjust = 1, size = 10) 
bplot2010

# combine
bplot1980$labels$y <- bplot2010$labels$y <- " "
STKbplotDD <- (bplot1980/bplot2010) +
  plot_layout(widths = c(0.01, 1)) +
  plot_layout(guides = "collect")
STKbplotDD

ggsave(here("output", "figures", "STKbplotDD.png"), plot=STKbplotDD, device="png", dpi=300)

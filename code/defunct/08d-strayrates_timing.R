library(here)
library(readxl)
library(tidyverse)

here::i_am("code/defunct/08d-strayrates_timing.R")

# read in data
stray_weeks <- read_xlsx(here("data", "raw", "ADFG_strayrates.xlsx"), sheet= "timing", col_names = TRUE)

# labels for sites
stray_weeks$Site[stray_weeks$Site == "SJH"] <- "Sheldon Jackson"
stray_weeks$Site[stray_weeks$Site == "IR"] <- "Indian River"

strayrate_timing <- ggplot(stray_weeks, aes(StatWeek, fill = Site)) + 
  geom_histogram(alpha = 0.5) +
  labs(x = "",
       y='Number of Surveys Conducted',
       title='Week of occurance of stray rate surveys',
       subtitle='2011 - 2024') + 
  scale_x_continuous(expand = c(0, 0.1), 
                     breaks=c(31, 34, 37, 40),
                     labels=c("July", "August", "September", "October")) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
strayrate_timing

ggsave(here("output", "figures", "stray_timing.png"), plot=strayrate_timing, device="png", dpi=300)
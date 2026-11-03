library(ggplot2)
library(here)

here::i_am("code/defunct/08c-strayrates_compare.R")

# load data
load(here("data", "clean", "strays_clean.Rda"))

# create opposite indicators
strays_clean$ir_pctUnmark <- 1 - strays_clean$ir_pctMark
strays_clean$sj_pctUnmark <- 1 - strays_clean$sj_pctMark

stray_comp <- ggplot(data = strays_clean, aes(x = ir_pctMark, y = sj_pctUnmark, label=Year)) +
  geom_point() +
  geom_text(hjust = -0.2, vjust = 0) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_classic() +
  labs(x = "Percent of fish marked at Indian River", y = "Percent of fish unmarked at SJ Hatchery")
stray_comp
ggsave(here("output", "figures", "stray_comp.png"), plot=stray_comp, device="png", dpi=300, height= 6, width = 9.5)

stray_ir <- strays_clean[ , c(1:3,6)]
stray_sj <- strays_clean[ , c(1, 4, 5, 7)]

names(stray_ir)[names(stray_ir) == 'ir_total'] <- 'total'
names(stray_ir)[names(stray_ir) == 'ir_pctMark'] <- 'pctMark'
names(stray_ir)[names(stray_ir) == 'ir_pctUnmark'] <- 'pctUnmark'
stray_ir$pctHome <- stray_ir$pctUnmark*100
stray_ir$pctStray <- stray_ir$pctMark*100
stray_ir$Site <- "Indian River"

names(stray_sj)[names(stray_sj) == 'sj_total'] <- 'total'
names(stray_sj)[names(stray_sj) == 'sj_pctMark'] <- 'pctMark'
names(stray_sj)[names(stray_sj) == 'sj_pctUnmark'] <- 'pctUnmark'
stray_sj$pctHome <- stray_sj$pctMark*100
stray_sj$pctStray <- stray_sj$pctUnmark*100
stray_sj$Site <- "Sheldon Jackson"

stray_long <- rbind(stray_ir, stray_sj)

stray_time <- ggplot(stray_long, aes(x = Year, y = pctStray)) +
  geom_area(aes(fill = Site), position = position_dodge(), alpha = 0.3) +
  geom_point(aes(color = Site))+
  labs(x = "",
       y='Percent of stray fish recovered at site',
       title='Straying to non-natal sites by year',
       subtitle='2011 - 2021') + 
  scale_x_continuous(expand = c(0.01, 0.0)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
stray_time

ggsave(here("output", "figures", "stray_time.png"), plot=stray_time, device="png", dpi=300)

# pull in IR estimated states
load(here("data", "clean", "IR_stateE.Rda"))
load(here("data", "clean", "IR_stateO.Rda"))

# merge
mergeE <- merge(IR_stateE, strays_clean, by="Year")
mergeO <- merge(IR_stateO, strays_clean, by="Year")
  # so few observations..
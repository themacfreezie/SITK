library(here)
library(patchwork)
library(readxl)
library(tidyverse)

here::i_am("code/primary/d01-STRAY_examination.R")

# read in data
strays <- read_csv(here("data", "raw", "strayrates_raw.csv"), col_names = TRUE)

# dismissing NSE inner and SSE
straysIR <- strays %>% filter(Source=="ESCAPEMENT SURVEY")
straysSJ <- strays %>% filter(Source!="ESCAPEMENT SURVEY")

# detail cleaning, IR
straysIR <- straysIR %>% filter(`AStream / Watershed Code`=="113-41-10190")
# dismiss 1 weird streamID that is not IR
straysIR <- straysIR %>% filter(`Stat Week` < 42)
# dismiss real late samples (past mid-october)

# collapse around year, IR
straysIR <- straysIR %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# build total and pct, IR
straysIR$ir_total = straysIR$Marked + straysIR$`Not Marked`
straysIR <- straysIR %>% filter(ir_total>=30)
# dismiss sampling efforts w/ fewer than 30 observaitons
straysIR$ir_pctMark = straysIR$Marked/straysIR$ir_total
straysIR <- straysIR[-c(2:3)]

# detail cleaning, SJ
straysSJ <- straysSJ %>% filter(`Stat Week` < 42)
# dismiss real late samples (past mid-october)

# grab cost recovery only
straysCRw <- straysSJ %>% filter(Source=="COST RECOVERY")

# collapse around year, SJ
straysSJ <- straysSJ %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))

# collapse around year, cost recovery
straysCR <- straysCRw %>%
  group_by(Year) %>%
  summarize(Marked = sum(Marked), `Not Marked` = sum(`Not Marked`))
straysCRw <- straysCRw[-c(2:5, 7:14, 17:39)]

# build total and pct, SJ
straysSJ$sj_total = straysSJ$Marked + straysSJ$`Not Marked`
straysSJ <- straysSJ %>% filter(sj_total>=30)
# dismiss sampling efforts w/ fewer than 30 observaitons
straysSJ$sj_pctMark = straysSJ$Marked/straysSJ$sj_total
straysSJ <- straysSJ[-c(2:3)]

# build total and pct, cost recovery
straysCR$CR_total = straysCR$Marked + straysCR$`Not Marked`
straysCR <- straysCR %>% filter(CR_total>=30)
# dismiss sampling efforts w/ fewer than 30 observaitons
straysCR$CR_pctMark = straysCR$Marked/straysCR$CR_total

# build total and pct, cost recovery
straysCRw$CRw_total = straysCRw$Marked + straysCRw$`Not Marked`
straysCRw <- straysCRw %>% filter(CRw_total>=30)
# dismiss sampling efforts w/ fewer than 30 observaitons
straysCRw$CRw_pctMark = straysCRw$Marked/straysCRw$CRw_total

strays_clean <- merge(straysIR, straysSJ, by="Year")
save(strays_clean, file=here("data", "clean", "strays_clean.Rda"))

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

stray_year <- ggplot(stray_long, aes(x = Year, y = pctStray)) +
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
stray_year

# read in data
stray_weeks <- read_xlsx(here("data", "raw", "ADFG_strayrates.xlsx"), sheet= "timing", col_names = TRUE)

# labels for sites
stray_weeks$Site[stray_weeks$Site == "SJH"] <- "Sheldon Jackson"
stray_weeks$Site[stray_weeks$Site == "IR"] <- "Indian River"

stray_week <- ggplot(stray_weeks, aes(StatWeek, fill = Site)) + 
  geom_histogram(alpha = 0.5
                 , show.legend = FALSE
                 ) +
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
stray_week

# stitch figures together
combined_stray_plot <- stray_year / stray_week + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")
combined_stray_plot

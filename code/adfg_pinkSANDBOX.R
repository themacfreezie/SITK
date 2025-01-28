## SET WORKING DIR & PACKAGES

# import packages
library(car)
library(cowplot)
library(dplyr)
library(gghighlight)
library(ggplot2)
library(gplots)
library(lmtest)
library(plm)
library(readxl)
library(tidyverse)
library(tseries)

# create working dir
WD1 <- "C:/Users/thema/OneDrive/Documents/dissertation/ch1/data"
setwd(WD1)

options(max.print=10000)


## DATA FILTERING

# pull in data
pinks.df <- read_excel("adfg_pink.xlsx", col_names = TRUE)

# data transform - create study year
pinks.df <- transform(pinks.df, studyYEAR = (YEAR - 1959))

# data transform - split into odd/even runs
pinksE.df <- pinks.df %>% filter(YEAR %% 2 == 0)
pinksO.df <- pinks.df %>% filter(YEAR %% 2 != 0)

# clean up studyYEAR for new datasets
pinksE.df$studyYEAR <- ((pinksE.df$studyYEAR + 1)/2)
pinksO.df$studyYEAR <- (pinksO.df$studyYEAR/2)

# data transform - log transform peak count
pinksE.df$lnPEAK_COUNT <- log(pinksE.df$PEAK_COUNT)
pinksO.df$lnPEAK_COUNT <- log(pinksO.df$PEAK_COUNT)

# add run variable for each even/odd dataset
pinksE.df <- pinksE.df %>%
  add_column(RUN = "EVEN")
pinksO.df <- pinksO.df %>%
  add_column(RUN = "ODD")

# drop SSE - northern inner and outer
NpinksE.df <- pinksE.df %>% filter(SUB_REGION!="SSE")
NpinksO.df <- pinksO.df %>% filter(SUB_REGION!="SSE")

# drop outer - northern inner only
inNpinksE.df <- NpinksE.df %>% filter(SUB_REGION!="NSE Outside")
inNpinksO.df <- NpinksO.df %>% filter(SUB_REGION!="NSE Outside")

# drop inner - northern outer only
ouNpinksE.df <- NpinksE.df %>% filter(SUB_REGION!="NSE Inside")
ouNpinksO.df <- NpinksO.df %>% filter(SUB_REGION!="NSE Inside")
  # outside is the smallest region by a lot


## SUMMARY STATS

# summary by group - outer N, even & odd
tapply(ouNpinksE.df$PEAK_COUNT, ouNpinksE.df$STREAM, summary)
tapply(ouNpinksO.df$PEAK_COUNT, ouNpinksO.df$STREAM, summary)

# standard deviation by group - outer N, even & odd
tapply(ouNpinksE.df$PEAK_COUNT, ouNpinksE.df$STREAM, sd)
tapply(ouNpinksO.df$PEAK_COUNT, ouNpinksO.df$STREAM, sd)


## LINE CHARTS

# add numeric id by stream
# ouNpinksE.df <- ouNpinksE.df |>
#   dplyr::group_by(STREAM) |>
#   dplyr::mutate(ID = dplyr::cur_group_id()) |>
#   ungroup()
# 
# ouNpinksO.df <- ouNpinksO.df |>
#   dplyr::group_by(STREAM) |>
#   dplyr::mutate(ID = dplyr::cur_group_id()) |>
#   ungroup()

# line chart by stream - outer N, even & odd years AND both
# # even
# ouNstreams <- c(unique(ouNpinksE.df$STREAM))
# for(i in ouNstreams){
#   plot_data.df <- subset(ouNpinksE.df, STREAM == i)
#   print(ggplot(plot_data.df, aes(x=YEAR, y=PEAK_COUNT)) +
#     ggtitle("Even Year - ", i) +
#     xlab("Year") + ylab("Peak Count") +
#     geom_line())
# }
# 
# # odd
# for(i in ouNstreams){
#   plot_data.df <- subset(ouNpinksO.df, STREAM == i)
#   print(ggplot(plot_data.df, aes(x=YEAR, y=PEAK_COUNT)) +
#           ggtitle("Odd Year - ", i) +
#           xlab("Year") + ylab("Peak Count") +
#           geom_line())
# }

# both
ouNstreams <- c(unique(ouNpinksE.df$STREAM))
for(i in ouNstreams){
  plot_dataE.df <- subset(ouNpinksE.df, STREAM == i)
  plot_dataO.df <- subset(ouNpinksO.df, STREAM == i)
  plot_data.df <- rbind(plot_dataE.df, plot_dataO.df)
  print(ggplot(plot_data.df, aes(x=YEAR, y=PEAK_COUNT, group=RUN, color=RUN)) +
          ggtitle(i) +
          xlab("Year") + ylab("Peak Count") +
          geom_line())
}

# All Streams grouped by runs
print(ggplot(ouNpinksE.df, aes(x=YEAR, y=PEAK_COUNT, group=STREAM, color=STREAM)) +
        ggtitle("Even Year Runs") +
        xlab("Year") + ylab("Peak Count") +
        geom_line())

print(ggplot(ouNpinksO.df, aes(x=YEAR, y=PEAK_COUNT, group=STREAM, color=STREAM)) +
        ggtitle("Odd Year Runs") +
        xlab("Year") + ylab("Peak Count") +
        geom_line())

# Examples of highlighted streams - needs to be applied to Indian River
print(ggplot(ouNpinksO.df, aes(x=YEAR, y=PEAK_COUNT, group=STREAM)) +
        ggtitle("Odd Year Runs") +
        xlab("Year") + ylab("Peak Count") +
        geom_line() +
        gghighlight(STREAM == "Lisianski River"))

print(ggplot(ouNpinksO.df, aes(x=YEAR, y=PEAK_COUNT, group=STREAM)) +
        ggtitle("Odd Year Runs") +
        xlab("Year") + ylab("Peak Count") +
        geom_line() +
        gghighlight(STREAM == "Marine Cove"))

# # both - all streams\
# districts <- c(unique(pinksE.df$District))
# for(j in districts){
#   dist_pinksE.df <- subset(pinksE.df, District == j)
#   dist_pinksO.df <- subset(pinksO.df, District == j)
#   streams <- c(unique(dist_pinksE.df$STREAM_NO))
#   list_plots <- vector('list', length(streams))
#   for(i in seq_along(list_plots)){
#     dist_plot_dataE.df <- subset(dist_pinksE.df, STREAM_NO == i)
#     dist_plot_dataO.df <- subset(dist_pinksO.df, STREAM_NO == i)
#     plot_data.df <- rbind(dist_plot_dataE.df, dist_plot_dataO.df)
#     name <- c(unique(plot_data.df$STREAM))
#     num <- c(unique(plot_data.df$STREAM_NO))
#     list_plots[[i]] <- ggplot(plot_data.df, aes(x=YEAR, y=PEAK_COUNT, group=RUN, color=RUN)) +
#             ggtitle(name, num) +
#             xlab("Year") + ylab("Peak Count") +
#             geom_line()
#     print(list_plots[[i]])
#   }
# grid_j <- plot_grid(list_plots)
# print(grid_j)
# }


## REGRESSION MODELING

# model: count ~ years (w/ stream FE) - outer N, even years
summary(model.E_y <- lm(PEAK_COUNT ~ studyYEAR, data = ouNpinksE.df))
summary(model.E_yFEs <- lm(PEAK_COUNT ~ studyYEAR + factor(STREAM), data = ouNpinksE.df))
  # depicts general trend over time

# model: count ~ years (w/ stream FE) - outer N, odd years
summary(model.O_y <- lm(PEAK_COUNT ~ studyYEAR, data = ouNpinksO.df))
summary(model.O_yFEs <- lm(PEAK_COUNT ~ studyYEAR + factor(STREAM), data = ouNpinksO.df))
  # depicts general trend over time


# model: count ~ (w/ stream & year FE) - outer N, even years
summary(model.E_FEsy <- lm(PEAK_COUNT ~ factor(STREAM) + factor(YEAR), data = ouNpinksE.df))
  # controls for each individual year, reduces variance around stream FE

# model: count ~ (w/ stream & year FE) - outer N, odd years
summary(model.O_FEsy <- lm(PEAK_COUNT ~ factor(STREAM) + factor(YEAR), data = ouNpinksO.df))
  # controls for each individual year, reduces variance around stream FE


# model: count ~ (w/ stream & year & subregion FE) - all, even years
summary(model.E_FEsry <- lm(PEAK_COUNT ~ factor(STREAM) + factor(SUB_REGION) + factor(YEAR), data = pinksE.df))
  # controls for each individual year and subregion, reduces variance around stream FE

# model: count ~ (w/ stream & year & subregion FE) - all, odd years
summary(model.O_FEsry <- lm(PEAK_COUNT ~ factor(STREAM) + factor(SUB_REGION) + factor(YEAR), data = pinksO.df))
  # controls for each individual year and subregion, reduces variance around stream FE
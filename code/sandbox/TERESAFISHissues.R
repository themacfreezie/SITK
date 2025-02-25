## SET WORKING DIR & PACKAGES

# import packages
# library(car)
# library(cowplot)
# library(dplyr)
# library(gghighlight)
# library(ggplot2)
# library(gplots)
# library(lmtest)
# library(plm)
library(readxl)
# library(tidyverse)
# library(tseries)
library(writexl)

# create working dir
WD1 <- "C:/Users/thema/OneDrive/Documents/dissertation/ch1/data"
setwd(WD1)

options(max.print=10000)


## DATA FILTERING

# pull in data
pA.df <- read_excel("adfg_pink.xlsx", col_names = TRUE)
pB.df <- read_excel("Pink Salmon Index Counts_from OceanAK.xlsx", col_names = TRUE)

# rename variables in pA
names(pA.df)[names(pA.df) == "District"] <- "A_DISTRICT"
names(pA.df)[names(pA.df) == "STREAM"] <- "A_STREAM"
names(pA.df)[names(pA.df) == "STOCK_CODE"] <- "A_STOCKCODE"
names(pA.df)[names(pA.df) == "STOCK"] <- "A_STOCK"
names(pA.df)[names(pA.df) == "MANAGEMENT_AREA"] <- "A_MGMTAREA"
names(pA.df)[names(pA.df) == "SUB_REGION"] <- "A_SUBREGION"
names(pA.df)[names(pA.df) == "USAGE_CODE"] <- "A_USAGE"
names(pA.df)[names(pA.df) == "PEAK_COUNT"] <- "A_COUNT"
pA.df$A_INDICATOR <- 1

# rename variables in pB
names(pB.df)[names(pB.df) == "Year"] <- "YEAR"
names(pB.df)[names(pB.df) == "Stream Number"] <- "STREAM_NO"

names(pB.df)[names(pB.df) == "Survey Date"] <- "B_DATE"
names(pB.df)[names(pB.df) == "Stock"] <- "B_STOCK"
names(pB.df)[names(pB.df) == "District"] <- "B_DISTRICT"
names(pB.df)[names(pB.df) == "Management Area"] <- "B_MGMTAREA"
names(pB.df)[names(pB.df) == "Sub Region"] <- "B_SUBREGION"
names(pB.df)[names(pB.df) == "Area Surveyed"] <- "B_SURVEYAREA"
names(pB.df)[names(pB.df) == "Usage"] <- "B_USAGE"
names(pB.df)[names(pB.df) == "Observer"] <- "B_OBSERVER"
names(pB.df)[names(pB.df) == "Counts"] <- "B_COUNT"
pB.df$B_INDICATOR <- 2

# merge
merge.df <- merge(pA.df, pB.df, all = TRUE)
merge.df$A_INDICATOR[is.na(merge.df$A_INDICATOR)] <- 0
merge.df$B_INDICATOR[is.na(merge.df$B_INDICATOR)] <- 0
merge.df$M_INDICATOR <- (merge.df$A_INDICATOR + merge.df$B_INDICATOR)

# what merged clean and what didn't?
merge3.df <- subset(merge.df, M_INDICATOR == 3)
merge2.df <- subset(merge.df, M_INDICATOR == 2)
merge1.df <- subset(merge.df, M_INDICATOR == 1)
merge3.df$difference <- (merge3.df$A_COUNT - merge3.df$B_COUNT)
mergediff.df <- subset(merge3.df, difference != 0)
mergegood.df <- subset(merge3.df, difference == 0)

mergeissues.df <- rbind(merge2.df, merge1.df)
mergeissues.df$difference <- NA
mergeissues.df <- rbind(mergeissues.df, mergediff.df)

write_xlsx(mergeissues.df, "TERESAFISHissues.xlsx")

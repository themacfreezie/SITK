library(here)
library(readr)
library(tidyverse)

here::i_am("code/primary/04-pdo_clean.R")

# read in data from table
if(!file.exists(here("data", "clean", "pdo_raw.Rda"))){
  pdo <- read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
                  sep = "",
                  header = TRUE,
                  skip = 1)
  save(pdo, file=here("data", "clean", "pdo_raw.Rda"))
} 
  
if(file.exists(here("data", "clean", "pdo_raw.Rda"))) {
  load(here("data", "clean", "pdo_raw.Rda"))
}

# annual average - is this smart?
annual <- rowMeans(pdo[ ,2:13])
annual

pdo$annual <- annual

# drop month vars and years before 1960
pdo <- pdo[-c(2:13)]

pdo <- pdo %>% filter(Year >= 1959)
pdo <- pdo %>% filter(Year < 2025)

# create lag (year pinks are at sea)
pdo$returnYear <- pdo$Year + 1

# data transform - split into odd/even runs
pdoE <- pdo %>% filter(returnYear %% 2 == 0)
pdoO <- pdo %>% filter(returnYear %% 2 != 0)

save(pdo, file=here("data", "clean", "pdo.Rda"))
save(pdoE, file=here("data", "clean", "pdoE.Rda"))
save(pdoO, file=here("data", "clean", "pdoO.Rda"))

# setting wide
yrs <- pdo$returnYear
idx <- pdo$annual

yrsE <- pdoE$returnYear
idxE <- pdoE$annual

yrsO <- pdoO$returnYear
idxO <- pdoO$annual

Wpdo <- as.data.frame(matrix(idx, nrow = 1, byrow = TRUE))
names(Wpdo) <- yrs

WpdoE <- as.data.frame(matrix(idxE, nrow = 1, byrow = TRUE))
names(WpdoE) <- yrsE

WpdoO <- as.data.frame(matrix(idxO, nrow = 1, byrow = TRUE))
names(WpdoO) <- yrsO

save(Wpdo, file=here("data", "clean", "Wpdo.Rda"))
save(WpdoE, file=here("data", "clean", "WpdoE.Rda"))
save(WpdoO, file=here("data", "clean", "WpdoO.Rda"))

pdoplot <- ggplot() +
  geom_line(data = pdo, aes(y=annual, x=Year, color=annual))
pdoplot

library(here)

here::i_am("code/precip_clean.R")

col_classes = c(rep("numeric", 14))
precip <- read.table(here("data", "NOWdata_precipitation_sitkaairport.txt"), sep = "", header = TRUE, nrows = 82)

save(precip, file=here("data", "precip_sitka.Rda"))

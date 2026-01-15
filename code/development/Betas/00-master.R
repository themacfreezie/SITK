library(here)

# set loc
here::i_am("code/development/Betas/00-master.R")

# betas modeling
source(here("code", "development", "Betas", "01-data_clean.R"))
source(here("code", "development", "Betas", "02-WIDEstandardize.R"))
source(here("code", "development", "Betas", "03-modelbuild.R"))
source(here("code", "development", "Betas", "04-modeleval.R"))
source(here("code", "development", "Betas", "05-bootstrap.R"))
source(here("code", "development", "Betas", "06-boot_output.R"))

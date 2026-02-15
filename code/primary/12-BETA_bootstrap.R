## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)

# set loc
here::i_am("code/primary/12-BETA_bootstrap.R")

# load in ss model
ssbeta_DFGob <- readRDS(file=here("data", "clean", "ssbeta_DFGob.rds"))

# # bootstrap model
if(!file.exists(here("data", "clean", "boot_DFGob.rds"))){
  bootO_DFGob <- MARSSboot(ssbeta_DFGob
                           , nboot=10000
                           , output="parameters"
                           , sim = "parametric"
                           , param.gen="hessian"
  )
  saveRDS(bootO_DFGob, file=here("data", "clean", "boot_DFGob.rds"))
}
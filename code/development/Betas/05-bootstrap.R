## SET WORKING DIR & PACKAGES
library(here)
library(MARSS)

# load in ss models
ssEbeta_AMPr <- readRDS(file=here("data", "clean", "ssEbeta_AMPr.rds"))
ssEbeta_DFGr <- readRDS(file=here("data", "clean", "ssEbeta_DFGr.rds"))
ssObeta_AMPr <- readRDS(file=here("data", "clean", "ssObeta_AMPr.rds"))
ssObeta_DFGr <- readRDS(file=here("data", "clean", "ssObeta_DFGr.rds"))

# bootstrap estimates
if(!file.exists(here("data", "clean", "bootE_AMPr.rds"))){
  bootE_AMPr <- MARSSboot(ssEbeta_AMPr, nboot=10000, output="parameters", sim = "parametric", param.gen="hessian")
  saveRDS(bootE_AMPr, file=here("data", "clean", "bootE_AMPr.rds"))
}
if(!file.exists(here("data", "clean", "bootE_DFGr.rds"))){
  bootE_DFGr <- MARSSboot(ssEbeta_DFGr, nboot=10000, output="parameters", sim = "parametric", param.gen="hessian")
  saveRDS(bootE_DFGr, file=here("data", "clean", "bootE_DFGr.rds"))
}
if(!file.exists(here("data", "clean", "bootO_AMPr.rds"))){
  bootO_AMPr <- MARSSboot(ssObeta_AMPr, nboot=10000, output="parameters", sim = "parametric", param.gen="hessian")
  saveRDS(bootO_AMPr, file=here("data", "clean", "bootO_AMPr.rds"))
}
if(!file.exists(here("data", "clean", "bootO_DFGr.rds"))){
  bootO_DFGr <- MARSSboot(ssObeta_DFGr, nboot=10000, output="parameters", sim = "parametric", param.gen="hessian")
  saveRDS(bootO_DFGr, file=here("data", "clean", "bootO_DFGr.rds"))
}

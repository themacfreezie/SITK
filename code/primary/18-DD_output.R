## SET WORKING DIR & PACKAGES
library(here)
library(modelsummary)
library(webshot2)

# set loc
here::i_am("code/primary/18-DD_output.R")
options(max.print=2000)

# load in ss models
DDmodE_obsv1980 <- readRDS(file=here("data", "clean", "DDmodE_obsv1980.rds"))
DDmodO_both1980 <- readRDS(file=here("data", "clean", "DDmodO_both1980.rds"))
DDmodE_both2010 <- readRDS(file=here("data", "clean", "DDmodE_both2010.rds"))
DDmodO_obsv2010 <- readRDS(file=here("data", "clean", "DDmodO_obsv2010.rds"))

summary(DDmodE_obsv1980) 
summary(DDmodE_both2010)
summary(DDmodO_both1980) 
summary(DDmodO_obsv2010)

# model list
models <- list("Even, 1980" = DDmodE_obsv1980, 
               "Odd, 1980" = DDmodO_both1980,
               "Even, 2010" = DDmodE_both2010,
               "Odd, 2010" = DDmodO_obsv2010)

# build results tabls
modelsummary(models,
             coef_omit = c(1, 4:13, 15),
             coef_rename = c("dPost" = "Time Effect", 
                             "dIR" = "Indian River Effect", 
                             "dPost:dIR" = "Treatment Effect"),
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik|RMSE|F',
             title = "Indian River pink salmon difference-in-difference models",
             output = here("output", "figures", "DD_table.png")
             )

# # build results tabls
# modelsummary(models,
#              coef_omit = c(1, 4:13, 15),
#              coef_rename = c("dPost" = "Treatment Effect", 
#                              "dIR" = "Indian River Effect", 
#                              "dPost:dIR" = "Interaction Effect"),
#              stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
#              gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik|RMSE|F',
#              title = "Indian River pink salmon difference-in-difference models",
#              output = "latex"
# )

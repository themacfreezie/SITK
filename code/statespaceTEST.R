# load libraries
library(readxl)
library(rjags)

# create working dir and output folder
WD1 <- "C:/Users/thema/OneDrive/Documents"
setwd(WD1)

options(max.print=10000)

# pull in data
gflu <- read_excel("flu.xlsx", col_names = TRUE)
time <- as.Date(gflu$Date)
y <- gflu$Florida
# plot time series
plot(time, y, type = 'l')

# jags code - building components
random_walk <- "
model{
  
  #### Observation Model
  for (t in 1:n){
    y[t] ~ dnorm(x[t], tau_obs)
  }
  
  #### Process Model
  for (t in 2:n){
    x[t] ~ dnorm(x[t-1], tau_proc)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic, tau_ic)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_proc ~ dgamma(a_proc, r_proc)
}
"

data <- list(
  y = y,
  n = length(y),
  x_ic = 1000,
  tau_ic = 1,
  a_obs = 1,
  r_obs = 1,
  a_proc = 1,
  r_proc = 1
)

# to test prediction ability
# data$y[(length(y)-51):length(y)] = NA

init <- list(list(tau_proc = 1/var(diff(y)), tau_obs = 5/var(y)))

# jags code - building jags model
j_model <- jags.model(file = textConnection(random_walk),
                      data = data,
                      inits = init,
                      n.chains = 1)

# 'burn-in' step
jags_out <- coda.samples(model = j_model,
                         variable.names = c("tau_proc", "tau_obs"),
                         n.iter = 10000)
plot(jags_out)

# derive x values (latent variables)
jags_out <- coda.samples(model = j_model,
                         variable.names = c("x", 
                                            "tau_proc", 
                                            "tau_obs"),
                         n.iter = 10000)

  # retrieve x values
output <- as.matrix(jags_out)
xs = output[,3:ncol(output)]

  # point estimates = averages of these 10K samples, uncertainty = variance among them
predictions <- colMeans(xs)
plot(time, predictions, type = 'l')
points(time, y)

pred_interval <- apply(xs, 2, quantile, c(0.025, 0.975))
lines(time, pred_interval[1,], lty = "dashed", col = "blue")
lines(time, pred_interval[2,], lty = "dashed", col = "blue")

# check estimated variance
hist(1/sqrt(output[,1])) #observation error variance
hist(1/sqrt(output[,2])) #process error variance
plot(1/sqrt(output[,1]), 1/sqrt(output[,2]))

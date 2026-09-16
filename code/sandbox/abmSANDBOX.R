# Pure AI example here..
# libraries
library(ggplot2)

# INFECTIONS!! ------------------------------------------------------------
## Set up
# set.seed(42)            # For reproducible results
num_agents <- 1000       # Total population size
prob_infection <- 0.05   # Probability of transmission during a contact

# Create the initial population data frame
agents <- data.frame(
  id = 1:num_agents,
  status = "Susceptible",
  stringsAsFactors = FALSE
)

# Patient Zero: Infect the very first agent
agents$status[1] <- "Infected"

## Defining interaction rules
# Function to run a single time step of the simulation
run_time_step <- function(population, transmission_rate) {
  # Find the row indices of all currently infected agents
  infected_indices <- which(population$status == "Infected")
  
  # If no one is infected, the simulation stops changing
  if (length(infected_indices) == 0) return(population)
  
  # Loop through each infected agent to simulate interactions
  for (i in infected_indices) {
    # Randomly select another agent from the population
    target_id <- sample(population$id, 1)
    
    # Check if the encountered agent is Susceptible
    if (population$status[target_id] == "Susceptible") {
      # Roll a random number to see if transmission occurs
      if (runif(1) < transmission_rate) {
        population$status[target_id] <- "Infected"
      }
    }
  }
  
  return(population)
}

## Run model across time
total_steps <- 365
history <- numeric(total_steps) # Array to record infected counts

# Main simulation loop
for (step in 1:total_steps) {
  # Update the population states for this time step
  agents <- run_time_step(agents, prob_infection)
  
  # Record the total number of infected agents at this moment
  history[step] <- sum(agents$status == "Infected")
}

# View the final epidemic trajectory
print(history)

# Quick plot over time using base R
plot(history, type = "l", col = "red", lwd = 2,
     xlab = "Time Steps (Days)", ylab = "Number of Infected Agents",
     main = "Epidemic Trajectory Over Time")

# infections with recovery -----------------------------------------------------
## Set up
num_agents <- 1000       # Total population size
prob_infection <- 0.25   # Probability of transmission during a contact
recovery_time <- 10      # Days an individual stays infected
total_steps <- 365

# Create the initial population data frame
agents <- data.frame(
  id = 1:num_agents,
  status = "Susceptible",
  infected_day = NA,          # Track the exact step an agent got infected
  stringsAsFactors = FALSE
)

# Patient Zero
agents$status[1] <- "Infected"
agents$infected_day[1] <- 1   # Infected at time step 1

## Defining interaction rules
run_time_step <- function(population, transmission_rate, current_step) {
  infected_indices <- which(population$status == "Infected")
  if (length(infected_indices) == 0) return(population)
  
  for (i in infected_indices) {
    target_id <- sample(population$id, 1)
    
    if (population$status[target_id] == "Susceptible") {
      if (runif(1) < transmission_rate) {
        population$status[target_id] <- "Infected"
        population$infected_day[target_id] <- current_step # Record infection time
      }
    }
  }
  return(population)
}

## Run model across time
history <- data.frame(
  Day = 1:total_steps,
  Susceptible = numeric(total_steps),
  Infected = numeric(total_steps),
  Recovered = numeric(total_steps)
)

for (step in 1:total_steps) {
  # 1. Handle Recoveries: Find agents infected long enough to recover
  recovery_threshold <- step - recovery_time
  recover_indices <- which(agents$status == "Infected" & agents$infected_day <= recovery_threshold)
  
  if (length(recover_indices) > 0) {
    agents$status[recover_indices] <- "Recovered"
  }
  
  # 2. Run Transmission: Pass the current step to the function
  agents <- run_time_step(agents, prob_infection, step)
  
  # 3. Record History: Count currently infected agents
  history$Susceptible[step] <- sum(agents$status == "Susceptible")
  history$Infected[step]    <- sum(agents$status == "Infected")
  history$Recovered[step]   <- sum(agents$status == "Recovered")
}

print(history)

## SIR plot
# Plot the first line (Susceptible)
plot(history$Day, history$Susceptible, type = "l", col = "blue", lwd = 2,
     ylim = c(0, num_agents), xlab = "Days", ylab = "Agent Count",
     main = "Agent-Based SIR Model Simulation")

# Add the other two lines
lines(history$Day, history$Infected, col = "red", lwd = 2)
lines(history$Day, history$Recovered, col = "green", lwd = 2)

# Add a helpful legend
legend("right", legend = c("Susceptible", "Infected", "Recovered"),
       col = c("blue", "red", "green"), lwd = 2)


# Infections w/ mortality, recovery, possible reinfection -----------------
## Set up
num_agents <- 1000 
prob_infection <- 0.3 
recovery_time <- 10  
total_steps <- 365
prob_death <- 0.01           # 0.5% daily chance of dying while infected
immunity_multiplier <- 0.2   # Recovered agents have 80% protection (only 20% risk)

# Create the initial population data frame
agents <- data.frame(
  id = 1:num_agents,
  status = "Susceptible",
  infected_day = NA,          
  stringsAsFactors = FALSE
)

# Patient Zero
agents$status[1] <- "Infected"
agents$infected_day[1] <- 1  

## Defining interaction rules
run_time_step <- function(population, base_transmission, multiplier, current_step) {
  # Only living, infected agents can transmit disease
  infected_indices <- which(population$status == "Infected")
  if (length(infected_indices) == 0) return(population)
  
  for (i in infected_indices) {
    target_id <- sample(population$id, 1)
    target_status <- population$status[target_id]
    
    # Calculate transmission probability based on target's immune history
    if (target_status == "Susceptible") {
      transmission_rate <- base_transmission
    } else if (target_status == "Recovered") {
      transmission_rate <- base_transmission * multiplier
    } else {
      next # Skip if the target is already infected or deceased
    }
    
    # Roll for transmission
    if (runif(1) < transmission_rate) {
      population$status[target_id] <- "Infected"
      population$infected_day[target_id] <- current_step 
    }
  }
  return(population)
}

## Run model across time
history <- data.frame(
  Day = 1:total_steps,
  Susceptible = numeric(total_steps),
  Infected = numeric(total_steps),
  Recovered = numeric(total_steps),
  Deceased = numeric(total_steps)
)

for (step in 1:total_steps) {
  # --- Phase 1: Handle Daily Mortality ---
  infected_indices <- which(agents$status == "Infected")
  if (length(infected_indices) > 0) {
    # Each infected agent rolls for survival independently
    death_rolls <- runif(length(infected_indices))
    died_today <- infected_indices[death_rolls < prob_death]
    
    if (length(died_today) > 0) {
      agents$status[died_today] <- "Deceased"
    }
  }
  
  # --- Phase 2: Handle Recoveries ---
  # Only agents who survived the day can recover
  recovery_threshold <- step - recovery_time
  recover_indices <- which(agents$status == "Infected" & agents$infected_day <= recovery_threshold)
  if (length(recover_indices) > 0) {
    agents$status[recover_indices] <- "Recovered"
  }
  
  # --- Phase 3: Run Transmission ---
  agents <- run_time_step(agents, prob_infection, immunity_multiplier, step)
  
  # --- Phase 4: Record All Four Counts ---
  history$Susceptible[step] <- sum(agents$status == "Susceptible")
  history$Infected[step]    <- sum(agents$status == "Infected")
  history$Recovered[step]   <- sum(agents$status == "Recovered")
  history$Deceased[step]    <- sum(agents$status == "Deceased")
}

# SIR plot w/ death
# Plot Susceptible curve
plot(history$Day, history$Susceptible, type = "l", col = "blue", lwd = 2,
     ylim = c(0, num_agents), xlab = "Days", ylab = "Agent Count",
     main = "SIRD Model Simulation with Reinfection")

# Add the remaining states
lines(history$Day, history$Infected, col = "red", lwd = 2)
lines(history$Day, history$Recovered, col = "green", lwd = 2)
lines(history$Day, history$Deceased, col = "black", lwd = 2, lty = 2) # Dashed line for death

# Add legend
legend("right", legend = c("Susceptible", "Infected", "Recovered", "Deceased"),
       col = c("blue", "red", "green", "black"), lty = c(1, 1, 1, 2), lwd = 2)


# Let's try something like IR/SJH and see how it goes... -----------------------
## Set up
num_agents <- 1000              # total number of fish (juveniles)
prob_stray <- 0.03              # likelihood of straying from natal stream
total_steps <- 50
  ## VERY SIMPLE!

# create initial population dataframe 
agents <- data.frame(
  id = 1:num_agents,
  status = "viable",
  stringsAsFactors = FALSE
)

## defining interaction rules - just straying
run_time_step <- function(population, stray_rate){
  for (i in 1:num_agents){
    if (population$status[i] == "viable"){
      if (runif(1) < stray_rate) {
        population$status[i] <- "strayed"
      }
    }
  }
  return(population)
}

# run model across time
history <- numeric(total_steps)

# main simulation loop
for (step in 1:total_steps){
  agents <- run_time_step(agents, prob_stray)
  
  history[step] <- sum(agents$status == "viable")
}

# View effect of straying (no carrying capacity or anything)
print(history)

# quick plot
plot(history, type = "l", col = "red", lwd = 2,
     xlab = "Time Steps (Years)", ylab = "Number of fish",
     main = "Population decline due to straying")


# IR/SJH - 2 populations -------------------------------------------------------
## Set up
num_agents <- 1000              # number of fish per population
prob_stray1 <- 0.03             # pop 1 likelihood of straying to pop 2
prob_stray2 <- 0.05             # pop 2 likelihood of straying to pop 1
total_steps <- 50
  # slightly more complicated, 2 populations
    ## VERY SIMPLE!

# create initial population dataframe 
agents1 <- data.frame(
  id = 1:num_agents,
  status = "pop1",
  stringsAsFactors = FALSE
)
agents2 <- data.frame(
  id = 1:num_agents,
  status = "pop2",
  stringsAsFactors = FALSE
)
agents <- rbind(agents1, agents2)

# ## defining interaction rules - just straying
# run_time_step <- function(population, stray_rate1, stray_rate2){
#   for (i in 1:num_agents){
#     if (population$status[i] == "pop1"){
#       if (runif(1) < stray_rate1) {
#         population$status[i] <- "pop2"
#       }
#     }
#     if (population$status[i] == "pop2"){
#       if (runif(1) < stray_rate2) {
#         population$status[i] <- "pop1"
#       }
#     }
#   }
#   return(population)
# }

# alternate approach
run_time_step <- function(population, stray_rate1, stray_rate2) {
  # Total number of fish in the dataframe
  n <- nrow(population)
  
  # Capture the original status before making any modifications
  current_status <- population$status
  
  # Generate independent random vectors for each potential transition
  rand_pop1 <- runif(n)
  rand_pop2 <- runif(n)
  
  # Apply transitions based strictly on the frozen current_status
  population$status <- ifelse(current_status == "pop1" & rand_pop1 < stray_rate1, "pop2",
                              ifelse(current_status == "pop2" & rand_pop2 < stray_rate2, "pop1", 
                                     current_status))
  
  return(population)
}

# run model across time
history <- data.frame(
  Year = 1:total_steps,
  Pop1 = numeric(total_steps),
  Pop2 = numeric(total_steps)
  )

# main simulation loop
for (step in 1:total_steps){
  agents <- run_time_step(agents, prob_stray1, prob_stray2)
  
  history$Pop1[step] <- sum(agents$status == "pop1")
  history$Pop2[step] <- sum(agents$status == "pop2")
}

# Plot the first line (Susceptible)
plot(history$Year, history$Pop1, type = "l", col = "blue", lwd = 2,
     ylim = c(0, (2*num_agents)), xlab = "Years", ylab = "Population",
     main = "Straying between two populations")

# Add the other line
lines(history$Year, history$Pop2, col = "red", lwd = 2)

# Add a helpful legend
legend("bottomright", legend = c("Population 1", "Population 2"),
       col = c("blue", "red"), lwd = 2)


# Straying and survival ---------------------------------------------------

# IR/SJH - 2 populations -------------------------------------------------------
## Set up
num_agents <- 1000              # number of fish per population
prob_stray1 <- 0.03             # pop 1 likelihood of straying to pop 2
prob_stray2 <- 0.05             # pop 2 likelihood of straying to pop 1
prob_surv1 <- 0.05              # annual survival probability for pop 1
prob_surv2 <- 0.8               # annual survival probability for pop 2
total_steps <- 50

# create initial population dataframe 
agents1 <- data.frame(
  id = 1:num_agents,
  status = "pop1",
  stringsAsFactors = FALSE
)
agents2 <- data.frame(
  id = (num_agents + 1):(2*num_agents),
  status = "pop2",
  stringsAsFactors = FALSE
)
agents <- rbind(agents1, agents2)

# Vectorized simulation function for one time step
run_time_step <- function(population, stray_rate1, stray_rate2, surv_rate1, surv_rate2) {
  n <- nrow(population)
  current_status <- population$status
  
  # Generate independent random numbers for survival and straying
  rand_surv <- runif(n)
  rand_stray <- runif(n)
  
  # Determine survival first based on original status
  # If already dead, they stay dead. Otherwise, check survival rate.
  is_alive <- ifelse(current_status == "pop1" & rand_surv < surv_rate1, TRUE,
                     ifelse(current_status == "pop2" & rand_surv < surv_rate2, TRUE, 
                            ifelse(current_status == "dead", FALSE, FALSE)))
  
  # Determine straying ONLY for individuals that survived this step
  # If they died or were already dead, status becomes/stays "dead"
  population$status <- ifelse(!is_alive, "dead",
                              ifelse(current_status == "pop1" & rand_stray < stray_rate1, "pop2",
                                     ifelse(current_status == "pop2" & rand_stray < stray_rate2, "pop1", 
                                            current_status)))
  
  return(population)
}

# run model across time
history <- data.frame(
  Year = 1:total_steps,
  Pop1 = numeric(total_steps),
  Pop2 = numeric(total_steps),
  Dead = numeric(total_steps)
)

# main simulation loop
for (step in 1:total_steps){
  agents <- run_time_step(agents, prob_stray1, prob_stray2, prob_surv1, prob_surv2)
  
  history$Pop1[step] <- sum(agents$status == "pop1")
  history$Pop2[step] <- sum(agents$status == "pop2")
  history$Dead[step] <- sum(agents$status == "dead")
}

# Plot the first line (Susceptible)
plot(history$Year, history$Pop1, type = "l", col = "blue", lwd = 2,
     ylim = c(0, (2*num_agents)), xlab = "Years", ylab = "Population",
     main = "Straying between two populations")

# Add the other line
lines(history$Year, history$Pop2, col = "red", lwd = 2)

# Add a helpful legend
plot(history$Year, history$Pop1, type = "l", col = "blue", lwd = 2,
     ylim = c(0, (2 * num_agents)), xlab = "Years", ylab = "Population Size",
     main = "Population Dynamics with Survival and Straying")

lines(history$Year, history$Pop2, col = "red", lwd = 2)
lines(history$Year, history$Dead, col = "black", lwd = 2, lty = 2) # Dashed line for deaths

legend("topright", legend = c("Population 1", "Population 2", "Total Dead"),
       col = c("blue", "red", "black"), lwd = 2, lty = c(1, 1, 2))
  # this doesn't make sense because they aren't being replenished..


# # straying, survival, and reproduction ------------------------------------
# num_agents  <- 100000  
# prob_stray1 <- 0.075  
# prob_stray2 <- 0.12  
# prob_surv1  <- 0.002  
# prob_surv2  <- 0.05   
# birth_rate1 <- 1500  
# birth_rate2 <- 1000   
# total_steps <- 50
# 
# agents1 <- data.frame(id = 1:num_agents, status = "pop1", stringsAsFactors = FALSE)
# agents2 <- data.frame(id = (num_agents + 1):(2*num_agents), status = "pop2", stringsAsFactors = FALSE)
# agents  <- rbind(agents1, agents2)
# 
# max_id <- max(agents$id)
# total_dead_counter <- 0
# 
# run_time_step <- function(population, stray_rate1, stray_rate2, surv_rate1, surv_rate2, b_rate1, b_rate2) {
#   n <- nrow(population)
#   if (n == 0) return(list(pop = population, dead_this_step = 0))
#   
#   current_status <- population$status
#   
#   rand_surv <- runif(n)
#   rand_stray <- runif(n)
#   
#   is_alive <- (current_status == "pop1" & rand_surv < surv_rate1) | 
#     (current_status == "pop2" & rand_surv < surv_rate2)
#   
#   population$status <- ifelse(current_status == "pop1" & rand_stray < stray_rate1, "pop2",
#                               ifelse(current_status == "pop2" & rand_stray < stray_rate2, "pop1", 
#                                      current_status))
#   
#   surv_pop1 <- sum(population$status == "pop1" & is_alive)
#   surv_pop2 <- sum(population$status == "pop2" & is_alive)
#   
#   population <- population[is_alive, , drop = FALSE]
#   dead_this_step <- n - nrow(population)
#   
#   new_births1 <- rpois(1, lambda = surv_pop1 * b_rate1)
#   new_births2 <- rpois(1, lambda = surv_pop2 * b_rate2)
#   total_births <- new_births1 + new_births2
#   
#   if (total_births > 0) {
#     new_ids <- (max_id + 1):(max_id + total_births)
#     max_id <<- max_id + total_births 
#     
#     new_status <- c(rep("pop1", new_births1), rep("pop2", new_births2))
#     newborns <- data.frame(id = new_ids, status = new_status, stringsAsFactors = FALSE)
#     population <- rbind(population, newborns)
#   }
#   
#   return(list(pop = population, dead_this_step = dead_this_step))
# }
# 
# history <- data.frame(
#   Year = 1:total_steps,
#   Pop1 = numeric(total_steps),
#   Pop2 = numeric(total_steps),
#   Dead = numeric(total_steps)
# )
# 
# for (step in 1:total_steps){
#   step_result <- run_time_step(agents, prob_stray1, prob_stray2, prob_surv1, prob_surv2, birth_rate1, birth_rate2)
#   agents <- step_result$pop
#   total_dead_counter <- total_dead_counter + step_result$dead_this_step
#   
#   history$Pop1[step] <- sum(agents$status == "pop1")
#   history$Pop2[step] <- sum(agents$status == "pop2")
#   history$Dead[step] <- total_dead_counter
# }
# 
# plot(history$Year, history$Pop1, type = "l", col = "blue", lwd = 2, 
#      ylim = c(0, max(c(history$Pop1, history$Pop2))), 
#      xlab = "Years", ylab = "Population Size", 
#      main = "Population Dynamics with Reproduction and Straying")
# 
# lines(history$Year, history$Pop2, col = "red", lwd = 2)
# 
# legend("topleft", legend = c("Population 1", "Population 2"), 
#        col = c("blue", "red"), lwd = 2)



# Simple Salmon Life Cycle Simulation Model -------------------------------
years <- 5000
spawners <- numeric(years)
eggs <- numeric(years)
smolts <- numeric(years)
adults <- numeric(years)

# Initial population
spawners[1] <- 1500

# Vital rates and parameters
fecundity <- 1900     # Average eggs per female
s_egg_base <- 0.065          # Egg to fry/smolt survival rate
s_ocean_base <- 0.04        # Smolt to adult ocean survival rate
harvest_rate <- 0.4   # Fishing mortality rate
carrying_cap <- 150000  # Stream rearing capacity (Beverton-Holt style)

for (t in 1:(years - 1)) {
  # 1. Spawning phase with random variation in fecundity
  # rpois protects against zero if numbers get low
  eggs[t] <- sum(rpois(spawners[t], lambda = fecundity)) * 0.5 
  
  # 2. Freshwater phase with density-dependence (Beverton-Holt) + environmental shock
  # Adding a random environmental multiplier to freshwater survival
  env_shock_fw <- max(0.2, rnorm(1, mean = 1, sd = 0.15)) 
  smolts_expected <- (s_egg_base * eggs[t]) / (1 + (s_egg_base * eggs[t]) / carrying_cap)
  smolts[t] <- smolts_expected * env_shock_fw
  
  # 3. Ocean phase with highly variable survival (simulating marine heatwaves/el niño)
  # Ocean conditions vary wildly using a log-normal distribution
  env_shock_ocean <- rlnorm(1, meanlog = 0, sdlog = 0.25)
  s_ocean_actual <- min(0.15, s_ocean_base * env_shock_ocean)
  raw_adults <- smolts[t] * s_ocean_actual
  
  # 4. Harvest phase
  adults[t] <- raw_adults * (1 - harvest_rate)
  
  # 5. Next year's spawners (escapement)
  spawners[t + 1] <- adults[t]
  
  # Extinction safeguard for the simulation loop
  if (spawners[t + 1] <= 0) {
    spawners[(t+1):years] <- 0
    break
  }
}

# Plot the dynamic, stable population
plot(1:years, spawners, type = "b", col = "darkgreen", lwd = 2, pch = 16,
     xlab = "Year", ylab = "Number of Spawners",
     main = "Stochastic Salmon Spawner Abundance Over Time")
# Add a line to show the healthy baseline equilibrium
abline(h = mean(spawners[10:years]), col = "red", lty = 2)



# ATTEMPT AT FULL MODEL --------------------------------------------------------
## Set up
# inputs
num_wild_init     <- 2000   
num_hatchery_init <- 10000

surv_wild     <- 0.001   
surv_hatchery <- 0.05 

mean_stray_wild_init     <- 0.05  
mean_stray_hatchery_init <- 0.15

fecundity_wild     <- 1000  
fecundity_hatchery <- 1000

hatchery_annual_release <- 10000 
stream_carrying_capacity <- 20000

total_steps             <- 10 

# dataframes
wild_agents <- data.frame(
  id = 1:num_wild_init,
  origin = "wild",
  stray_rate = rnorm(num_wild_init, mean = mean_stray_wild_init, sd = 0.01)
)

hatch_agents <- data.frame(
  id = (num_wild_init + 1):(num_wild_init + num_hatchery_init),
  origin = "hatchery",
  stray_rate = rnorm(num_hatchery_init, mean = mean_stray_hatchery_init, sd = 0.01)
)

agents <- rbind(wild_agents, hatch_agents)
agents$stray_rate <- pmax(0, pmin(1, agents$stray_rate)) # Bound probabilities between 0 and 1

max_id <- max(agents$id)

## build main loop function
run_generation <- function(current_agents, s_wild, s_hatch, f_wild, f_hatch, hatch_release, capacity) {
  n <- nrow(current_agents)
  if (n == 0) return(data.frame(id=integer(), origin=character(), stray_rate=numeric()))
  
  # freshwater/early survival
  rand_surv <- runif(n)
  is_surviving <- ifelse(current_agents$origin == "wild" & rand_surv < s_wild, TRUE,
                         ifelse(current_agents$origin == "hatchery" & rand_surv < s_hatch, TRUE, FALSE))
  
  returning_adults <- current_agents[is_surviving, ]
  n_adults <- nrow(returning_adults)
  
  if (n_adults == 0) {
    # if all die, the hatchery still releases its fish next year
    new_hatchery <- data.frame(
      id = (max_id + 1):(max_id + hatch_release),
      origin = "hatchery",
      stray_rate = rnorm(hatch_release, mean = mean_stray_hatchery_init, sd = 0.01)
    )
    max_id <<- max_id + hatch_release
    return(new_hatchery)
  }
    # not so sure on this piece - they would need to source new broodstock but whatever
  
  # straying
  rand_stray <- runif(n_adults)
  returning_adults$spawning_site <- ifelse(rand_stray < returning_adults$stray_rate,
                                           ifelse(returning_adults$origin == "wild", "hatchery", "wild"),
                                           returning_adults$origin)
  
  # reproduction and heritability with carrying capacity - all adults die after this step
  stream_spawners <- returning_adults[returning_adults$spawning_site == "wild", ]
  
  new_wild_agents <- data.frame(id=integer(), origin=character(), stray_rate=numeric())
  n_stream <- nrow(stream_spawners)
  
  if (n_stream >= 2) {
    # pairs to mate
    parents1 <- stream_spawners[sample(1:n_stream, n_stream, replace = TRUE), ]
    parents2 <- stream_spawners[sample(1:n_stream, n_stream, replace = TRUE), ]
    
    # calculate eggs per pair based on maternal origin - parent 1 is mother
    eggs_per_pair <- ifelse(parents1$origin == "wild", f_wild, f_hatch)
    total_potential_eggs <- sum(eggs_per_pair)
    
    if (total_potential_eggs > 0) {
      # Beverton-Holt density dependence applied to offspring limit
      # prevents total born wild juveniles from exceeding the stream capacity
      actual_offspring_count <- round(total_potential_eggs / (1 + (total_potential_eggs / capacity)))
      
      # sample indices proportionally to eggs produced by each parent pair
      sampled_parent_indices <- sample(1:n_stream, size = actual_offspring_count, replace = TRUE, prob = eggs_per_pair)
      
      # offspring inherit the mid-parent stray rate plus a tiny mutation/variation
      mid_parent_stray <- (parents1$stray_rate[sampled_parent_indices] + parents2$stray_rate[sampled_parent_indices]) / 2
      offspring_stray <- mid_parent_stray + rnorm(actual_offspring_count, mean = 0, sd = 0.005)
      offspring_stray <- pmax(0, pmin(1, offspring_stray))
      
      new_ids <- (max_id + 1):(max_id + actual_offspring_count)
      max_id <<- max_id + actual_offspring_count
      
      new_wild_agents <- data.frame(
        id = new_ids,
        origin = "wild",
        stray_rate = offspring_stray,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # fixed hatchery release - overrides natural reproduction
  new_hatch_ids <- (max_id + 1):(max_id + hatch_release)
  max_id <<- max_id + hatch_release
  
  new_hatchery_agents <- data.frame(
    id = new_hatch_ids,
    origin = "hatchery",
    stray_rate = rnorm(hatch_release, mean = mean_stray_hatchery_init, sd = 0.01),
    stringsAsFactors = FALSE
  )
  
  next_generation <- rbind(new_wild_agents, new_hatchery_agents)
  return(next_generation)
}

## run simulation
# dataframe to store information
history <- data.frame(
  Generation = 1:total_steps,
  Wild_Abundance = numeric(total_steps),
  Hatchery_Abundance = numeric(total_steps),
  Mean_Stream_Stray_Gene = numeric(total_steps)
)

# running
for (gen in 1:total_steps) {
  # track status 
  history$Wild_Abundance[gen] <- sum(agents$origin == "wild")
  history$Hatchery_Abundance[gen] <- sum(agents$origin == "hatchery")
  history$Mean_Stream_Stray_Gene[gen] <- mean(agents$stray_rate[agents$origin == "wild"])
  
  agents <- run_generation(agents, surv_wild, surv_hatchery, fecundity_wild, fecundity_hatchery, hatchery_annual_release, stream_carrying_capacity)
}

# what is this??
par(mfrow = c(1, 2))

## plot results
plot(history$Generation, history$Wild_Abundance, type = "b", col = "darkgreen", lwd = 2,
     ylim = c(0, max(c(history$Wild_Abundance, history$Hatchery_Abundance))),
     xlab = "Generations", ylab = "Abundance (Smolts/Fry)", 
     main = "Population Sizes Over Time")
lines(history$Generation, history$Hatchery_Abundance, type = "b", col = "orange", lwd = 2)
legend("bottomright", legend = c("Wild Born", "Hatchery Released"), col = c("darkgreen", "orange"), lwd = 2)

plot(history$Generation, history$Mean_Stream_Stray_Gene, type = "b", col = "purple", lwd = 2,
     xlab = "Generations", ylab = "Mean Straying Trait Value",
     main = "Evolution of Wild Straying Trait")

# salmon agent-based simulation model: straying parameter sweep
library(tidyverse)

# wrap logic into a function that returns abundance metric
run_salmon_sim <- function(wild_stray_rate, hatch_stray_rate) {
  
  # parameters
  Years        <- 50     # number of seasons
  Weeks        <- 12     # weeks per season
  Fecundity    <- 4      # average surviving smolts/offspring per spawning adult
  HeritStray   <- 0      # 0 = offspring stray rate is mean of population; 1 = exact mid-parent trait
  HeritTime    <- 1      # 0 = offspring stray rate is mean of population; 1 = exact mid-parent trait
  
  # stream/hatchery settings
  Stream_K          <- 1500   # Carrying capacity of the Wild Stream (K)
  Hatchery_Release  <- 3000   # Constant annual smolt release from hatchery
  Broodstock_Need   <- 250
  
  Initial_Abundance <- c(1500, 3000) # [1 = Wild Stream, 2 = Hatchery]
  Natal_Mortality   <- c(0.15, 0.10) 
  Mean_Run_Timing   <- c(4.0, 7.0)   # Wild is early-ish (4); Hatchery is mid-run (7)
  
  # initialization - starting agents dataframes
  init_stream <- function(id_start, num_fish, natal_origin, initial_stray_prob, initial_mean_week) {
    data.frame(
      id         = id_start:(id_start + num_fish - 1),
      natal      = rep(natal_origin, num_fish),   
      current    = rep(natal_origin, num_fish), 
      stray_prob = rnorm(num_fish, mean = initial_stray_prob, sd = 0.01),
      run_timing = rnorm(num_fish, mean = initial_mean_week, sd = 1.2)
    )
  }

  agents1 <- init_stream(id_start = 1, 
                         num_fish = Initial_Abundance[1], 
                         natal_origin = 1, 
                         initial_stray_prob = wild_stray_rate, 
                         initial_mean_week = Mean_Run_Timing[1])
  
  agents2 <- init_stream(id_start = Initial_Abundance[1] + 1, 
                         num_fish = Initial_Abundance[2], 
                         natal_origin = 2, 
                         initial_stray_prob = hatch_stray_rate, 
                         initial_mean_week = Mean_Run_Timing[2])
  
  agents  <- rbind(agents1, agents2)
  
  # tracking annual abundance and harvest
  history <- data.frame(
    Year       = 1:Years, 
    WildStream = numeric(Years), 
    Hatchery   = numeric(Years),
    Harvested  = numeric(Years),
    Wild_AvgStray = numeric(Years), 
    Wild_AvgTime  = numeric(Years), 
    Hatch_AvgStray= numeric(Years), 
    Hatch_AvgTime = numeric(Years)  
    
  )
  
  # main simulation loop
  next_id <- max(agents$id) + 1
  
  for (yr in 1:Years) {
    n_fish <- nrow(agents)
    
    if (n_fish == 0) {
      cat("Extinction event in year:", yr, "\n")
      break
    }
    
    # does an individual stray?
    will_stray <- runif(n_fish) < agents$stray_prob
    
    agents$current <- ifelse(will_stray, 
                             ifelse(agents$natal == 1, 2, 1), # stray to opposite stream
                             agents$natal) # return to natal stream
    
    # prespawn mortality  - mortality threshold based on origin
    mort_threshold <- ifelse(agents$natal == 1, Natal_Mortality[1], Natal_Mortality[2])
    survived_natal_mort <- runif(n_fish) > mort_threshold
    surv_agents <- agents[survived_natal_mort, ]
    
    # log traits before replacement occurs
    wild_born     <- surv_agents[surv_agents$natal == 1, ]
    hatchery_born <- surv_agents[surv_agents$natal == 2, ]
    
    history$Wild_AvgStray[yr] <- if(nrow(wild_born) > 0) mean(wild_born$stray_prob) else NA
    history$Wild_AvgTime[yr]  <- if(nrow(wild_born) > 0) mean(wild_born$run_timing) else NA
    
    history$Hatch_AvgStray[yr] <- if(nrow(hatchery_born) > 0) mean(hatchery_born$stray_prob) else NA
    history$Hatch_AvgTime[yr]  <- if(nrow(hatchery_born) > 0) mean(hatchery_born$run_timing) else NA
    
    # apply mortality filter to progress simulation loop
    agents <- surv_agents
    
    # update population count post-mortality
    n_fish <- nrow(agents)
    if (n_fish == 0) break
    
    # assign entry week rounded to discrete weeks, bounded by season limits
    agents$entry_week <- round(agents$run_timing)
    agents$entry_week <- pmax(1, pmin(agents$entry_week, Weeks))
    
    river_returns    <- agents[agents$current == 1, ]
    hatchery_returns <- agents[agents$current == 2, ]
    
    # log returns
    history$WildStream[yr] <- nrow(river_returns)
    history$Hatchery[yr]   <- nrow(hatchery_returns)
    
    # submodel a: wild stream spawning w/ density dependence
    wild_offspring <- list()
    n_river <- nrow(river_returns)
    
    if (n_river >= 2) {
      # density dependence impacts spawning capacity
      expected_adults_per_spawner <- Fecundity * (1 - Natal_Mortality[1])
      success_prob_river <- min(1, Stream_K / (n_river * expected_adults_per_spawner))
      river_survival <- runif(n_river) < success_prob_river
      river_spawners <- river_returns[river_survival, ]
      n_spawners_river <- nrow(river_spawners)
      
      # week-by-week spawning within the river
      if (n_spawners_river >= 2) {
        for (wk in 1:Weeks) {
          wk_spawners <- river_spawners[river_spawners$entry_week == wk, ]
          n_wk <- nrow(wk_spawners)
          
          if (n_wk >= 2) {
            wk_offspring_count <- round(n_wk * Fecundity)
            if (wk_offspring_count > 0) {
              p1 <- sample(1:n_wk, wk_offspring_count, replace = TRUE)
              p2 <- sample(1:n_wk, wk_offspring_count, replace = TRUE)
              
              # inherit stray probability w/ noise
              mid_stray <- (wk_spawners$stray_prob[p1] + wk_spawners$stray_prob[p2]) / 2
              pop_stray <- mean(river_spawners$stray_prob)
              o_stray   <- (HeritStray * mid_stray) + ((1 - HeritStray) * pop_stray)
              o_stray   <- pmax(0, pmin(o_stray + rnorm(wk_offspring_count, 0, 0.002), 1))
              
              # inherit run timing w/ noise
              mid_time  <- (wk_spawners$run_timing[p1] + wk_spawners$run_timing[p2]) / 2
              pop_time  <- mean(river_spawners$run_timing)
              o_time    <- (HeritTime * mid_time) + ((1 - HeritTime) * pop_time) + rnorm(wk_offspring_count, 0, 0.5)
              
              wild_offspring[[wk]] <- data.frame(
                id = next_id:(next_id + wk_offspring_count - 1),
                natal = 1, current = 1, stray_prob = o_stray, run_timing = o_time
              )
              next_id <- next_id + wk_offspring_count
            }
          }
        }
      }
    }
    wild_generation <- do.call(rbind, wild_offspring)
    
    # submodel b: hatchery returns (broodstock selection and harvest)
    hatchery_generation <- data.frame()
    n_hatch <- nrow(hatchery_returns)
    
    if (n_hatch >= 2) {
      # bias selection toward early weeks - week 1 (of W) has weight W, Week W has weight 1.
      ## this might be overly simplistic..
      sampling_weights <- (Weeks + 1) - hatchery_returns$entry_week
      
      # cap sampling needs at whatever returned
      actual_brood_count <- min(Broodstock_Need, n_hatch)
      
      # sample individual broodstock rows based on early returning bias
      broodstock_indices <- sample(1:n_hatch, size = actual_brood_count, 
                                   replace = FALSE, prob = sampling_weights)
      broodstock <- hatchery_returns[broodstock_indices, ]
      
      # remaining fish are routed to commercial harvest logs
      history$Harvested[yr] <- n_hatch - actual_brood_count
      
      # produce fixed # of offspring matrix from the selected broodstock
      if (actual_brood_count >= 2) {
        p1 <- sample(1:actual_brood_count, Hatchery_Release, replace = TRUE)
        p2 <- sample(1:actual_brood_count, Hatchery_Release, replace = TRUE)
        
        mid_stray <- (broodstock$stray_prob[p1] + broodstock$stray_prob[p2]) / 2
        pop_stray <- mean(broodstock$stray_prob)
        o_stray   <- (HeritStray * mid_stray) + ((1 - HeritStray) * pop_stray)
        o_stray   <- pmax(0, pmin(o_stray + rnorm(Hatchery_Release, 0, 0.002), 1))
        
        mid_time  <- (broodstock$run_timing[p1] + broodstock$run_timing[p2]) / 2
        pop_time  <- mean(broodstock$run_timing)
        o_time    <- (HeritTime * mid_time) + ((1 - HeritTime) * pop_time) + rnorm(Hatchery_Release, 0, 0.5)
        
        hatchery_generation <- data.frame(
          id = next_id:(next_id + Hatchery_Release - 1),
          natal = 2, current = 2, stray_prob = o_stray, run_timing = o_time
        )
        next_id <- next_id + Hatchery_Release
      }
    } else {
      history$Harvested[yr] <- 0
    }
    
    # progeny become next year's agents
    agents <- rbind(wild_generation, hatchery_generation)
    if (is.null(agents)) { agents <- data.frame() }
  } 
  
  # calculate and return average abundance across years
  return(mean(history$WildStream, na.rm = TRUE))
}

# define stray parameter grid
wild_stray_levels  <- seq(0.00, 0.5, by = 0.05)
hatch_stray_levels <- seq(0.00, 0.5, by = 0.05)

sweep_grid <- expand.grid(
  Wild_Stray  = wild_stray_levels,
  Hatch_Stray = hatch_stray_levels
)

# run parameter sweep loop
cat("Starting parameter sweep across", nrow(sweep_grid), "scenarios...\n")

sweep_results <- sweep_grid %>%
  rowwise() %>%
  mutate(Avg_Wild_Abundance = run_salmon_sim(Wild_Stray, Hatch_Stray)) %>%
  ungroup()

# plot- heat map
ggplot(sweep_results, aes(x = factor(Wild_Stray), y = factor(Hatch_Stray), fill = Avg_Wild_Abundance)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # continuous color palette tailored for ecological abundance data
  scale_fill_viridis_c(option = "mako", direction = -1, na.value = "black") +
  labs(
    title = "Wild Stream Abundance Across Varying Stray Regimes",
    subtitle = "Mean annual wild stream returns over a 50-year horizon",
    x = "Base Wild-to-Hatchery Stray Rate",
    y = "Base Hatchery-to-Wild Stray Rate",
    fill = "Mean Returns\n(Wild Habitat)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    axis.text = element_text(size = 9),
    panel.grid = element_blank()
  )
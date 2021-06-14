library(targets)
library(tarchetypes)
library(magrittr)

source("R/simulate.R")
source("R/fit.R")
source("R/calculate-power.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "here", "glue", "broom",
                            "gasr"))

# The data storage location
data_dir <- here::here("..", "data", "n-goals_n-levels")
# The number of simulations per set of parameters
n_sim <- 1000

set.seed(15)

list(
  tar_target(n_goals, c(1, 2, 3, 4, 5, 6, 7)),
  tar_target(n_levels, c(3, 5, 7)),
  tar_target(n_subjects, c(40, 60, 80)),
  tar_target(delta, c(0.3, 0.5, 0.7)),
  tar_target(
    sim_data,
    simulate(n_goals, n_levels, n_subjects, delta,
             n_sim, data_dir),
    pattern = cross(n_goals, n_levels, n_subjects, delta),
    format = "file"
  ),
  
  tar_target(
    sim_data_ttest,
    fit_data(sim_data),
    pattern = sim_data
  ),
  
  tar_target(
    sim_data_power,
    calculate_power(sim_data_ttest),
    pattern = sim_data_ttest
  )
)

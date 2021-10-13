library(targets)
library(tarchetypes)

source("R/simulate.R")
source("R/fit.R")
source("R/measure-performance.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "here", "glue", "broom",
                            "gasr"))

# The data storage location
data_dir <- here::here("..",
                       "data", "n-subjects_delta_n-goals_n-levels_score-dist")

set.seed(15)

list(
  # Number of simulations per set of parameters
  tar_target(n_sim, c(1000)),
  # Sets of parameters to simulate
  tar_target(n_goals, c(1, 2, 3, 4, 5)),
  tar_target(n_subjects, c(40, 60, 80)),
  tar_target(delta, c(0.3, 0.5, 0.7, 0.9, 1.1)),
  tar_target(n_levels, c(5)),
   
  # Simulate the data
  tar_target(
    sim_data,
    simulate(n_subjects, delta, n_goals, n_levels,
             n_sim, data_dir),
    pattern = cross(n_subjects, delta, n_goals, n_levels),
    format = "file"
  ),
  
  # Fit
  tar_target(
    sim_data_tscore_ttest,
    fit_tscore_ttest(sim_data),
    pattern = sim_data
  ),
  tar_target(
    sim_data_mean_score_ttest,
    fit_mean_score_ttest(sim_data),
    pattern = sim_data
  ),
  
  # Calculate performance metrics
  tar_target(
    sim_data_tscore_power,
    calculate_power(sim_data_tscore_ttest),
    pattern = sim_data_tscore_ttest
  ),
  tar_target(
    sim_data_mean_score_power,
    calculate_power(sim_data_mean_score_ttest),
    pattern = sim_data_mean_score_ttest
  ),
  tar_target(
    sim_data_mean_score_perf,
    calculate_performance(sim_data_mean_score_ttest),
    pattern = sim_data_mean_score_ttest
  ),
  tar_target(
    sim_data_tscore_effect_size,
    calculate_tscore_effect_size(sim_data),
    pattern = sim_data
  )
)

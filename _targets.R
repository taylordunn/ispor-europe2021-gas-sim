library(targets)
library(tarchetypes)
library(future)
library(future.callr)

source("R/simulate.R")
source("R/fit.R")
source("R/calculate.R")
source("R/visualize.R")
source("R/utils.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "here", "gasr", "glue", "broom",
                            "gasr", "ardea", "ggtext", "patchwork"))

# Load the Arial font for Ardea theme plots
extrafont::loadfonts(device = "win", quiet = TRUE)

# Parallelize targets locally with the future package
future::plan(callr)

# The data storage location
# A lot of data will be generated, so use the shared drive
project_dir <- file.path("G:", "Shared drives", "Current Projects",
                         "GAS Data Simulations", "ispor-europe2021-gas-sim")
data_dir <- file.path(project_dir, "data")
models_dir <- file.path(project_dir, "models")
results_dir <- file.path(project_dir, "results")
figs_dir <- file.path(results_dir, "figures")

list(
  # Number of simulations per set of parameters
  tar_target(n_sim, 1e4),
  # Sets of parameters to simulate
  tar_target(n_subjects, c(40, 50, 60, 70, 80)),
  tar_target(delta, c(0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1.1)),
  tar_target(n_goals, c(1, 2, 3, 4, 5, 6)),
  tar_target(rho, c(0.3)),
   
  # Simulate the data with a treatment effect
  tar_target(
    sim_data,
    simulate(n_subjects, delta, n_goals, rho, n_sim, save_dir = data_dir),
    pattern = cross(n_subjects, delta, n_goals, rho, n_sim),
    format = "file"
  ),
  # Simulate data under the null hypothesis (no effect) 
  tar_target(
    sim_data_null,
    simulate(n_subjects, delta = 0.0, n_goals, rho, n_sim, save_dir = data_dir),
    pattern = cross(n_subjects, n_goals, rho, n_sim),
    format = "file"
  ),
  
  # Fit data and compute effect sizes
  tar_target(
    sim_data_tscore_ttest,
    fit_tscore_ttest(sim_data),
    pattern = sim_data
  ),
  tar_target(
    sim_data_null_tscore_ttest,
    fit_tscore_ttest(sim_data_null),
    pattern = sim_data_null
  ),
  
  # Calculate power
  tar_target(
    sim_data_tscore_ttest_power,
    calculate_power(sim_data_tscore_ttest),
    pattern = sim_data_tscore_ttest
  ),
  # Calculate type 1 error
  tar_target(
    sim_data_null_tscore_ttest_type1_error,
    calculate_power(sim_data_null_tscore_ttest),
    pattern = sim_data_null_tscore_ttest
  ),
  # Calculate effect size difference
  tar_target(
    effect_size_summary,
    summarize_effect_size(sim_data_tscore_ttest),
    pattern = sim_data_tscore_ttest
  ),
  
  # Plot
  tar_target(
    fig_score_distribution_legend,
    plot_score_distribution_legend(save_dir = figs_dir),
    format = "file"
  ),
  tar_target(
    fig_power_delta_n_goals,
    plot_power_delta_n_goals(sim_data_tscore_ttest_power,
                             plot_n_subjects = n_subjects, save_dir = figs_dir),
    pattern = map(n_subjects),
    format = "file"
  ),
  tar_target(
    fig_effect_size_delta_n_goals,
    plot_effect_size_delta_n_goals(
      effect_size_summary,
      plot_n_subjects = n_subjects, save_dir = figs_dir
    ),
    pattern = map(n_subjects),
    format = "file"
  ),
  tar_target(
    fig_power_diff_grid,
    plot_power_diff_grid(sim_data_tscore_ttest_power, save_dir = figs_dir),
    #pattern = sim_data_tscore_ttest_power,
    format = "file"
  )
)

#' Simulate many GAS trials
#' 
#' A wrapper function around `gasr::sim_trial()` to simulate many trials with
#' the same set of parameters.
#' This function also compute discrete goal scores (and the corresponding
#' T-scores) using both normal and uniform thresholds.
#'
#' @param n_subjects Total number of subjects, equally split between control
#'  and treatment groups.
#' @param delta Simulated treatment effect size.
#' @param n_goals Number of goals per subject.
#' @param rho The simulated degree of inter-correlation for a subject's goals.
#' @param n_sim Number of simulations to run.
#' @param save_dir Directory to save the data. If not provided, defaults to
#'  the directory returned by `here::here()`
#' @param save_filename Name of the data file. If not provided, produces one
#'  based on the provided simulated parameters via `sim_params_string()`.
#'
#' @return The file path of the saved RDS file.
simulate <- function(
  n_subjects = 100, delta = 0.3, n_goals = 3, rho = 0.3, n_sim = 1e4,
  save_dir = NULL, save_filename = NULL
) {
  if (is.null(save_dir)) save_dir <- here::here()
  if (is.null(save_filename)) {
    # Generate a filename from the simulation parameters
    save_filename <-
      paste0(sim_params_string(n_subjects, delta, n_goals, rho, n_sim),
             ".rds")
  }
  
  # Also apply alternate thresholds approximating a uniform score distribution
  thresh_unif <- gasr::create_thresholds(score_dist = "unif")
  
  # From rho, the correlation between goals (in the control group), set the
  #  variances such that sigma_u^2 + sigma_e^2 = 1
  sigma_u <- sqrt(rho)
  sigma_e <- sqrt(1 - rho)
  
  d <- tibble(
    sim = 1:n_sim,
  ) %>%
    rowwise() %>%
    mutate(
      sim_n_subjects = n_subjects, sim_n_goals = n_goals, sim_delta = delta,
      sim_rho = rho,
      data = list(
        gasr::sim_trial(
          n_subjects = n_subjects, delta = delta,
          n_goals = n_goals, sigma_u = sigma_u, sigma_e = sigma_e
        ) %>%
          rename(score_discrete_norm = score_discrete, tscore_norm = tscore) %>%
          mutate(
            score_discrete_unif = discretize_from_thresholds(score_continuous,
                                                             thresh_unif)
          ) %>%
          group_by(subject_id) %>%
          mutate(tscore_unif = calc_tscore(score_discrete_unif)) %>%
          ungroup()
      )
    ) %>%
    ungroup()
  
  save_dir_filename <- file.path(save_dir, save_filename)
  write_rds(d, save_dir_filename)
  return(save_dir_filename)
}

#' Return a string representation of simulation parameters
#'
#' Convenience function for file and target naming by combining the parameters
#' of a simulation into a single string.
#'
#' @param n_subjects Number of subjects.
#' @param delta Standardized effect size.
#' @param n_goals Number of goals per subject.
#' @param rho Goal correlation within subjects (in control group).
#' @param n_sim Number of simulations.
#'
#' @return A single string.
sim_params_string <- function(n_subjects, delta, n_goals, rho, n_sim) {
  glue::glue("n-subjects{n_subjects}_delta{delta}_",
             "n-goals{n_goals}_rho{rho}_n-sim{n_sim}")
}

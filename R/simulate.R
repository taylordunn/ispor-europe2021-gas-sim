simulate <- function(
  n_subjects = 100, delta = 0.3, n_goals = 3, n_levels = 5,
  n_sim = 1000, data_dir = "data/"
) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  filename <- here(data_dir,
                   glue("n-subjects{n_subjects}_delta{delta}_",
                        "n-goals{n_goals}_n-levels{n_levels}_",
                        "n-sim{n_sim}.rds"))
  
  # Also apply alternate thresholds approximating a uniform score distribution
  thresh_unif <- create_thresholds(score_dist = "unif")
  
  d <- tibble(
    sim = 1:n_sim,
  ) %>%
    mutate(
      data = map(
        sim,
        ~sim_trial(n_subjects = n_subjects, delta = delta,
                   n_goals = n_goals, n_levels = n_levels) %>%
          rename(
            score_discrete_norm = score_discrete,
            tscore_norm = tscore
          ) %>%
          mutate(
            score_discrete_unif = discretize_from_thresholds(score_continuous,
                                                             thresh_unif)
          ) %>%
          group_by(subject_id) %>%
          mutate(tscore_unif = calc_tscore(score_discrete_unif)) %>%
          ungroup()
      ),
      n_subjects = n_subjects, delta = delta,
      n_goals = n_goals, n_levels = n_levels
    )
  
  write_rds(d, filename)
  
  return(filename)
}

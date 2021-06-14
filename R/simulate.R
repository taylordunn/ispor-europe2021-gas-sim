simulate <- function(
  n_goals, n_levels,
  n_subjects = 100, delta = 0.3,
  n_sim = 1000, data_dir = "data/"
) {
  #set.seed(seed)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  filename <- here(data_dir,
                   glue("n-goals{n_goals}_n-levels{n_levels}_",
                        "n-subjects{n_subjects}_delta{delta}_",
                        "n-sim{n_sim}.rds"))
  
  d <- tibble(
    sim = 1:n_sim,
  ) %>%
    mutate(
      data = map(
        sim,
        ~sim_trial(n_subjects = n_subjects, delta = delta,
                   n_goals = n_goals, n_levels = n_levels)
      ),
      #seed = seed,
      n_goals = n_goals, n_levels = n_levels,
      n_subjects = n_subjects, delta = delta
    )
  write_rds(d, filename)
  
  return(filename)
}

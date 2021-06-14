calculate_power <- function(sim_data_fit, alpha = 0.05) {
  sim_data_fit %>%
    group_by(n_goals, n_levels, n_subjects, delta) %>%
    summarise(
      power = mean(p.value < alpha),
      across(starts_with("tscore"),
             .fns = list(mean = mean, sd = sd)),
      .groups = "drop"
    )
}
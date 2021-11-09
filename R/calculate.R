calculate_power <- function(sim_data_fit, alpha = 0.05) {
  sim_data_fit %>%
    filter(
      stat %in% c("p_value", "cohen_d", "tscore_diff",
                  "tscore_diff_conf_low", "tscore_diff_conf_high",
                  "tscore_control", "tscore_treatment")
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    group_by(sim_n_subjects, sim_n_goals, sim_delta, sim_rho, score_dist) %>%
    summarise(
      n_sim = n(),
      power = mean(p_value < alpha),
      across(matches("^tscore|^cohen_d"),
             .fns = list(mean = mean, sd = sd)),
      .groups = "drop"
    )
}

calculate_performance <- function(sim_data_fit) {
  sim_data_fit %>%
    filter(
      stat %in% c("score_diff", "tscore_diff")
    ) %>%
    mutate(stat = ifelse(stat == "tscore_diff", "score_diff", stat)) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    group_by(
      n_subjects, delta, n_goals, n_levels, score_dist,
    ) %>%
    summarise(
      n_sim = n(),
      empirical_se = sd(score_diff),
      empirical_se_se = empirical_se / sqrt(n_sim),
      bias = mean(score_diff - delta),
      bias_se = bias / sqrt(n_sim),
      .groups = "drop"
    )
}

summarize_effect_size <- function(sim_data_fit, stat_summary = c("cohen_d")) {
  sim_data_fit %>%
    filter(stat %in% stat_summary) %>%
    pivot_wider(names_from = score_dist, values_from = value) %>%
    mutate(diff = norm - unif) %>%
    group_by(sim_n_subjects, sim_n_goals, sim_delta, sim_rho, stat) %>%
    summarise(
      n_sim = n(),
      across(c(norm, unif, diff),
             .fns = list(mean = mean, sd = sd,
                         median = median, min = min, max = max)),
      .groups = "drop"
    )
}
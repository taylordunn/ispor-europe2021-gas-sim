calculate_power <- function(sim_data_fit, alpha = 0.05) {
  sim_data_fit %>%
    filter(
      stat %in% c("p_value", "tscore_diff",
                  "tscore_control", "tscore_treatment")
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    group_by(n_subjects, delta, n_goals, n_levels, score_dist) %>%
    summarise(
      n_sim = n(),
      power = mean(p_value < alpha),
      across(starts_with("tscore"),
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

calculate_tscore_effect_size <- function(filename) {
  sim_data <- read_rds(filename)
  
  sim_data %>%
    mutate(
      tscore_effect_size = map(
        data,
        ~{
          distinct(., subject_id, group, tscore_norm, tscore_unif) %>%
            pivot_longer(cols = c(tscore_norm, tscore_unif),
                         names_to = "tscore", values_to = "score") %>%
            separate(tscore, into = c("tscore", "score_dist"), sep = "_") %>%
            select(-tscore) %>%
            group_by(score_dist) %>%
            summarise(
              cohen_d = (mean(score[group == "treatment"]) - mean(score[group == "control"])) /
                sd(score),
              .groups = "drop"
            )
        }
      )
    ) %>%
    select(-data) %>%
    unnest(tscore_effect_size)
}
fit_tscore_ttest <- function(filename) {
  sim_data <- read_rds(filename)
  
  sim_data %>%
    mutate(
      ttest_tscore_norm = map(
        data,
        ~t.test(
          tscore_norm ~ group,
          # We are only analyzing the subject-level T-scores
          data = distinct(., subject_id, group, tscore_norm)
        )
      ),
      ttest_tscore_unif = map(
        data,
        ~t.test(
          tscore_unif ~ group,
          data = distinct(., subject_id, group, tscore_unif)
        )
      ),
      ttest_tscore_norm = map(
        ttest_tscore_norm,
        ~broom::tidy(.) %>% rename_with(~str_c(.x, "_norm"))
      ),
      ttest_tscore_unif = map(
        ttest_tscore_unif,
        ~broom::tidy(.) %>% rename_with(~str_c(.x, "_unif"))
      )
    ) %>%
    select(-data) %>%
    unnest(c(ttest_tscore_norm, ttest_tscore_unif)) %>%
    select(-starts_with("method"), -starts_with("alternative")) %>%
    pivot_longer(
      cols = estimate_norm:conf.high_unif,
      names_to = "stat", values_to = "value"
    ) %>%
    separate(
      stat, into = c("stat", "score_dist"), sep = "_"
    ) %>%
    mutate(
      stat = recode(
        stat,
        estimate = "tscore_diff",
        conf.low = "tscore_diff_conf_low", conf.high = "tscore_diff_conf_high",
        estimate1 = "tscore_control", estimate2 = "tscore_treatment",
        statistic = "t", p.value = "p_value", parameter = "df"
      )
    )
}

fit_mean_score_ttest <- function(filename) {
  sim_data <- read_rds(filename)
  
  # Wrangle the data before fitting
  sim_data <- sim_data %>%
    mutate(
      data = map(
        data,
        ~{
          # Compute subject-level means
          group_by(., group, subject_id) %>%
            summarise(
              across(starts_with("score_discrete"), mean),
              .groups = "drop"
            ) %>%
            pivot_longer(cols = starts_with("score_discrete"),
                         values_to = "score_discrete") %>%
            separate(name, into = c("score_type", "score_dist"),
                     sep = "discrete_") %>%
            select(-score_type)
        } 
      )
    )
  
  sim_data %>%
    mutate(
      ttest_mean_score_norm = map(
        data,
        ~t.test(
          score_discrete ~ group,
          data = filter(., score_dist == "norm"),
        )
      ),
      ttest_mean_score_unif = map(
        data,
        ~t.test(
          score_discrete ~ group,
          data = filter(., score_dist == "unif")
        )
      ),
      ttest_mean_score_norm = map(
        ttest_mean_score_norm,
        ~broom::tidy(.) %>% rename_with(~str_c(.x, "_norm"))
      ),
      ttest_mean_score_unif = map(
        ttest_mean_score_unif,
        ~broom::tidy(.) %>% rename_with(~str_c(.x, "_unif"))
      )
    ) %>%
    select(-data) %>%
    unnest(c(ttest_mean_score_norm, ttest_mean_score_unif)) %>%
    select(-starts_with("method"), -starts_with("alternative")) %>%
    pivot_longer(
      cols = estimate_norm:conf.high_unif,
      names_to = "stat", values_to = "value"
    ) %>%
    separate(
      stat, into = c("stat", "score_dist"), sep = "_"
    ) %>%
    mutate(
      stat = recode(
        stat,
        estimate = "score_diff",
        conf.low = "score_diff_conf_low", conf.high = "score_diff_conf_high",
        estimate1 = "score_control", estimate2 = "score_treatment",
        statistic = "t", p.value = "p_value", parameter = "df"
      )
    )
}

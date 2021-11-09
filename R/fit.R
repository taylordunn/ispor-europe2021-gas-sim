#' Fit simulated T-scores with a t-test
#' 
#' Runs a t-test on mean T-scores between treatment and control groups.
#' Tidies the output (via `broom`), and also compute the standardized effect
#' size (Cohen's d).
#'
#' @param filename File path of a data frame of simulated GAS trial data in
#'  RDS format, as returned by the `simulate()` function.
#'
#' @return A data frame in long format, with `score_dist` `stat` and `value`
#'  variables.
fit_tscore_ttest <- function(filename) {
  sim_data <- read_rds(filename)
  
  sim_data %>%
    mutate(
      ttest_tscore_norm = map(
        data,
        ~t.test(
          tscore_norm ~ group,
          # Only analyzing the subject-level T-scores
          data = distinct(., subject_id, group, tscore_norm)
        ) %>%
          broom::tidy() %>%
          rename_with(~str_c(., "_norm"))
      ),
      ttest_tscore_unif = map(
        data,
        ~t.test(
          tscore_unif ~ group,
          data = distinct(., subject_id, group, tscore_unif)
        ) %>%
          broom::tidy() %>%
          rename_with(~str_c(., "_unif"))
      ),
      cohen.d_norm = map_dbl(
        data,
        ~distinct(., subject_id, group, tscore_norm) %>%
          summarise(
            tscore_treatment_norm = mean(tscore_norm[group == "treatment"]),
            tscore_control_norm = mean(tscore_norm[group == "control"]),
            # Difference in means divided by pooled standard deviation
            cohen.d_norm = (tscore_treatment_norm - tscore_control_norm) /
              sd(tscore_norm),
            .groups = "drop"
          ) %>%
          pull(cohen.d_norm)
      ),
      cohen.d_unif = map_dbl(
        data,
        ~distinct(., subject_id, group, tscore_unif) %>%
          summarise(
            tscore_treatment_unif = mean(tscore_unif[group == "treatment"]),
            tscore_control_unif = mean(tscore_unif[group == "control"]),
            # Difference in means divided by pooled standard deviation
            cohen.d_unif = (tscore_treatment_unif - tscore_control_unif) /
              sd(tscore_unif),
            .groups = "drop"
          ) %>%
          pull(cohen.d_unif)
      ),
    ) %>%
    select(-data) %>%
    unnest(c(ttest_tscore_norm, ttest_tscore_unif)) %>%
    # Drop unnecessary variables returned by t.test()
    select(-starts_with("method"), -starts_with("alternative")) %>%
    pivot_longer(cols = estimate_norm:cohen.d_unif,
                 names_to = "stat", values_to = "value") %>%
    separate(stat, into = c("stat", "score_dist"), sep = "_") %>%
    # Recode variable names to be more descriptive (and use snake case)
    mutate(
      stat = recode(
        stat,
        estimate = "tscore_diff",
        conf.low = "tscore_diff_conf_low", conf.high = "tscore_diff_conf_high",
        estimate1 = "tscore_control", estimate2 = "tscore_treatment",
        statistic = "t", p.value = "p_value", parameter = "df",
        cohen.d = "cohen_d"
      )
    )
}


fit_data <- function(filename) {
  sim_data <- read_rds(filename)
  
  sim_data %>%
    mutate(
      ttest_tscore = map(
        data,
        ~t.test(
          tscore ~ group,
          # We are only analyzing the subject-level T-scores
          data = distinct(., subject_id, group, tscore)
        )
      ),
      ttest_tscore = map(ttest_tscore, broom::tidy)
    ) %>%
    select(-data) %>%
    unnest(ttest_tscore) %>%
    rename(
      tscore_diff = estimate,
      tscore_control = estimate1, tscore_treatment = estimate2
    )
}


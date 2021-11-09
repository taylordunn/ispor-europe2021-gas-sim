plot_score_distribution_legend <- function(
  norm_color = "#E42BC4", unif_color = "#7E8CC5",
  save_dir = NULL, fig_width = 10, fig_height = 2, fig_dpi = 400
) {
  d <- tribble(
    ~score_discrete, ~perc_scores,
    -2, 0.2,
    -1, 0.2,
    0, 0.2,
    1, 0.2,
    2, 0.2
  ) %>%
    mutate(score_dist = "unif") %>%
    bind_rows(
      tribble(
        ~score_discrete, ~perc_scores,
        -2, 0.08,
        -1, 0.22,
        0, 0.4,
        1, 0.22,
        2, 0.08
      ) %>%
        mutate(score_dist = "norm")
    ) %>%
    mutate(
      score_discrete_label = factor(score_discrete, levels = -2:2,
                                    labels = c("-2", "-1", "0", "+1", "+2")),
      score_dist_label = factor(score_dist, levels = c("norm", "unif"),
                                labels = c("Normal", "Uniform"))
    )
    
  p_title <- glue(
    "<span style='color:{norm_color}'>",
    "Normally-distributed scores</span> vs ",
    "<span style='color:{unif_color}'>",
    "uniformly-distributed scores</span>"
  ) 
  p_norm <- d %>%
    filter(score_dist == "norm") %>%
    ggplot(aes(y = score_discrete_label, x = perc_scores,
               fill = score_dist)) +
    geom_col() +
    geom_text(aes(label = scales::percent(perc_scores, accuracy = 1)),
              hjust = 1, color = "white", size = 6) +
    scale_x_continuous(NULL, expand = expansion(mult = c(0, 0.1)),
                       limits = c(0, 0.45)) +
    scale_fill_manual(values = c(norm = norm_color, unif = unif_color)) +
    labs(y = NULL, x = NULL, title = p_title) +
    theme_ardea(base_size = 16, gridlines = "x") +
    theme(legend.position = "none",
          plot.title = element_markdown(),
          axis.ticks.x = element_blank(), axis.line.x = element_blank(),
          axis.text.x = element_blank(), panel.border = element_blank())
  p_unif <- p_norm %+%
    (d %>% filter(score_dist == "unif")) +
    theme(axis.text.y = element_blank()) +
    labs(title = NULL)
    
  p <- p_norm + p_unif
  
  
  if (is.null(save_dir)) save_dir <- here::here()
  save_dir_filename <- file.path(save_dir, "score-distribution-legend.png")
  ggsave(save_dir_filename, plot = p,
         width = fig_width, height = fig_height, dpi = fig_dpi)
  return(save_dir_filename)
}

plot_power_delta_n_goals <- function(
  power_df,
  plot_n_subjects = 60, # A single value of n_subjects per plot
  plot_delta = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1.1),
  #plot_n_goals = c(1, 2, 3),
  plot_n_goals = c(3, 4, 5),
  norm_color = "#E42BC4", unif_color = "#7E8CC5",
  save_dir = NULL, fig_width = 10, fig_height = 3.5, fig_dpi = 400
) {
  power_df <- power_df %>%
    filter(
      sim_n_subjects == plot_n_subjects,
      sim_delta %in% plot_delta,
      sim_n_goals %in% plot_n_goals
    ) %>%
    mutate(
      n_goals_label = glue("{sim_n_goals} goals per subject"),
      # Replace the plural "1 goals" with "1 goal"
      n_goals_label = str_replace(n_goals_label, "1 goals", "1 goal"),
      score_dist_label = factor(score_dist, levels = c("norm", "unif"),
                                labels = c("Normal", "Uniform"))
    )
  
  power_diff <- power_df %>%
    pivot_wider(names_from = score_dist, values_from = power) %>%
    mutate(power_diff = norm - unif)
  
  p_title <- glue(
    "Statistical power does not differ significantly between<br>",
    "<span style='color:{norm_color}'>",
    "normally-distributed</span> and ",
    "<span style='color:{unif_color}'>",
    "uniformly-distributed</span> scores"
  ) 
  
  p <- power_df %>%  
    ggplot(aes(x = sim_delta, y = power, color = score_dist_label)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 4, alpha = 0.6) +
    geom_hline(size = 1,yintercept = 0.8) +
    facet_wrap(~n_goals_label, nrow = 1) +
    scale_x_continuous("Simulated treatment effect size",
                       breaks = c(0.3, 0.5, 0.7, 0.9, 1.1)) +
    scale_y_continuous("Power",
                       breaks = c(0, 0.5, 0.8, 1.0), labels = scales::percent) +
    labs(
      color = "Score distribution",
      title = p_title,
      subtitle = glue(
        "Power vs simulated treatment effect for {plot_n_subjects} subjects"
      )
    ) +
    theme_ardea(base_size = 16) +
    scale_color_manual(values = c(norm_color, unif_color)) +
    theme(legend.position = "none",
          plot.title = element_markdown())
  
  if (is.null(save_dir)) save_dir <- here::here()
  save_dir_filename <-
    file.path(save_dir,
              glue("power_delta_n-goals_n-subjects{plot_n_subjects}.png"))
  ggsave(save_dir_filename, plot = p,
         width = fig_width, height = fig_height, dpi = fig_dpi)
  return(save_dir_filename)
}

plot_power_diff_grid <- function(
  power_df,
  plot_n_subjects = c(40, 50, 60, 70, 80),
  plot_delta = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1.1),
  plot_n_goals = c(3, 4, 5, 6),
  norm_color = "#E42BC4", unif_color = "#7E8CC5",
  save_dir = NULL, fig_width = 7, fig_height = 5, fig_dpi = 400
) {
  power_df <- power_df %>%
    filter(
      sim_n_subjects %in% plot_n_subjects,
      sim_delta %in% plot_delta,
      sim_n_goals %in% plot_n_goals
    ) %>%
    mutate(
      n_goals_label = glue("{sim_n_goals} goals per subject"),
      # Replace the plural "1 goals" with "1 goal"
      n_goals_label = str_replace(n_goals_label, "1 goals", "1 goal")
    )
  
  power_diff <- power_df %>%
    select(starts_with("sim"), n_goals_label, score_dist, power) %>%
    pivot_wider(names_from = score_dist, values_from = power) %>%
    mutate(power_diff = norm - unif) %>%
    mutate(
      power_diff_label = scales::percent(power_diff, accuracy = 0.1),
      power_label = glue(
        "{scales::percent(norm, accuracy = 1)} - ",
        "{scales::percent(unif, accuracy = 1)}"
      )
    )
  
  p_title <- glue(
    "<span style='color:white'>",
    "Difference in power (",
    "<span style='color:{norm_color}'>",
    "normal</span> - ",
    "<span style='color:{unif_color}'>",
    "uniform</span>)<br> ",
    "for all simulation scenarios",
    "</span>"
  )
   
  p <- power_diff %>%
    ggplot(aes(x = factor(sim_delta), y = factor(sim_n_subjects))) +
    geom_tile(aes(fill = power_diff)) +
    geom_text(aes(label = power_diff_label), color = "white") +
    facet_wrap(~n_goals_label, ncol = 2) +
    scale_x_discrete("Simulated treatment effect size", expand = c(0, 0)) +
    scale_y_discrete("Number of subjects", expand = c(0, 0)) +
    #scale_fill_viridis_c() +
    scale_fill_gradient(low = unif_color, high = norm_color) +
    labs(title = p_title) +
    theme_ardea(base_size = 16) +
    theme(
      legend.position = "none",
      plot.title = element_markdown(),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      axis.ticks = element_line(color = "white")
    )
  
  if (is.null(save_dir)) save_dir <- here::here()
  save_dir_filename <-
    file.path(save_dir, glue("power-diff-grid.png"))
  ggsave(save_dir_filename, plot = p,
         width = fig_width, height = fig_height, dpi = fig_dpi)
  return(save_dir_filename)
  
}

plot_effect_size_delta_n_goals <- function(
  effect_size_summary,
  plot_stat = "cohen_d",
  plot_n_subjects = 60, # A single value of n_subjects per plot
  plot_delta = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1.1),
  #plot_n_goals = c(1, 2, 3),
  plot_n_goals = c(3, 4, 5),
  norm_color = "#E42BC4", unif_color = "#7E8CC5",
  save_dir = NULL, fig_width = 10, fig_height = 3.5, fig_dpi = 400
) {
  effect_size_df <- effect_size_summary %>%
    filter(
      sim_n_subjects == plot_n_subjects,
      sim_delta %in% plot_delta,
      sim_n_goals %in% plot_n_goals
    ) %>%
    select(starts_with("sim"), norm_mean, norm_sd, unif_mean, unif_sd) %>%
    pivot_longer(cols = c(norm_mean, norm_sd, unif_mean, unif_sd)) %>%
    separate(name, into = c("score_dist", "stat")) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(
      n_goals_label = glue("{sim_n_goals} goals per subject"),
      # Replace the plural "1 goals" with "1 goal"
      n_goals_label = str_replace(n_goals_label, "1 goals", "1 goal"),
      score_dist_label = factor(score_dist, levels = c("norm", "unif"),
                                labels = c("Normal", "Uniform"))
    )
  
  p_title <- glue(
    "Measured effect sizes slightly higher with <br>",
    "<span style='color:{norm_color}'>",
    "normally-distributed</span> compared to ",
    "<span style='color:{unif_color}'>",
    "uniformly-distributed</span> scores"
  ) 
  
  p <- effect_size_df %>%  
    ggplot(aes(x = sim_delta, y = mean, color = score_dist_label)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 4, alpha = 0.6) +
    facet_wrap(~n_goals_label, nrow = 1) +
    scale_x_continuous("Simulated treatment effect size",
                       breaks = c(0.3, 0.5, 0.7, 0.9, 1.1)) +
    scale_y_continuous("Measured effect size") +
    labs(
      color = "Score distribution",
      title = p_title,
      subtitle = glue(
        "Simulated vs mean measured effect size for ",
        "{plot_n_subjects} subjects"
      )
    ) +
    theme_ardea(base_size = 16) +
    scale_color_manual(values = c(norm_color, unif_color)) +
    theme(legend.position = "none",
          plot.title = element_markdown())
  
  if (is.null(save_dir)) save_dir <- here::here()
  save_dir_filename <-
    file.path(save_dir,
              glue("effect-size_delta_n-goals_n-subjects{plot_n_subjects}.png"))
  ggsave(save_dir_filename, plot = p,
         width = fig_width, height = fig_height, dpi = fig_dpi)
  return(save_dir_filename)
}


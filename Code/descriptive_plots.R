# This script plots the trends in key characteristics over time, and the number
# of trials per sponsor over time (Figures 3.2 and 3.3).
 
analysis_ts <- combined_analysis_final %>%
  mutate(
    start_year = year(study_first_submitted_date),
    
    duration_days = as.numeric(completion_date - start_date),
    completed = if_else(overall_status == "COMPLETED", 1, 0),
    
    tc_group = case_when(
      str_detect(group, "Treatment") ~ "Treatment",
      str_detect(group, "Control")   ~ "Control"
    )
  )

# Calculate number of trials, median duration, completion rate, and median
# enrollment over time
yearly_summary <- analysis_ts %>%
  group_by(start_year, tc_group) %>%
  summarise(
    n_trials = n(),
    median_duration = median(duration_days, na.rm = TRUE),
    completed_pct = mean(completed, na.rm = TRUE),
    median_enroll   = median(enrollment, na.rm = TRUE),
    .groups = "drop"
  )

yearly_long <- yearly_summary %>%
  pivot_longer(
    cols = c(n_trials, median_duration, completed_pct, median_enroll),
    names_to  = "outcome",
    values_to = "value"
  ) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("n_trials", "median_duration", "completed_pct", "median_enroll"),
      labels = c(
        "Number of trials",
        "Median duration (days)",
        "Completion rate",
        "Median enrollment"
      )
    )
  )

base_theme <- theme_minimal(base_size = 12, base_family = "CMU Serif") +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 10,        
      hjust = 0,
      margin = margin(b = 6)
    ),
    legend.position = "none"
  )

# Plot of key trial characteristics over time
p_trials <- ggplot(yearly_summary, aes(start_year, n_trials, color = tc_group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = NULL, color = "", title = "Number of trials") +
  scale_color_manual(values = c("Treatment" = "#5E2B97", "Control"   = "#D67CA2")) +
  base_theme

p_dur <- ggplot(yearly_summary, aes(start_year, median_duration, color = tc_group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = NULL, color = "", title = "Median duration (days)") +
  scale_color_manual(values = c("Treatment" = "#5E2B97", "Control"   = "#D67CA2")) +
  base_theme

p_comp <- ggplot(yearly_summary, aes(start_year, completed_pct, color = tc_group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Year", y = NULL, color = "", title = "Completion rate") +
  scale_color_manual(values = c("Treatment" = "#5E2B97", "Control"   = "#D67CA2")) +
  base_theme

p_enr <- ggplot(yearly_summary, aes(start_year, median_enroll, color = tc_group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = NULL, color = "", title = "Median enrollment") +
  scale_color_manual(values = c("Treatment" = "#5E2B97", "Control"   = "#D67CA2")) +
  base_theme

((p_trials + p_dur) / (p_comp + p_enr)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Key Trial Characteristics Over Time",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 14,             
        family = "CMU Serif",
        hjust = 0,
        margin = margin(b = 12)
      )
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Number of trials per sponsor-year
sponsor_yearly <- combined_analysis_final %>%
  filter(!is.na(lead_name)) %>%
  mutate(start_year = lubridate::year(study_first_submitted_date)) %>%
  count(lead_name, start_year, name = "n_trials")

# Avg number of trials per sponsor-year 
sponsor_year_summary <- sponsor_yearly %>%
  group_by(start_year) %>%
  summarise(
    mean_trials   = mean(n_trials),
    median_trials = median(n_trials),
    p75_trials    = quantile(n_trials, 0.75),
    .groups = "drop"
  )
ggplot(sponsor_year_summary,
       aes(x = start_year, y = mean_trials)) +
  geom_line(linewidth = 1, color = "#5E2B97") +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Average trials per sponsor",
    title = "Number of Trials Started per Sponsor per Year"
  ) +
  theme_minimal(base_size = 12, base_family = "CMU Serif") +
  theme(plot.title = element_text(face = "bold", hjust = 0))

year_range <- range(sponsor_yearly$start_year, na.rm = TRUE)
sponsor_yearly_full <- sponsor_yearly %>%
  group_by(lead_name) %>%
  complete(start_year = seq(year_range[1], year_range[2], by = 1),
           fill = list(n_trials = 0)) %>%
  ungroup()

ggplot(sponsor_yearly_full,
       aes(x = start_year, y = n_trials, group = lead_name)) +
  # Trend for each individual sponsor 
  geom_line(
    alpha = 0.6,
    linewidth = 0.5,
    color = "#FFB6C1"   
  ) +
  # Mean across all sponsors 
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.3,
    color = "#5E2B97"   
  ) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "grey50") +
  labs(
    x = "Year",
    y = "Number of trials started",
    title = "Number of Trials Started per Sponsor per Year"
  ) +
  theme_minimal(base_size = 12, base_family = "CMU Serif") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0)
  )
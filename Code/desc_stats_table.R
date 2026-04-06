# This script calculates and creates the table of summary statistics 
# for trial-level characteristics by treatment status and period.

# Combine treatment & control datasets
combined_analysis_df <- bind_rows(
  treatment_df %>% mutate(tc_group = "Treatment"),
  control_df   %>% mutate(tc_group = "Control")
) %>%
  mutate(
    period = if_else(period == "Pre", "Pre-2017", "Post-2017"),
    group  = paste(tc_group, period)
  )

# Add relevant variables from studies dataset
studies_add <- studies %>%
  transmute(
    nct_id = as.character(nct_id),
    start_date      = ymd(start_date),
    enrollment      = as.numeric(enrollment),
    overall_status  = toupper(str_trim(overall_status)),
    number_of_arms  = as.numeric(number_of_arms)
  )

combined_analysis_df2 <- combined_analysis_df %>%
  mutate(nct_id = as.character(nct_id)) %>%
  left_join(studies_add, by = "nct_id")

# Add relevant variables from interventions dataset
interv_flags <- interventions %>%
  transmute(
    nct_id = as.character(nct_id),
    intervention_type = toupper(str_trim(intervention_type))
  ) %>%
  filter(intervention_type %in% c("DRUG", "DEVICE", "BIOLOGICAL")) %>%
  group_by(nct_id) %>%
  summarise(
    has_drug = any(intervention_type == "DRUG"),
    has_device = any(intervention_type == "DEVICE"),
    has_biological = any(intervention_type == "BIOLOGICAL"),
    .groups = "drop"
  )

combined_analysis_final <- combined_analysis_df2 %>%
  left_join(interv_flags, by = "nct_id") %>%
  mutate(
    has_drug   = replace_na(has_drug, FALSE),
    has_device = replace_na(has_device, FALSE),
    has_biological    = replace_na(has_biological, FALSE)
  )

# Helper function to calculate mean, SD, median and IQR for numeric variables
mean_median_stats <- function(x, digits = 2) {
  m  <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  tibble(
    stat = c("Mean (SD)", "Median (IQR)"),
    value = c(
      sprintf(paste0("%.", digits, "f (%.", digits, "f)"), m, s),
      sprintf(paste0("%.", digits, "f (%.", digits, "f)"), med, q3 - q1)
    )
  )
}

# Helper function to calculate percentages for categorical variables
prop_pct <- function(x) sprintf("%.1f\\%%", 100 * mean(x, na.rm = TRUE))

desc_df <- combined_analysis_final %>%
  mutate(
    us_site = as.integer(any_us),
    
    duration_days = as.numeric(completion_date - start_date),
    log_duration = log(duration_days), 
    
    completed = as.integer(overall_status == "COMPLETED"),  
    withdrawn = as.integer(overall_status == "WITHDRAWN"),
    terminated = as.integer(overall_status == "TERMINATED")
    
  )

# Create the table of descriptive statistics (Table 3.2)
desc_table <- desc_df %>%
  group_by(group) %>%
  summarise(
    `N trials`            = n(),
    `US sites (%)`        = prop_pct(us_site),
    `Duration (days)`     = mean_median_stats(duration_days),
    `Completed (%)`       = prop_pct(completed),
    `Terminated (%)`      = prop_pct(terminated),
    `Withdrawn (%)`       = prop_pct(withdrawn),
    `Enrollment`          = mean_median_stats(enrollment),
    `Number of arms`      = mean_median_stats(number_of_arms),
    `Drug (%)`            = prop_pct(has_drug),
    `Device (%)`          = prop_pct(has_device),
    `Biological (%)`      = prop_pct(has_biological),
    `Early Phase 1 (%)`   = prop_pct(phase == "EARLY_PHASE1"),
    `Phase 1 (%)`         = prop_pct(phase == "PHASE1"),
    `Phase 1/2 (%)`       = prop_pct(phase == "PHASE1/PHASE2"),
    `Phase 2 (%)`         = prop_pct(phase == "PHASE2"),
    `Phase 2/3 (%)`       = prop_pct(phase == "PHASE2/PHASE3"),
    `Phase 3 (%)`         = prop_pct(phase == "PHASE3"),
    `Phase 4 (%)`         = prop_pct(phase == "PHASE4"),
    .groups = "drop"
  ) %>%
  mutate(
    group = factor(group, levels = c(
      "Treatment Pre-2017", "Treatment Post-2017",
      "Control Pre-2017",   "Control Post-2017"
    ))
  ) %>%
  arrange(group)

desc_table
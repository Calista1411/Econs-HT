# This script runs the robustness checks and extensions.

# Placebo policy test for trial-level outcomes
did_trials_placebo <- did_trials %>%
  mutate(placebo_post = as.integer(year >= 2014))

did_duration_placebo <- feols(
  log_duration ~ 
    treat_flag * placebo_post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials_placebo,
  cluster = ~lead_name
)
etable(did_duration_placebo)

did_completed_placebo <- feols(
  completed ~ 
    treat_flag * placebo_post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials_placebo,
  cluster = ~lead_name
)
etable(did_completed_placebo)

did_enrollment_placebo <- feols(
  log_enrollment ~ 
    treat_flag * placebo_post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials_placebo,
  cluster = ~lead_name
)
etable(did_enrollment_placebo)

# Placebo test for sponsor-level outcome (# of trials per sponsor-year)
sponsor_year_placebo <- sponsor_year %>%
  mutate(
    placebo_post = as.integer(year >= 2014)
  )

did_placebo_N <- feols(
  N_trials ~
    treat_flag * placebo_post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 |
    lead_name + year,
  data    = sponsor_year_placebo,
  cluster = ~lead_name
)

etable(did_placebo_N)

# Drop transition year (2016)
did_trials_no2016 <- did_trials %>%
  filter(year != 2016)

# DiD for trial-level outcomes
did_duration_no2016 <- feols(
  log_duration ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials_no2016,
  cluster = ~lead_name
)
etable(did_duration_no2016)

did_duration_sponsortrend_no2016 <- feols(
  log_duration ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year], # sponsor trends
  data    = did_trials_no2016,
  cluster = ~lead_name
)
etable(did_duration_sponsortrend_no2016)

did_completed_no2016 <- feols(
  completed ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials_no2016,
  cluster = ~lead_name
)
etable(did_completed_no2016)

did_completed_sponsortrend_no2016 <- feols(
  completed ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year],
  data    = did_trials_no2016,
  cluster = ~lead_name
)
etable(did_completed_sponsortrend_no2016)

did_enrollment_no2016 <- feols(
  log_enrollment ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials_no2016,
  cluster = ~lead_name
)
etable(did_enrollment_no2016)

did_enrollment_sponsortrend_no2016 <- feols(
  log_enrollment ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year],
  data    = did_trials_no2016,
  cluster = ~lead_name
)
etable(did_enrollment_sponsortrend_no2016)

# DiD for sponsor-level outcome (# of trials per sponsor-year)
sponsor_year_no2016 <- sponsor_year %>%
  filter(year != 2016)

did_N_no2016 <- feols(
  N_trials ~
    treat_flag * post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 |
    lead_name + year,
  data    = sponsor_year_no2016,
  cluster = ~lead_name
)
etable(did_N_no2016)

did_N_trend_no2016 <- feols(
  N_trials ~
    treat_flag * post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 |
    lead_name + year + lead_name[year],
  data    = sponsor_year_no2016,
  cluster = ~lead_name
)
etable(did_N_trend_no2016)

# Alternative treatment assignment: 
# only include trials conducted exclusively in the U.S. in the treatment group

# Define flags for control and treatment group 
analysis_trials <- analysis_trials %>%
  mutate(
    treat_flag   = (fda_status == "FDA regulated")     & country_group %in% "US only",
    control_flag = (fda_status == "Not FDA regulated") & country_group == "Non-US only"
  )

# Treatment group: FDA regulated AND only U.S. sites (5,686 obs)
treatment_df <- analysis_trials %>% filter(treat_flag) 

# Control group: Not FDA regulated AND international sites only (no U.S. sites)
control_df   <- analysis_trials %>% filter(control_flag)

did_trials <- did_trials %>%
  mutate(
    treat_us_only = treat_flag & (country_group == "US only"),
    treat_mixed   = treat_flag & (country_group == "US + International")
  )

# DiD for trial-level outcomes
did_duration_hetero <- feols(
  log_duration ~
    treat_us_only * post +
    treat_mixed   * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)
etable(did_duration_hetero)

did_duration_hetero_sponsortrend <- feols(
  log_duration ~
    treat_us_only * post +
    treat_mixed   * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year], # sponsor trends
  data    = did_trials,
  cluster = ~lead_name
)
etable(did_duration_hetero_sponsortrend)

did_completed_hetero <- feols(
  completed ~
    treat_us_only * post +
    treat_mixed   * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)
etable(did_completed_hetero)

did_completed_hetero_sponsortrend <- feols(
  completed ~
    treat_us_only * post +
    treat_mixed   * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year +lead_name[year],
  data    = did_trials,
  cluster = ~lead_name
)
etable(did_completed_hetero_sponsortrend)

did_enrollment_hetero <- feols(
  log_enrollment ~
    treat_us_only * post +
    treat_mixed   * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)
etable(did_enrollment_hetero)

did_enrollment_hetero_sponsortrend <- feols(
  log_enrollment ~
    treat_us_only * post +
    treat_mixed   * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year],
  data    = did_trials,
  cluster = ~lead_name
)
etable(did_enrollment_hetero_sponsortrend)

# DiD for sponsor-level outcome (# of trials per sponsor-year)
sponsor_year <- trial_level %>%
  group_by(lead_name, year) %>%
  summarise(
    N_trials = n(),
    
    share_us_only = mean(country_group == "US only", na.rm = TRUE),
    share_mixed   = mean(country_group == "US + International", na.rm = TRUE),
    
    mean_arms       = mean(number_of_arms, na.rm = TRUE),
    mean_pct_us     = mean(pct_us_sites, na.rm = TRUE),
    
    share_phase1 = mean(phase == "PHASE1", na.rm = TRUE),
    share_phase2 = mean(phase == "PHASE2", na.rm = TRUE),
    share_phase3 = mean(phase == "PHASE3", na.rm = TRUE),
    
    share_device      = mean(has_device, na.rm = TRUE),
    share_biological  = mean(has_biological, na.rm = TRUE),
    
    treat_flag = first(treat_flag),
    .groups = "drop"
  )

did_N_hetero <- feols(
  N_trials ~
    share_mixed   * post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 |
    lead_name + year,
  data    = sponsor_year,
  cluster = ~lead_name
)
etable(did_N_hetero)

did_N_hetero_sponsortrend <- feols(
  N_trials ~
    share_mixed   * post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 |
    lead_name + year +lead_name[year],
  data    = sponsor_year,
  cluster = ~lead_name
)
etable(did_N_hetero_sponsortrend)
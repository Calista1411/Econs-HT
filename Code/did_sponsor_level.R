# This script runs parallel trends tests and DiD regressions on the sponsor-
# level outcome, the number of trials per sponsor-year.

trial_level <- combined_analysis_final %>%
  mutate(
    year = year(study_first_submitted_date)
  )

sponsor_year <- trial_level %>%
  group_by(lead_name, year) %>%
  summarise(
    N_trials = n(),
    
    # Average number of arms and % of US sites
    mean_arms       = mean(number_of_arms, na.rm = TRUE),
    mean_pct_us     = mean(pct_us_sites, na.rm = TRUE),
    
    # % of sponsor's trials of each phase
    share_phase1 = mean(phase == "PHASE1", na.rm = TRUE),
    share_phase2 = mean(phase == "PHASE2", na.rm = TRUE),
    share_phase3 = mean(phase == "PHASE3", na.rm = TRUE),
    
    # % of sponsor's trials of each intervention type
    share_device      = mean(has_device, na.rm = TRUE),
    share_biological  = mean(has_biological, na.rm = TRUE),
    
    treat_flag = first(treat_flag)
  ) %>%
  ungroup()

all_years <- seq(min(sponsor_year$year), max(sponsor_year$year))

# Fill in missing sponsor-year cells with zeros
sponsor_year <- sponsor_year %>%
  complete(lead_name, year = all_years) %>%
  arrange(lead_name, year) %>%
  group_by(lead_name) %>%
  mutate(
    N_trials = replace_na(N_trials, 0),
    treat_flag = first(na.omit(treat_flag))
  ) %>%
  ungroup()

sponsor_year <- sponsor_year %>%
  mutate(
    mean_arms = replace_na(mean_arms, 0),
    mean_pct_us = replace_na(mean_pct_us, 0),
    share_phase1 = replace_na(share_phase1, 0),
    share_phase2 = replace_na(share_phase2, 0),
    share_phase3 = replace_na(share_phase3, 0),
    share_device = replace_na(share_device, 0),
    share_biological = replace_na(share_biological, 0)
  )

# Indicator for post-2017 period
sponsor_year <- sponsor_year %>%
  mutate(
    post = as.integer(year >= 2017)
  )

# Parallel trends test (Figures 4.1 and 4.2)
did_es_ctrl_N <- feols(
  N_trials ~
    i(year, treat_flag, ref = 2016) +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 +
    share_device +
    share_biological |
    lead_name + year,
  data    = sponsor_year,
  cluster = ~lead_name
)

par(
  family = "CMU Serif",
  font   = 1
)

iplot(
  did_es_ctrl_N,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study Parallel Trends Test",
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_es_ctrl_N,
  keep = "year::(2013|2014|2015):treat_flag"
)

did_es_trend_N <- feols(
  N_trials ~
    i(year, treat_flag, ref = 2016) +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 +
    share_device +
    share_biological |
    lead_name + year + lead_name[year], # sponsor trends
  data    = sponsor_year,
  cluster = ~lead_name
)

par(
  family = "CMU Serif",
  font   = 1
)

iplot(
  did_es_trend_N,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study with Sponsor-specific Trends",
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_es_trend_N,
  keep = "year::(2013|2014|2015):treat_flag"
)

# Baseline DiD 
did_N_ctrl <- feols(
  N_trials ~
    treat_flag * post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 +
    share_device +
    share_biological |
    lead_name + year,
  data    = sponsor_year,
  cluster = ~lead_name
)

etable(did_N_ctrl)

# DiD with sponsor-specific trends
did_N_trend <- feols(
  N_trials ~
    treat_flag * post +
    mean_arms +
    mean_pct_us +
    share_phase1 +
    share_phase2 +
    share_phase3 +
    share_device +
    share_biological |
    lead_name + year + lead_name[year],
  data    = sponsor_year,
  cluster = ~lead_name
)

etable(did_N_trend)
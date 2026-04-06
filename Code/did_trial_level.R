# This script runs parallel trends tests (Figures 4.1 and 4.2) and DiD 
# regressions on trial-level outcomes (trial duration, enrollment, and completion).

did_trials <- combined_analysis_final %>%
  mutate(
    year = year(study_first_submitted_date),
    post = as.integer(year >= 2017)
  )

# Create the variables log(Duration), log(Enrollment), and Completed 
did_trials <- did_trials %>%
  mutate(
    duration_days  = as.numeric(completion_date - start_date),
    log_duration   = log1p(duration_days), 
    log_enrollment = log1p(enrollment),
    completed      = as.integer(overall_status == "COMPLETED")
  )

# Parallel trends tests (log(Duration))
did_es_ctrl_duration <- feols(
  log_duration ~
    i(year, treat_flag, ref = 2016) +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)

par(
  family = "CMU Serif",
  font   = 1
)

iplot(
  did_es_ctrl_duration,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study Parallel Trends Test",
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_es_ctrl_duration,
  keep = "year::(2013|2014|2015):treat_flag"
)

did_trend_ctrl_duration <- feols(
  log_duration ~
    i(year, treat_flag, ref = 2016) +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year], # sponsor trends
  data    = did_trials,
  cluster = ~lead_name
)

iplot(
  did_trend_ctrl_duration,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study with Sponsor-specific Trends", 
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_trend_ctrl_duration,
  keep = "year::(2013|2014|2015):treat_flag"
)

# Parallel trends tests (Completed)
did_es_ctrl_completed <- feols(
  completed ~
    i(year, treat_flag, ref = 2016) +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)

par(
  family = "CMU Serif",
  font   = 1
)

iplot(
  did_es_ctrl_completed,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study Parallel Trends Test",
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_es_ctrl_completed,
  keep = "year::(2013|2014|2015):treat_flag"
)

did_trend_ctrl_completed <- feols(
  completed ~
    i(year, treat_flag, ref = 2016) +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year],
  data    = did_trials,
  cluster = ~lead_name
)

iplot(
  did_trend_ctrl_completed,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study with Sponsor-specific Trends", 
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_trend_ctrl_completed,
  keep = "year::(2013|2014|2015):treat_flag"
)

# Parallel trends tests (log(Enrollment))
did_es_ctrl_enrollment <- feols(
  log_enrollment ~
    i(year, treat_flag, ref = 2016) +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)

par(
  family = "CMU Serif",
  font   = 1
)

iplot(
  did_es_ctrl_enrollment,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study Parallel Trends Test",
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_es_ctrl_enrollment,
  keep = "year::(2013|2014|2015):treat_flag"
)

did_trend_ctrl_enrollment <- feols(
  log_enrollment ~
    i(year, treat_flag, ref = 2016) +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year],
  data    = did_trials,
  cluster = ~lead_name
)

iplot(
  did_trend_ctrl_enrollment,
  ref.line = 0,
  xlab = "Year",
  ylab = "Treatment Effect Relative to 2016",
  main = "Event-study with Sponsor-specific Trends", 
  col    = "#5E2B97",
  pt.col = "#5E2B97",
  ci.col = "#5E2B97"
)

wald(
  did_trend_ctrl_enrollment,
  keep = "year::(2013|2014|2015):treat_flag"
)

# DiD for log(Duration)
did_duration_ctrl <- feols(
  log_duration ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)

etable(did_duration_ctrl)

did_duration_trend <- feols(
  log_duration ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year], # sponsor trends
  data    = did_trials,
  cluster = ~lead_name
)

etable(did_duration_trend)

# DiD for Completed
did_completion_ctrl <- feols(
  completed ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)

etable(did_completion_ctrl)

did_completion_trend <- feols(
  completed ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year + lead_name[year],
  data    = did_trials,
  cluster = ~lead_name
)

etable(did_completion_trend)

# DiD for log(Enrollment)
did_enrollment_ctrl <- feols(
  log_enrollment ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year,
  data    = did_trials,
  cluster = ~lead_name
)

etable(did_enrollment_ctrl)

did_enrollment_trend <- feols(
  log_enrollment ~ 
    treat_flag * post +
    pct_us_sites +
    number_of_arms +
    phase +
    has_device +
    has_biological |
    lead_name + year +lead_name[year],
  data    = did_trials,
  cluster = ~lead_name
)

etable(did_enrollment_trend)
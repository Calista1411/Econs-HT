# This script constructs the treatment and control group samples based on the 
# inclusion criteria and identification strategy described in the thesis.

# Helper function to track counts after each step
count_step <- function(df, step, id = "nct_id") {
  tibble(
    step = step,
    n_rows = nrow(df),
    n_trials = n_distinct(df[[id]])
  )
}

# Merge data from studies, lead sponsors, U.S. flags, and share of U.S. sites
base_trials <- studies %>%
  transmute(
    nct_id,
    study_type,
    is_fda_regulated_drug,
    is_fda_regulated_device,
    study_first_submitted_date = ymd(study_first_submitted_date),
    phase,
    completion_date_type,              
    completion_date = ymd(completion_date) 
  ) %>%
  left_join(lead_sponsors, by = "nct_id") %>%
  left_join(us_flags, by = "nct_id") %>%
  left_join(us_share, by = "nct_id") %>%
  mutate(
    any_us        = replace_na(any_us,  FALSE),
    all_us        = replace_na(all_us,  FALSE),
    us_only       = replace_na(us_only, FALSE),
    pct_us_sites  = replace_na(pct_us_sites, 0)
  )

# Filter interventions dataset to only include drug/device/biological trials 
trial_intervention <- interventions %>%
  transmute(
    nct_id,
    intervention_type = str_to_upper(str_trim(intervention_type))
  ) %>%
  filter(intervention_type %in% c("DRUG","DEVICE","BIOLOGICAL")) %>%
  distinct(nct_id)  

# Merge with interventions dataset: 546,474 obs.
base_trials <- base_trials %>%
  mutate(nct_id = as.character(nct_id)) %>%
  left_join(trial_intervention %>% mutate(flag_ddb = TRUE), by = "nct_id") %>%
  mutate(flag_ddb = replace_na(flag_ddb, FALSE))

flow <- count_step(base_trials, "Start: studies + lead + flags")

# (1) Drop trials with missing country info: 496,729 obs.
step1 <- base_trials %>%
  semi_join(countries %>% distinct(nct_id), by = "nct_id")
flow <- bind_rows(flow, count_step(step1, "Drop missing country info"))

# (2) Only interventional studies: 384,106 obs.
step2 <- step1 %>%
  filter(study_type == "INTERVENTIONAL")
flow <- bind_rows(flow, count_step(step2, "Only interventional"))

# (3) Only drug/device/biological: 240,093 obs.
step3 <- step2 %>%
  filter(flag_ddb)
flow <- bind_rows(flow, count_step(step3, "Only Drug/Device/Biological"))

# (4) Only completion_date_type == ACTUAL: 154,378 obs.
step4 <- step3 %>%
  filter(completion_date_type == "ACTUAL")
flow <- bind_rows(flow, count_step(step4, "Only completion_date_type == ACTUAL"))

# (5) Drop trials with missing FDA regulation status: 62,787 obs.
step5 <- step4 %>%
  filter(is_fda_regulated_drug %in% c("t","f"),
         is_fda_regulated_device %in% c("t","f"))
flow <- bind_rows(flow, count_step(step5, "Drop missing FDA regulation status"))

# (6) Drop trials with missing phase info: 43,260 obs. 
step6 <- step5 %>%
  mutate(phase = toupper(str_trim(phase))) %>%
  filter(!is.na(phase), phase != "", phase != "NA")
flow <- bind_rows(flow, count_step(step6, "Drop missing phase"))

# (7) Drop trials registered on/after 2021-01-01 
# (avoid short duration trials after 2017): 30,345 obs.
step7 <- step6 %>%
  filter(study_first_submitted_date < ymd("2021-01-01"))
flow <- bind_rows(flow, count_step(step7, "Drop trials registered on/after 2021-01-01"))

# (8) Drop trials registered before 2013-01-01 
# (to make the data symmetric pre- and post-policy): 27,670 obs.
step8 <- step7 %>%
  filter(study_first_submitted_date >= ymd("2013-01-01"))

flow <- bind_rows(flow, count_step(step8, "Drop trials registered before 2013-01-01"))

# (9) Restrict to only sponsors with at least 1 trial before & after 2017: 17,139 obs.
active_leads_1 <- step8 %>%
  filter(!is.na(lead_name)) %>%
  mutate(
    period = case_when(
      study_first_submitted_date <  ymd("2017-01-01") ~ "Pre",
      study_first_submitted_date >= ymd("2017-01-01") ~ "Post",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(nct_id, lead_name, period) %>%
  count(lead_name, period) %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  filter(Pre > 0 & Post > 0) %>%
  pull(lead_name)

step9 <- step8 %>%
  filter(lead_name %in% active_leads_1)

flow <- bind_rows(flow, count_step(step9, "Drop sponsors not active both before & after 2017 (>=1 each)"))

# Diagnostics to decide on the threshold for the min. number of trials per sponsor
# (Figure 3.1)
sponsor_trial_counts <- step9 %>%
  filter(!is.na(lead_name)) %>%
  mutate(period = if_else(study_first_submitted_date >= ymd("2017-01-01"), 
                          "Post", "Pre")) %>%
  distinct(nct_id, lead_name, period) %>%
  count(lead_name, period, name = "n_trials") %>%
  pivot_wider(names_from = period, values_from = n_trials, values_fill = 0) %>%
  mutate(
    min_trials = pmin(Pre, Post),
    total_trials = Pre + Post
  )

ggplot(sponsor_trial_counts, aes(x = min_trials)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white") +
  labs(
    x = "Minimum number of trials per sponsor (min[Pre, Post])",
    y = "Number of sponsors",
    title = "Sponsor activity around 2017 cutoff"
  )

cutoff_table <- tibble(cutoff = 1:20) %>%
  rowwise() %>%
  mutate(
    n_sponsors = sum(sponsor_trial_counts$min_trials >= cutoff)
  ) %>%
  ungroup()

ggplot(cutoff_table, aes(x = cutoff, y = n_sponsors)) +
  geom_line(linewidth = 1.1, colour = "#FFB6C1") +
  geom_point(size = 2.2, colour = "#FFB6C1") +
  scale_x_continuous(breaks = 1:20) +
  labs(
    title = "Sponsor Eligibility by Trial Activity",
    x = "Minimum number of trials pre- and post-2017",
    y = "Number of lead sponsors"
  ) +
  theme_minimal(base_size = 12, base_family = "CMU Serif") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0,
      margin = margin(b = 12)
    ),
    plot.margin = margin(t = 14, r = 12, b = 10, l = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

# (10) Restrict to sponsors with at least 5 trials before and after 2017: 10,737 obs.

per_period_min <- 5

active_leads_5 <- step9 %>%
  filter(!is.na(lead_name)) %>%
  mutate(
    period = case_when(
      study_first_submitted_date <  ymd("2017-01-01") ~ "Pre",
        study_first_submitted_date >=  ymd("2017-01-01") ~ "Post",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(nct_id, lead_name, period) %>%
  count(lead_name, period) %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  filter(Pre >= per_period_min & Post >= per_period_min) %>%
  pull(lead_name)

step10 <- step9 %>%
  filter(lead_name %in% active_leads_5)

flow <- bind_rows(flow, count_step(step10, "Restrict to sponsors 
                                   with >=5 trials in Pre and Post"))

# Display counts
flow <- flow %>%
  mutate(
    removed_trials = lag(n_trials) - n_trials,
    removed_rows   = lag(n_rows) - n_rows
  )

flow

# Final analysis sample
analysis_trials <- step10

# Define indicator variables for FDA status, country group and period
analysis_trials <- analysis_trials %>%
  mutate(
    period = if_else(study_first_submitted_date >= ymd("2017-01-01"), "Post", "Pre"),
    
    fda_status = case_when(
      is_fda_regulated_drug == "t" | is_fda_regulated_device == "t" ~ "FDA regulated",
      is_fda_regulated_drug == "f" & is_fda_regulated_device == "f" ~ "Not FDA regulated"
    ),
    
    country_group = case_when(
      any_us & n_countries == 1 ~ "US only",
      any_us & n_countries >  1 ~ "US + International",
      !any_us & n_countries >= 1 ~ "Non-US only"
    )
  )

# Define flags for control and treatment group 
analysis_trials <- analysis_trials %>%
  mutate(
    treat_flag   = (fda_status == "FDA regulated")     & country_group %in% c("US only", "US + International"),
    control_flag = (fda_status == "Not FDA regulated") & country_group == "Non-US only"
  )

# Treatment group: FDA regulated AND have U.S. sites
treatment_df <- analysis_trials %>% filter(treat_flag)

# Control group: Not FDA regulated AND international sites only (no U.S. sites)
control_df   <- analysis_trials %>% filter(control_flag)

# Check the trials not included in either groups
analysis_trials %>%
  mutate(
    fda = if_else(is_fda_regulated_drug == "t" | is_fda_regulated_device == "t",
                      "FDA regulated", "Not FDA regulated"),
    us  = if_else(any_us, "Has US site", "Non-US only")
  ) %>%
  count(fda, us) %>%
  tidyr::pivot_wider(names_from = us, values_from = n, values_fill = 0)
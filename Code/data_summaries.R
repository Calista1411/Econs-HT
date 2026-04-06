# This script computes summary statistics of the data used in deciding on the
# identification strategy.

# Number of trials by start year
trials_per_year <- studies %>%
  filter(!is.na(start_date)) %>%
  mutate(start_year = year(start_date)) %>%
  group_by(start_year) %>%
  summarise(num_trials = n()) %>%
  arrange(start_year) 

# Number of completed trials by year of completion
completed_trials_by_year <- studies %>%
  # Keep only trials with completion date and overall_status = "COMPLETED"
  filter(!is.na(completion_date), overall_status == "COMPLETED") %>%
  mutate(completion_year = year(completion_date)) %>%
  group_by(completion_year) %>%
  summarise(num_completed_trials = n()) %>%
  arrange(completion_year)

# Mean & median duration by completion status
duration_by_trial <- studies %>%
  group_by(overall_status) %>%
  summarise(
    mean_duration = mean(duration_days, na.rm = TRUE),
    median_duration = median(duration_days, na.rm = TRUE),
    n = n()
  )

# Trial counts by study type (interventional/observational/EA)
studies %>%
  count(study_type) %>%
  arrange(desc(n))

# Trial counts by intervention type (drug/device/biological/...)
interventions %>%
  count(intervention_type) %>%
  arrange(desc(n))

# Trial counts by FDA regulation status
studies %>%
  count(is_fda_regulated_drug, is_fda_regulated_device)

# ----- Run this only after sample_construction.R -----
# FDA regulation flag (for both drug & device combined)
fda_status_studies <- analysis_trials %>%
  mutate(
    fda_status = case_when(
      is_fda_regulated_drug == "t" | is_fda_regulated_device == "t" ~ "FDA regulated",
      is_fda_regulated_drug == "f" & is_fda_regulated_device == "f" ~ "Not FDA regulated"
    )
  )

fda_share_tbl <- function(fda_status_studies, by = NULL) {
  
  fda_status_studies <- fda_status_studies %>%
    filter(!is.na(fda_status))
  
  if (is.null(by)) {
    out <- fda_status_studies %>%
      summarise(
        n_total = n(),
        n_fda   = sum(fda_status == "FDA regulated"),
        FDA_share = n_fda / n_total,
        FDA_share_pct = scales::percent(FDA_share)
      )
    return(out)
  }
  
  fda_status_studies %>%
    group_by(across(all_of(by))) %>%
    summarise(
      n_total = n(),
      n_fda   = sum(fda_status == "FDA regulated"),
      .groups = "drop"
    ) %>%
    mutate(
      FDA_share = n_fda / n_total,
      FDA_share_pct = scales::percent(FDA_share)
    ) %>%
    arrange(desc(FDA_share))
}

# Share of FDA regulation status by country/phase/lead sponsor class
fda_share_tbl(fda_status_studies, by = "country_group")
fda_share_tbl(fda_status_studies, by = "phase")
fda_share_tbl(fda_status_studies, by = "lead_agency_class")
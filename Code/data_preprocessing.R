# This script reads in the raw data files downloaded from the AACT database, 
# and conducts preliminary data pre-processing.

# Read in data files
studies <- read.delim("~/Desktop/econs honours thesis/data_txt/studies.txt", 
                      sep = "|", stringsAsFactors = FALSE)
sponsors <- read.delim("~/Desktop/econs honours thesis/data_txt/sponsors.txt", 
                       sep = "|", stringsAsFactors = FALSE)
interventions <- read.delim("~/Desktop/econs honours thesis/data_txt/interventions.txt", 
                            sep = "|", stringsAsFactors = FALSE)
countries <- read.delim("~/Desktop/econs honours thesis/data_txt/countries.txt", 
                        sep = "|", stringsAsFactors = FALSE)

# Convert dates to Date format
studies$start_date <- as.Date(studies$start_date)
studies$completion_date <- as.Date(studies$completion_date)

# Create trial duration in days
studies <- studies %>%
  mutate(duration_days = as.numeric(completion_date - start_date))

# Filter sponsor dataset to only keep lead sponsors
lead_sponsors <- sponsors %>%
  filter(lead_or_collaborator == "lead") %>%
  transmute(
    nct_id,
    lead_name         = name,
    lead_agency_class = agency_class
  )

# Create indicator variables for U.S. sites
us_flags <- countries %>%
  group_by(nct_id) %>%
  summarise(
    any_us       = any(name == "United States"),          
    all_us       = all(name == "United States"),
    n_countries  = n_distinct(name),                      
    us_only      = n_countries == 1 & any_us,                
    .groups = "drop"
  )

# Calculate the % of U.S. sites for each trial
us_share <- countries %>%
  group_by(nct_id) %>%
  summarise(
    n_sites_total = n(),  
    n_sites_us    = sum(name == "United States"),
    pct_us_sites  = n_sites_us / n_sites_total,
    .groups = "drop"
  )

# Install fonts
install.packages("showtext")
library(showtext)

font_add(
  family = "CMU Serif",
  regular = "~/Downloads/computer-modern/cmunrm.ttf",
  bold    = "~/Downloads/computer-modern/cmunbx.ttf"
)

showtext_auto()
# Set working directory
setwd("C:/Users/sergi/OneDrive/Bureaublad/TextminingGCMs")

# Define required packages
required_packages <- c(
  "data.table", "tidyverse", "janitor", "stringr", 
  "ggplot2", "reshape2", "cowplot", "patchwork", 
  "ggpubr", "lubridate", "knitr", "sf", 
  "rnaturalearth", "readr", "ggforce", "dplyr"
)

# Install missing packages only
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Set knitr options if knitr is loaded (for RMarkdown use)
if ("knitr" %in% loadedNamespaces()) {
  knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)
}

# Define plotting theme
theme_AP <- function() {
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-7, -7, -7, -7),
      legend.key = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "white")
    )
}


# Load Scopus metadata
full.dt <- fread("C:/Users/sergi/OneDrive/Bureaublad/TextminingGCMs/scopus_5_with_institution_and_country.csv", encoding = "UTF-8", quote = '"', na.strings = "", data.table = TRUE)
full.dt <- clean_names(full.dt)

# Define GCM model list
gcms <- c("ACCESS1.0", "ACCESS1.3", "BCC-CSM1.1", "BCC-CSM1.1(m)", "BNU-ESM", "CanESM2", "CCSM4", "CESM1-BGC", "CESM1-CAM5", "CESM1-FASTCHEM", "CFSv2-2011", "CMCC-CESM", "CMCC-CM", "CMCC-CMS", "CNRM-CM5", "CNRM-CM5-2", "CSIRO-Mk3.6.0", "EC-EARTH", "FGOALS-g2", "FGOALS-gl", "FGOALS-s2", "FIO-ESM", "GFDL-CM2.1", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "GISS-E2-H", "GISS-E2-H-CC", "GISS-E2-R", "GISS-E2-R-CC", "HadCM3", "HadGEM2-AO", "HadGEM2-CC", "HadGEM2-ES", "INMCM4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC-ESM", "MIROC-ESM-CHEM", "MIROC4h", "MIROC5", "MPI-ESM-LR", "MPI-ESM-MR", "MPI-ESM-P", "MRI-CGCM3", "MRI-ESM1", "NorESM1-M", "NorESM1-ME", "ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-HR", "AWI-CM-1-1-LR", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CAMS-CSM1-0", "CAS-ESM2-0", "CESM2", "CESM2-FV2", "CESM2-WACCM", "CIESM", "CMCC-CM2-SR5", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "CanESM5", "CanESM5-1", "CanESM5-CanOE", "E3SM-1-0", "E3SM-1-1", "E3SM-1-1-ECA", "E3SM-2-0", "EC-Earth3", "EC-Earth3-AerChem", "EC-Earth3-CC", "EC-Earth3-HR", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", "GFDL-CM4", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-G-CC", "GISS-E2-1-H", "GISS-E2-2-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "IITM-ESM", "INM-CM4-8", "INM-CM5-0", "IPSL-CM5A2-INCA", "IPSL-CM6A-LR", "KACE-1-0-G", "KIOST-ESM", "MCM-UA-1-0", "MIROC-ES2H", "MIROC-ES2L", "MIROC6", "MPI-ESM-1-2-HAM", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3", "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL", "UKESM1-1-LL")

# Combine text fields
full.dt[, full_text := paste(title, abstract, author_keywords, index_keywords, sep = " ")]
full.dt[, full_text := tolower(full_text)]

# Create GCM detection columns
for (model in gcms) {
  colname <- gsub("-", "_", model)
  full.dt[, (colname) := fifelse(grepl(tolower(model), full_text), 1, 0)]
}

# Extract institution
#full.dt[, institution := gsub(".*?;\\s*", "", correspondence_address)]
#full.dt[, institution := trimws(institution)]

# Melt to long format
melted <- melt(full.dt, id.vars = "institution", 
               measure.vars = gsub("-", "_", gcms),
               variable.name = "GCM", value.name = "Mention")

# Summarise
summary_table <- melted %>%
  group_by(institution, GCM) %>%
  summarise(mentions = sum(Mention, na.rm = TRUE)) %>%
  pivot_wider(names_from = GCM, values_from = mentions, values_fill = 0)

# Save summary
fwrite(summary_table, "GCM_mentions_by_institution.csv")

# -----------------------------
# Create model popularity table
# -----------------------------
model_popularity <- melted %>%
  group_by(GCM) %>%
  summarise(total_mentions = sum(Mention, na.rm = TRUE)) %>%
  arrange(desc(total_mentions))

# View the top few models
print(head(model_popularity, 10))

# Save to CSV
fwrite(model_popularity, "GCM_model_popularity.csv")



# --------------------------------------------------------
# Create a GCM mention frequency ranking per institution
# --------------------------------------------------------

institution_gcm_ranking <- melted %>%
  group_by(institution, GCM) %>%
  summarise(frequency = sum(Mention, na.rm = TRUE)) %>%
  filter(frequency > 0) %>%  # only include mentioned models
  arrange(institution, desc(frequency)) %>%
  ungroup()

# View top few rows
print(head(institution_gcm_ranking, 10))

# Save to CSV
fwrite(institution_gcm_ranking, "GCM_mentions_ranking_by_institution.csv")




# --------------------------------------------------------
# Create a GCM mention frequency ranking per country
# --------------------------------------------------------

# Melt again using 'country' as ID if not already done
melted_country <- melt(full.dt, 
                       id.vars = "country", 
                       measure.vars = gsub("-", "_", gcms),
                       variable.name = "GCM", 
                       value.name = "Mention")

# Summarise by country and GCM
country_gcm_ranking <- melted_country %>%
  group_by(country, GCM) %>%
  summarise(frequency = sum(Mention, na.rm = TRUE)) %>%
  filter(frequency > 0) %>%
  arrange(country, desc(frequency)) %>%
  ungroup()

# View top results
print(head(country_gcm_ranking, 10))

# Save to CSV
fwrite(country_gcm_ranking, "GCM_mentions_ranking_by_country.csv")







# --------------------------------------------------------
# GCM Mention Frequency Ranking by Research Goal
# --------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

# Assume 'full.dt' is a data.table with a 'full_text' column and GCM binary columns
# Also assume 'gcms' is a character vector of GCM column names

# Define research goals with regex patterns including variants and synonyms
goal_keywords <- list(
  "carbon cycle" = "(?i)carbon cycle|carbon-climate feedback|carbon budget",
  "ITCZ" = "(?i)ITCZ|Intertropical Convergence Zone",
  "sea level rise" = "(?i)sea level rise|rising sea level|global mean sea level",
  "El Niño" = "(?i)El Niño|ENSO|El Nino|El Niño|El Niño–Southern Oscillation",
  "monsoon" = "(?i)monsoon|Asian monsoon|Indian monsoon|West African monsoon|monsoonal",
  "tropical cyclone" = "(?i)tropical cyclone|hurricane|typhoon|cyclonic storm",
  "Hadley circulation" = "(?i)Hadley cell|Hadley circulation",
  "Arctic amplification" = "(?i)Arctic amplification|polar amplification",
  "ocean heat content" = "(?i)ocean heat content|OHC|heat uptake by ocean|thermal expansion",
  "jet stream" = "(?i)jet stream|polar jet|subtropical jet"
)

# Assign article IDs
full.dt[, article_id := .I]

# Create long-format table linking each article to all matching research goals
goal_links <- rbindlist(lapply(names(goal_keywords), function(goal) {
  pattern <- goal_keywords[[goal]]
  full.dt[grepl(pattern, full_text), .(article_id, goal)]
}))

# Merge matched goals back to full dataset
goal_merged <- merge(goal_links, full.dt, by = "article_id")

# Melt GCM columns to long format
melted_goals <- melt(goal_merged,
                     id.vars = c("goal"),
                     measure.vars = gsub("-", "_", gcms),
                     variable.name = "GCM",
                     value.name = "Mention")

# Summarize GCM mention frequency by research goal
goal_gcm_ranking <- melted_goals %>%
  group_by(goal, GCM) %>%
  summarise(frequency = sum(Mention, na.rm = TRUE)) %>%
  filter(frequency > 0) %>%
  arrange(goal, desc(frequency)) %>%
  ungroup()

# View top results
print(head(goal_gcm_ranking, 15))

# Save to CSV
fwrite(goal_gcm_ranking, "GCM_mentions_ranking_by_research_goal.csv")


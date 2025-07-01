# Define required packages
required_packages <- c(
  "data.table", "tidyverse", "janitor", "stringr", 
  "ggplot2", "reshape2", "cowplot", "patchwork", 
  "ggpubr", "lubridate", "knitr", "sf", 
  "rnaturalearth", "readr", "ggforce", "dplyr", "randomForest", "rpart.plot", "writexl", "scatterpie"
)



# ── Top-50 countries by publication count ───────────────────────────────
library(data.table)

# 1. Load the country–GCM frequency table
country_gcm <- fread("C:/Textmining GCMs 2.0/GCM_mentions_ranking_by_country.csv")  
#            

# 2. Collapse to one row per country (sum of all GCM frequencies)
country_totals <- country_gcm[
  , .(total_publications = sum(frequency, na.rm = TRUE)),  # aggregate
  by = country
][order(-total_publications)]                               # sort desc.

# 3. Keep the 50 most-prolific countries
top_50_countries <- head(country_totals, 50)

# 4. Show in the console
print(top_50_countries)

# (optional) save to file
fwrite(top_50_countries, "top_50_countries_by_publications.csv")



#####################################################
######################## top 5 gcms per country

###############################################################
##  Top-5 GCMs for the 50 most-publishing countries
###############################################################

# ── Packages ────────────────────────────────────────────────
library(data.table)   # fast IO & data wrangling
library(dplyr)        # piping / summarising
library(stringr)      # string helpers

# ── 1.  Load the two input files ────────────────────────────
# adjust the paths if your working directory is set elsewhere
top50_path   <- "top_50_countries_by_publications.csv"
gcm_path     <- "GCM_mentions_ranking_by_country.csv"

top50         <- fread(top50_path)          # must contain a “country” column
gcm_country   <- fread(gcm_path)            # must contain “country”, “GCM”, “frequency”

# ── 2.  Harmonise country names (lower-case, trimmed) ───────
top50   <- top50   %>% mutate(country = str_trim(tolower(country)))
gcm_country <- gcm_country %>% mutate(country = str_trim(tolower(country)))

# keep just the 50 countries we care about
gcm_top50 <- gcm_country %>% 
  filter(country %in% top50$country)

# ── 3.  Pick the top-5 GCMs for each country ────────────────
top5_by_country <- gcm_top50 %>% 
  arrange(country, desc(frequency)) %>%                 # rank descending
  group_by(country) %>% 
  slice_max(order_by = frequency, n = 5, with_ties = TRUE) %>%  # keep ties in 5th place
  ungroup()

# ── 4.  Output ──────────────────────────────────────────────
out_path <- "top_5_GCMs_per_top50_country.csv"
fwrite(top5_by_country, out_path)

# Quick peek in console
cat("\nTop-5 GCMs for the first few countries:\n")
print(head(top5_by_country, 15))
cat("\n✓ File written to:", out_path, "\n")



########################## scatterpie world map GCMs
# ------------------------------------------
# SCATTERPIE MAP FOR TOP 50 COUNTRIES BY GCMs
# ------------------------------------------
# ------------------------------------------
# SCATTERPIE MAP FOR TOP 50 COUNTRIES BY GCMs
# ------------------------------------------

# Load necessary packages
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scatterpie)

# ----------------------------
# 1. Load and prepare data
# ----------------------------

# Coordinates for the top‑50 publishing countries
coords <- fread("C:/Textmining GCMs 2.0/Top_50_Countries_with_Coordinates_UPDATED.csv")
coords[, country := tolower(country)]

# Top‑5 GCMs for each of those countries
gcms  <- fread("C:/Textmining GCMs 2.0/top_5_GCMs_per_top50_country.csv")
gcms[, country := tolower(country)]
gcms[, GCM := trimws(GCM)]

# Sum frequencies in case duplicates exist
gcms_df <- gcms %>%
  group_by(country, GCM) %>%
  summarise(count = sum(frequency), .groups = "drop")

# Wide format
gcms_wide <- dcast(gcms_df, country ~ GCM, value.var = "count", fill = 0)

# ----------------------------
# 2. Merge with coordinates
# ----------------------------
pie_data <- merge(coords, gcms_wide, by = "country")
setnames(pie_data, old = c("latitude", "longitude"))
pie_data$latitude  <- as.numeric(pie_data$latitude)
pie_data$longitude <- as.numeric(pie_data$longitude)

# Calculate radius
gcm_columns <- setdiff(names(gcms_wide), "country")
pie_data[, radius := rowSums(.SD), .SDcols = gcm_columns]
pie_data[, radius := radius ^ 0.3]

# ----------------------------
# 3. Colour palette
# ----------------------------
gcm_palette <- c(
  "ACCESS" = "#66c2a5", "AWI" = "#fc8d62", "BCC" = "#8da0cb", "BNU" = "#e78ac3",
  "CAMS" = "#a6d854", "CAS" = "#ffd92f", "CIESM" = "#e5c494", "CCSM" = "#b3b3b3",
  "CESM" = "#1b9e77", "CFSv2" = "#d95f02", "CMCC" = "#7570b3", "CNRM" = "#e7298a",
  "CanESM" = "#66a61e", "E3SM" = "#e6ab02", "EC-Earth" = "#a6761d", "ECMWF-IFS" = "#666666",
  "FGOALS" = "#1f78b4", "FIO" = "#d53e4f", "GFDL" = "#33a02c", "GEOS" = "#5e4fa2",
  "GISS" = "#fb9a99", "HadGEM" = "#cab2d6", "HiRAM" = "#f46d43", "ICON" = "#66c2a5",
  "IITM" = "#3288bd", "INM" = "#fdbf6f", "IPSL" = "#b2df8a", "NorESM" = "#9ecae1",
  "MIROC" = "#ff7f00", "MPI-ESM" = "#6a3d9a", "MRI" = "#ffff99", "NESM" = "#b15928",
  "NICAM" = "#a1d99b", "UKESM" = "#e41a1c"
)
used_gcms  <- intersect(names(gcm_palette), gcm_columns)
gcm_colors <- gcm_palette[used_gcms]

# ----------------------------
# 4. Mark countries whose top GCM is their own
# ----------------------------
# Define mapping of GCM to origin country
origin_map <- c(
  "ACCESS" = "australia", "AWI" = "germany", "BCC" = "china", "BNU" = "china",
  "CAMS" = "china", "CAS" = "china", "CIESM" = "italy", "CCSM" = "united states",
  "CESM" = "united states", "CFSv2" = "united states", "CMCC" = "italy",
  "CNRM" = "france", "CanESM" = "canada", "E3SM" = "united states",
  "EC-Earth" = "europe (consortium)", "ECMWF-IFS" = "europe (consortium)",
  "FGOALS" = "china", "FIO" = "china", "GFDL" = "united states", "GEOS" = "united states",
  "GISS" = "united states", "HadGEM" = "united kingdom", "HiRAM" = "united states",
  "ICON" = "germany", "IITM" = "india", "INM" = "russia", "IPSL" = "france",
  "KACE" = "south korea", "KIOST" = "south korea", "MCM-UA" = "mexico",
  "MIROC" = "japan", "MPI-ESM" = "germany", "MRI" = "japan", "NESM" = "china",
  "NICAM" = "japan", "NorESM" = "norway", "SAM0" = "south korea",
  "TaiESM" = "taiwan", "UKESM" = "united kingdom"
)

# Get favorite model
pie_data[, top_gcm := gcm_columns[which.max(.SD)], .SDcols = gcm_columns, by = country]
pie_data[, origin_match := tolower(country) == origin_map[top_gcm]]

# ----------------------------
# 5. Plot
# ----------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_scatterpie(
    aes(x = longitude, y = latitude, r = radius, group = country),
    data = pie_data,
    cols = used_gcms,
    color = NA,
    alpha = 0.9
  ) +
  geom_point(data = pie_data[origin_match == TRUE],
             aes(x = longitude, y = latitude),
             shape = 8, size = 3, color = "black") +
  scale_fill_manual(values = gcm_colors, name = "GCM") +
  geom_scatterpie_legend(
    pie_data$radius, x = -170, y = -50, n = 3,
    labeller = function(x) round((x)^(1/0.3))
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal() +
  labs(
  )

print(p)

# 7. Export high-resolution PNG using CairoPNG
CairoPNG("C:/Textmining GCMs 2.0/plots/scatterpie_map_top50_GCMs.png",
         width = 16, height = 9, units = "in", dpi = 600)

# Print to file
print(p)

# Close the device
dev.off()







# ──────────────────────────────────────────────────────────────
# Research Goal Frequency Ranking per Country (Top 50 Countries)
# Source file: Familynames metadata.csv
# ──────────────────────────────────────────────────────────────

# Load packages
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------
# 1. Load the metadata file
# ----------------------------
metadata <- fread("C:/Textmining GCMs 2.0/Familynames metadata.csv")

# Make sure text fields exist and combine them into full_text
txt_cols <- intersect(names(metadata), c("title", "abstract", "author_keywords", "index_keywords"))
metadata[, full_text := do.call(paste, c(.SD, sep = " ")), .SDcols = txt_cols]

# Assign paper_id for traceability
metadata[, paper_id := .I]

# ----------------------------
# 2. Ensure 'country' column exists
# ----------------------------
if (!"country" %in% names(metadata)) stop("❌ 'country' column not found in metadata.")

# Explode multi-country strings like "United States/Canada"
metadata_long <- metadata[!is.na(country)] %>%
  mutate(country = str_split(country, "/")) %>%
  unnest(country) %>%
  mutate(country = str_trim(tolower(country)))

# ----------------------------
# 3. Identify top 50 countries by publication count
# ----------------------------
top50 <- metadata_long %>%
  count(country, name = "n_papers") %>%
  arrange(desc(n_papers)) %>%
  slice_head(n = 50) %>%
  pull(country)

# ----------------------------
# 4. Define research goal patterns
# ----------------------------
goal_patterns <- list(
  "carbon cycle"         = "(?i)carbon cycle",
  "ITCZ"                 = "(?i)ITCZ|Intertropical Convergence Zone",
  "sea level rise"       = "(?i)sea level rise",
  "El Niño"              = "(?i)El Niño|ENSO|El Nino|El Niño|El Niño–Southern Oscillation",
  "monsoon"              = "(?i)monsoon|Asian monsoon|Indian monsoon|West African monsoon|monsoonal",
  "tropical cyclone"     = "(?i)tropical cyclone",
  "Hadley circulation"   = "(?i)Hadley circulation",
  "Arctic amplification" = "(?i)Arctic amplification",
  "ocean heat content"   = "(?i)ocean heat content",
  "jet stream"           = "(?i)jet stream"
)

# ----------------------------
# 5. Detect research goal matches
# ----------------------------
goal_hits <- rbindlist(lapply(names(goal_patterns), function(goal) {
  matched <- metadata[str_detect(full_text, goal_patterns[[goal]]), .(paper_id)]
  if (nrow(matched) == 0) return(NULL)
  matched[, goal := goal]
  return(matched)
}))

# Merge with country info
goal_hits_with_country <- merge(goal_hits, metadata_long[, .(paper_id, country)], by = "paper_id", allow.cartesian = TRUE)
goal_hits_with_country <- goal_hits_with_country[country %in% top50]

# ----------------------------
# 6. Frequency and ranking
# ----------------------------
goal_rank_by_country <- goal_hits_with_country[
  , .(frequency = .N), by = .(country, goal)
][order(country, -frequency)]

# Add rank within each country
goal_rank_by_country <- goal_rank_by_country %>%
  group_by(country) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# ----------------------------
# 7. Preview and save
# ----------------------------
print(goal_rank_by_country[order(country, rank)][1:50])
# fwrite(goal_rank_by_country, "C:/Textmining GCMs 2.0/goal_frequency_ranking_by_country.csv")














# ───────────────────────────────────────────────────────────────
# Scatterpie map for top 5 research goals per top 50 countries
# Matches style of GCM scatterpie map (white bg, clean theme)
# ───────────────────────────────────────────────────────────────

# Load packages
library(data.table)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(scatterpie)
library(sf)
library(rnaturalearth)

# ───────────────────────────────
# 1. Load data
# ───────────────────────────────
coords <- fread("C:/Textmining GCMs 2.0/Top_50_Countries_with_Coordinates_UPDATED.csv")
coords[, country := tolower(country)]

goals <- fread("C:/Textmining GCMs 2.0/top_5_research_goals_per_top50_country.csv")
goals[, country := tolower(country)]
goals[, goal := str_trim(goal)]

# ───────────────────────────────
# 2. Summarise + pivot wide
# ───────────────────────────────
goal_df <- goals %>%
  group_by(country, goal) %>%
  summarise(count = sum(frequency), .groups = "drop")

goal_wide <- dcast(goal_df, country ~ goal, value.var = "count", fill = 0)
pie_data <- merge(coords, goal_wide, by = "country")
pie_data[, latitude := as.numeric(latitude)]
pie_data[, longitude := as.numeric(longitude)]

goal_cols <- setdiff(names(goal_wide), "country")
pie_data[, radius := rowSums(.SD), .SDcols = goal_cols]
pie_data[, radius := radius ^ 0.3]

# ───────────────────────────────
# 3. Define color palette
# ───────────────────────────────
goal_palette <- c(
  "carbon cycle"         = "#66c2a5",
  "ITCZ"                 = "#fc8d62",
  "sea level rise"       = "#8da0cb",
  "El Niño"              = "#e78ac3",
  "monsoon"              = "#a6d854",
  "tropical cyclone"     = "#ffd92f",
  "Hadley circulation"   = "#e5c494",
  "Arctic amplification" = "#b3b3b3",
  "ocean heat content"   = "#1b9e77",
  "jet stream"           = "#d95f02"
)
used_goals <- intersect(goal_cols, names(goal_palette))
goal_colors <- goal_palette[used_goals]

# ───────────────────────────────
# 4. Create plot
# ───────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot(world) +
  geom_sf(fill = "antiquewhite") +
  geom_scatterpie(data = pie_data,
                  aes(x = longitude, y = latitude, r = radius, group = country),
                  cols = used_goals,
                  color = NA, alpha = 0.9) +
  scale_fill_manual(values = goal_colors, name = "Research Goal") +
  geom_scatterpie_legend(
    pie_data$radius, x = -170, y = -50, n = 3,
    labeller = function(x) round(x^(1/0.3))
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.title = element_blank(),
    panel.grid = element_blank()
  )

# Show plot
print(p)

# Optional: export to file
ggsave("C:/Textmining GCMs 2.0/plots/scatterpie_research_goals_map.png",
       plot = p, width = 16, height = 9, dpi = 600)













# ──────────────────────────────────────────────────────────────
# Research Goal Frequency Ranking per Country (Top 50 Countries)
# Source file: Familynames metadata.csv
# ──────────────────────────────────────────────────────────────

# Load packages
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------
# 1. Load the metadata file
# ----------------------------
metadata <- fread("C:/Textmining GCMs 2.0/Familynames metadata.csv")

# Make sure text fields exist and combine them into full_text
txt_cols <- intersect(names(metadata), c("title", "abstract", "author_keywords", "index_keywords"))
metadata[, full_text := do.call(paste, c(.SD, sep = " ")), .SDcols = txt_cols]

# Assign paper_id for traceability
metadata[, paper_id := .I]

# ----------------------------
# 2. Ensure 'country' column exists
# ----------------------------
if (!"country" %in% names(metadata)) stop("❌ 'country' column not found in metadata.")

# Explode multi-country strings like "United States/Canada"
metadata_long <- metadata[!is.na(country)] %>%
  mutate(country = str_split(country, "/")) %>%
  unnest(country) %>%
  mutate(country = str_trim(tolower(country)))

# ----------------------------
# 3. Identify top 50 countries by publication count
# ----------------------------
top50 <- metadata_long %>%
  count(country, name = "n_papers") %>%
  arrange(desc(n_papers)) %>%
  slice_head(n = 50) %>%
  pull(country)

# ----------------------------
# 4. Define research goal patterns
# ----------------------------
goal_patterns <- list(
  "carbon cycle"         = "(?i)carbon cycle",
  "ITCZ"                 = "(?i)ITCZ|Intertropical Convergence Zone",
  "sea level rise"       = "(?i)sea level rise",
  "El Niño"              = "(?i)El Niño|ENSO|El Nino|El Niño|El Niño–Southern Oscillation",
  "monsoon"              = "(?i)monsoon|Asian monsoon|Indian monsoon|West African monsoon|monsoonal",
  "tropical cyclone"     = "(?i)tropical cyclone",
  "Hadley circulation"   = "(?i)Hadley circulation",
  "Arctic amplification" = "(?i)Arctic amplification",
  "ocean heat content"   = "(?i)ocean heat content",
  "jet stream"           = "(?i)jet stream"
)

# ----------------------------
# 5. Detect research goal matches
# ----------------------------
goal_hits <- rbindlist(lapply(names(goal_patterns), function(goal) {
  matched <- metadata[str_detect(full_text, goal_patterns[[goal]]), .(paper_id)]
  if (nrow(matched) == 0) return(NULL)
  matched[, goal := goal]
  return(matched)
}))

# Merge with country info
goal_hits_with_country <- merge(goal_hits, metadata_long[, .(paper_id, country)], by = "paper_id", allow.cartesian = TRUE)
goal_hits_with_country <- goal_hits_with_country[country %in% top50]

# ----------------------------
# 6. Frequency and ranking
# ----------------------------
goal_rank_by_country <- goal_hits_with_country[
  , .(frequency = .N), by = .(country, goal)
][order(country, -frequency)]

# Add rank within each country
goal_rank_by_country <- goal_rank_by_country %>%
  group_by(country) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# ----------------------------
# 7. Preview and save
# ----------------------------
print(goal_rank_by_country[order(country, rank)][1:50])
# fwrite(goal_rank_by_country, "C:/Textmining GCMs 2.0/goal_frequency_ranking_by_country.csv")









# Define required packages
required_packages <- c(
  "data.table", "tidyverse", "janitor", "stringr", 
  "ggplot2", "reshape2", "cowplot", "patchwork", 
  "ggpubr", "lubridate", "knitr", "sf", 
  "rnaturalearth", "readr", "ggforce", "dplyr", "randomForest", "rpart.plot", "writexl", "scatterpie"
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





# Load required packages
library(data.table)
library(dplyr)
library(stringr)

# Load data
metadata <- fread("C:/Textmining GCMs 2.0/Familynames metadata.csv")
institutions <- fread("C:/Textmining GCMs 2.0/Institution top 50 GCM connection/top46_institutions_with_countries.csv")

# Lowercase affiliations for matching
metadata[, Affiliations := tolower(Affiliations)]
institutions[, institution := tolower(institution)]

# Combine searchable text fields
metadata[, full_text := paste(Title, `Author Keywords`, `Index Keywords`, Abstract, sep = " ")]

# Define goal patterns
goal_keywords <- list(
  "carbon cycle" = "(?i)carbon cycle",
  "ITCZ" = "(?i)ITCZ|Intertropical Convergence Zone",
  "sea level rise" = "(?i)sea level rise",
  "El Niño" = "(?i)El Niño|ENSO|El Nino|El Niño|El Niño–Southern Oscillation",
  "monsoon" = "(?i)monsoon|Asian monsoon|Indian monsoon|West African monsoon|monsoonal",
  "tropical cyclone" = "(?i)tropical cyclone",
  "Hadley circulation" = "(?i)Hadley circulation",
  "Arctic amplification" = "(?i)Arctic amplification",
  "ocean heat content" = "(?i)ocean heat content",
  "jet stream" = "(?i)jet stream"
)

# Tag articles with research goals
metadata[, article_id := .I]
goal_links <- rbindlist(lapply(names(goal_keywords), function(goal) {
  pattern <- goal_keywords[[goal]]
  metadata[grepl(pattern, full_text), .(article_id, goal)]
}))

# Merge goal tags with metadata
goal_data <- merge(goal_links, metadata, by = "article_id")

# For each institution, get top 5 goals
results <- rbindlist(lapply(institutions$institution, function(inst) {
  goal_data[str_detect(Affiliations, fixed(inst)), 
            .N, by = goal][order(-N)][1:5][, institution := inst]
}))

# Reorder columns and save
setcolorder(results, c("institution", "goal", "N"))
fwrite(results, "top5_goals_per_top46_institutions.csv")
# Save your top 5 goals per institution to file
fwrite(results, "C:/Textmining GCMs 2.0/top5_goals_per_top46_institutions.csv")
############################################################################# top 5 gcms per institutions

# Load required packages
library(data.table)
library(dplyr)

# Load data
gcm_mentions <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_institution.csv")
top46 <- fread("C:/Textmining GCMs 2.0/Institution top 50 GCM connection/top46_institutions_with_countries.csv")

# Ensure consistent case for matching
gcm_mentions[, institution := tolower(institution)]
top46[, institution := tolower(institution)]

# Filter to only include top 46 institutions
filtered_mentions <- gcm_mentions[institution %in% top46$institution]

# For each institution, get top 5 GCMs by frequency
top5_gcms <- filtered_mentions %>%
  group_by(institution) %>%
  arrange(desc(frequency)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Optional: sort output nicely
top5_gcms <- top5_gcms %>%
  arrange(institution, desc(frequency))

# Save to CSV
fwrite(top5_gcms, "top5_gcm_mentions_per_top46_institutions.csv")


#############################
##################### world map

# Load required packages
library(data.table)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load your data
institutions <- fread("C:/Textmining GCMs 2.0/Institution top 50 GCM connection/top46_institutions_with_countries.csv")

# Make sure coordinates are numeric
institutions[, Latitude := as.numeric(Latitude)]
institutions[, Longitude := as.numeric(Longitude)]

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot
ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_point(data = institutions, 
             aes(x = Longitude, y = Latitude), 
             color = "darkred", size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Top 46 Institutions Using GCMs",
       x = "Longitude", y = "Latitude") +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = "white"))


################
###### scatterpie research goals
# ----------------------------
# Load required libraries
# ----------------------------
# Install if needed
# install.packages("scatterpie")
# install.packages("reshape2")

# Load libraries
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scatterpie)

# -------------------------------
# 1. Load and clean data
# -------------------------------

# Load coordinates of institutions
coords <- fread("C:/Textmining GCMs 2.0/Institution top 50 GCM connection/top46_institutions_with_countries.csv")

# Load research goals per institution
goals <- fread("C:/Textmining GCMs 2.0/top5_goals_per_top46_institutions.csv")

# Standardize institution names
coords[, institution := tolower(institution)]
goals[, institution := tolower(institution)]

# Convert goals to clean data.frame
goals_df <- as.data.frame(goals)
names(goals_df) <- tolower(names(goals_df))
goals_df$count <- as.numeric(goals_df$n)
goals_df$n <- NULL

# Aggregate goal counts per institution-goal pair
goals_agg <- aggregate(count ~ institution + goal, data = goals_df, FUN = sum)

# Reshape to wide format (goal columns)
goals_wide <- dcast(
  data = goals_agg,
  formula = institution ~ goal,
  value.var = "count",
  fill = 0
)

# -------------------------------
# 2. Merge and prepare for plotting
# -------------------------------

# Merge with coordinates
pie_data <- merge(coords, goals_wide, by = "institution")

# Ensure correct coordinate column names and types
setnames(pie_data, old = c("Latitude", "Longitude"), new = c("latitude", "longitude"), skip_absent = TRUE)
pie_data$latitude <- as.numeric(pie_data$latitude)
pie_data$longitude <- as.numeric(pie_data$longitude)

# Define goal columns
goal_columns <- setdiff(names(goals_wide), "institution")

# Calculate pie radius (scaled)
pie_data <- as.data.table(pie_data)
pie_data[, radius := rowSums(.SD), .SDcols = goal_columns]
pie_data[, radius := radius ^ 0.3]

# -------------------------------
# 3. Plot map with correct pie positions
# -------------------------------

# Load world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Final plot
ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_scatterpie(
    aes(x = longitude, y = latitude, r = radius, group = institution),  # ✅ group added here!
    data = pie_data,
    cols = goal_columns,
    color = NA,
    alpha = 0.9
  ) +
  geom_scatterpie_legend(
    pie_data$radius, x = -170, y = -50, n = 3,
    labeller = function(x) round((x)^(1/0.3))
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Top 5 Research Goal Distribution of Institution(s) per Country",
    subtitle= "Based on top 47 institutions with most publications",
    fill = "Research Goals",
    x = "Longitude", y = "Latitude"
  )



#########################################
################ world map scatterpie top gcms

#########################################
################ world map scatterpie top gcms

#########################################
################ world map scatterpie top gcms

# ----------------------------
# Load required libraries
# ----------------------------
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scatterpie)
library(stringr)

# ----------------------------
# 1. Load and prepare data
# ----------------------------

# Load coordinates
coords <- fread("C:/Textmining GCMs 2.0/Institution top 50 GCM connection/top46_institutions_with_countries.csv")

# Ensure institution column is lowercase
if (!"institution" %in% names(coords)) {
  setnames(coords, old = "Institution", new = "institution", skip_absent = TRUE)
}
coords[, institution := tolower(institution)]

# Load Top 5 GCMs file
top5_gcms <- fread("C:/Textmining GCMs 2.0/Top_5_GCMs.csv")

# Standardize institution and GCM names
top5_gcms$institution <- tolower(top5_gcms$institution)
top5_gcms$GCM <- str_to_title(top5_gcms$GCM)

# Fix exceptions in GCM naming
top5_gcms$GCM <- gsub("Mpi-Esm", "MPI-ESM", top5_gcms$GCM)
top5_gcms$GCM <- gsub("Canesm", "CanESM", top5_gcms$GCM)
top5_gcms$GCM <- gsub("Ukesm", "UKESM", top5_gcms$GCM)
top5_gcms$GCM <- gsub("Ec-Earth", "EC-Earth", top5_gcms$GCM)
top5_gcms$GCM <- gsub("Ecmwf-Ifs", "ECMWF-IFS", top5_gcms$GCM)

# Prepare GCM data
gcms_df <- as.data.frame(top5_gcms)
gcms_df$count <- as.numeric(gcms_df$frequency)
gcms_df$frequency <- NULL

# Aggregate GCMs per institution
gcms_agg <- aggregate(count ~ institution + GCM, data = gcms_df, FUN = sum)

# Wide format: one GCM per column
gcms_wide <- dcast(gcms_agg, institution ~ GCM, value.var = "count", fill = 0)

# ----------------------------
# 2. Merge with coordinates
# ----------------------------

pie_data <- merge(coords, gcms_wide, by = "institution")
setnames(pie_data, old = c("Latitude", "Longitude"), new = c("latitude", "longitude"), skip_absent = TRUE)
pie_data$latitude <- as.numeric(pie_data$latitude)
pie_data$longitude <- as.numeric(pie_data$longitude)

gcm_columns <- setdiff(names(gcms_wide), "institution")

# Calculate pie size radius
pie_data <- as.data.table(pie_data)
pie_data[, radius := rowSums(.SD), .SDcols = gcm_columns]
pie_data[, radius := radius ^ 0.3]

# ----------------------------
# 3. Define GCM color palette and legend labels
# ----------------------------

gcm_palette <- c(
  "Access" = "#66c2a5", "Awi" = "#fc8d62", "Bcc" = "#8da0cb", "Bnu" = "#e78ac3",
  "Cams" = "#a6d854", "Cas" = "#ffd92f", "Ciesm" = "#e5c494", "Ccsm" = "#b3b3b3",
  "Cesm" = "#1b9e77", "Cfsv2" = "#d95f02", "Cmcc" = "#7570b3", "Cnrm" = "#e7298a",
  "CanESM" = "#66a61e", "E3Sm" = "#e6ab02", "EC-Earth" = "#a6761d", "ECMWF-IFS" = "#666666",
  "Fgoals" = "#1f78b4", "Fio" = "#d53e4f", "Gfdl" = "#33a02c", "Geos" = "#5e4fa2",
  "Giss" = "#fb9a99", "Hadgem" = "#cab2d6", "Hiram" = "#f46d43", "Icon" = "#66c2a5",
  "Iitm" = "#3288bd", "Inm" = "#fdbf6f", "Ipsl" = "#b2df8a", "Noresm" = "#9ecae1",
  "Miroc" = "#ff7f00", "MPI-ESM" = "#6a3d9a", "Mri" = "#ffff99", "Nesm" = "#b15928",
  "Nicam" = "#a1d99b", "UKESM" = "#e41a1c"
)

# Labels for legend — your specified capitalization
gcm_labels <- c(
  "Access" = "ACCESS", "Awi" = "AWI", "Bcc" = "BCC", "Bnu" = "BNU",
  "Cams" = "CAMS", "Cas" = "CAS", "Ciesm" = "CIESM", "Ccsm" = "CCSM",
  "Cesm" = "CESM", "Cfsv2" = "CFSv2", "Cmcc" = "CMCC", "Cnrm" = "CNRM",
  "CanESM" = "CanESM", "E3Sm" = "E3SM", "EC-Earth" = "EC-Earth", "ECMWF-IFS" = "ECMWF-IFS",
  "Fgoals" = "FGOALS", "Fio" = "FIO", "Gfdl" = "GFDL", "Geos" = "GEOS",
  "Giss" = "GISS", "Hadgem" = "HadGEM", "Hiram" = "HiRAM", "Icon" = "ICON",
  "Iitm" = "IITM", "Inm" = "INM", "Ipsl" = "IPSL", "Noresm" = "NorESM",
  "Miroc" = "MIROC", "MPI-ESM" = "MPI-ESM", "Mri" = "MRI", "Nesm" = "NESM",
  "Nicam" = "NICAM", "UKESM" = "UKESM"
)

# Match only used GCMs
used_gcms <- intersect(names(gcm_palette), gcm_columns)
gcm_colors <- gcm_palette[used_gcms]

# ----------------------------
# 4. Plot scatterpie world map
# ----------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_scatterpie(
    aes(x = longitude, y = latitude, r = radius, group = institution),
    data = pie_data,
    cols = gcm_columns,
    color = NA,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = gcm_colors,
    labels = gcm_labels[used_gcms],
    name = "GCM"
  ) +
  geom_scatterpie_legend(
    pie_data$radius, x = -170, y = -50, n = 3,
    labeller = function(x) round((x)^(1 / 0.3))
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Top 5 GCM Distribution of Institution(s) per Country",
    subtitle = "Based on top 47 institutions with most publications",
    x = "Longitude", y = "Latitude"
  )


################################ research goals with right pallet

#############################
## Research Goal Scatterpie Map
#############################

# ----------------------------
# Load required libraries
# ----------------------------
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scatterpie)

# -------------------------------
# 1. Load and clean data
# -------------------------------

# Load coordinates of institutions
coords <- fread("C:/Textmining GCMs 2.0/Institution top 50 GCM connection/top46_institutions_with_countries.csv")

# Load research goals per institution
goals <- fread("C:/Textmining GCMs 2.0/top5_goals_per_top46_institutions.csv")

# Standardize institution names
coords[, institution := tolower(institution)]
goals[, institution := tolower(institution)]

# Convert goals to clean data.frame
goals_df <- as.data.frame(goals)
names(goals_df) <- tolower(names(goals_df))
goals_df$count <- as.numeric(goals_df$n)
goals_df$n <- NULL

# Aggregate goal counts per institution-goal pair
goals_agg <- aggregate(count ~ institution + goal, data = goals_df, FUN = sum)

# Reshape to wide format (goal columns)
goals_wide <- dcast(
  data = goals_agg,
  formula = institution ~ goal,
  value.var = "count",
  fill = 0
)

# -------------------------------
# 2. Merge and prepare for plotting
# -------------------------------

# Merge with coordinates
pie_data <- merge(coords, goals_wide, by = "institution")

# Ensure correct coordinate column names and types
setnames(pie_data, old = c("Latitude", "Longitude"), new = c("latitude", "longitude"), skip_absent = TRUE)
pie_data$latitude <- as.numeric(pie_data$latitude)
pie_data$longitude <- as.numeric(pie_data$longitude)

# Define goal columns
goal_columns <- setdiff(names(goals_wide), "institution")

# Calculate pie radius (scaled)
pie_data <- as.data.table(pie_data)
pie_data[, radius := rowSums(.SD), .SDcols = goal_columns]
pie_data[, radius := radius ^ 0.3]

# -------------------------------
# 3. Define color palette for research goals
# -------------------------------

goal_palette <- c(
  "carbon cycle" = "#66c2a5",
  "ITCZ" = "#fc8d62",
  "sea level rise" = "#8da0cb",
  "El Niño" = "#e78ac3",
  "monsoon" = "#a6d854",
  "tropical cyclone" = "#ffd92f",
  "Hadley circulation" = "#e5c494",
  "Arctic amplification" = "#b3b3b3",
  "ocean heat content" = "#1b9e77",
  "jet stream" = "#d95f02"
)

# Filter only goals present in the data
used_goals <- intersect(names(goal_palette), goal_columns)
goal_colors <- goal_palette[used_goals]

# -------------------------------
# 4. Plot map with correct pie positions and colors
# -------------------------------

# Load world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# Final plot
ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  geom_scatterpie(
    aes(x = longitude, y = latitude, r = radius, group = institution),
    data = pie_data,
    cols = goal_columns,
    color = NA,
    alpha = 0.9
  ) +
  scale_fill_manual(values = goal_colors) +
  geom_scatterpie_legend(
    pie_data$radius, x = -170, y = -50, n = 3,
    labeller = function(x) round((x)^(1/0.3))
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Top 5 Research Goal Distribution of Institution(s) per Country",
    subtitle = "Based on top 47 institutions with most publications",
    fill = "Research Goals",
    x = "Longitude", y = "Latitude"
  )










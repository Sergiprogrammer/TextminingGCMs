# Set working directory
setwd("C:/Textmining GCMs 2.0")

# Define required packages
required_packages <- c(
 "data.table", "stringr" 
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


# Load necessary packages
library(data.table)
library(stringr)

# Load data
dt <- fread("C:/Textmining GCMs 2.0/Familynames metadata.csv", encoding = "UTF-8")

# Clean column names
setnames(dt, tolower(gsub(" ", "_", names(dt))))  # Replace spaces with underscores

# Combine relevant text fields
dt[, full_text := paste(title, abstract, author_keywords, index_keywords, sep = " ")]
dt[, full_text := tolower(full_text)]

# Define GCMs of interest
gcms <- c("access", "awi", "bcc", "bnu", "cams", "cas", "ciesm", "ccsm", "cesm",
          "cfsv2", "cmcc", "cnrm", "canesm", "e3sm", "ec-earth", "ecmwf-ifs",
          "fgoals", "fio", "gfdl", "geos", "giss", "hadgem", "hiram", "icon",
          "iitm", "inm", "ipsl", "kace", "kiost", "mcm-ua", "miroc", "mpi-esm",
          "mri", "nesm", "nicam", "noresm", "sam0", "taiesm", "ukesm")

# Count total GCM mentions
dt[, total_gcm_mentions := rowSums(sapply(gcms, function(gcm) str_detect(full_text, gcm)))]

# Count specific GCM mentions
dt[, access_mentions := str_detect(full_text, "access")]
dt[, cas_mentions := str_detect(full_text, "cas")]

# Summarise totals
total_mentions <- sum(dt$total_gcm_mentions, na.rm = TRUE)
access_total <- sum(dt$access_mentions, na.rm = TRUE)
cas_total <- sum(dt$cas_mentions, na.rm = TRUE)

# Calculate percentages
access_pct <- round((access_total / total_mentions) * 100, 2)
cas_pct <- round((cas_total / total_mentions) * 100, 2)

# Output
cat("Total GCM mentions:", total_mentions, "\n")
cat("ACCESS mentions:", access_total, "(", access_pct, "% )\n")
cat("CAS mentions:", cas_total, "(", cas_pct, "% )\n")


########################
### percentage of publications that used CAS or ACCESS
# Totale aantal publicaties
total_publications <- 4081

# Bereken percentages
only_access_pct <- round((only_access / total_publications) * 100, 2)
only_cas_pct <- round((only_cas / total_publications) * 100, 2)
both_pct <- round((both_access_cas / total_publications) * 100, 2)
either_pct <- round((either_access_or_cas / total_publications) * 100, 2)

# Output met percentages
cat("Mentions of only ACCESS:", only_access, "(", only_access_pct, "%)\n")
cat("Mentions of only CAS:", only_cas, "(", only_cas_pct, "%)\n")
cat("Mentions of BOTH ACCESS and CAS:", both_access_cas, "(", both_pct, "%)\n")
cat("Publications mentioning ACCESS or CAS (combined):", either_access_or_cas, "(", either_pct, "%)\n")


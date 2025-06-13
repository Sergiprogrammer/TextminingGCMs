# --------------------------------------
# 0. Install and load required packages
# --------------------------------------
required_packages <- c("data.table")

# Install missing packages
installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
  install.packages(missing)
}

# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))

# --------------------------------------
# 1. Load all relevant datasets
# --------------------------------------
institution_gcm <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_institution.csv")
country_gcm     <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_country.csv")
goal_gcm        <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_research_goal.csv")
institutions_per_paper_gcm <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_by_combined_institutions.csv")

# --------------------------------------
# 2. Standardize GCM format
# --------------------------------------
institution_gcm[, GCM := toupper(GCM)]
country_gcm[,     GCM := toupper(GCM)]
goal_gcm[,        GCM := toupper(gsub("_", "-", GCM))]
institutions_per_paper_gcm[, GCM := toupper(GCM)]

# --------------------------------------
# 3. Build contingency tables
# --------------------------------------
inst_tab <- xtabs(frequency ~ institution + GCM, data = institution_gcm)
inst_tab <- inst_tab[rowSums(inst_tab) >= 5, ]

country_tab <- xtabs(frequency ~ country + GCM, data = country_gcm)
country_tab <- country_tab[rowSums(country_tab) >= 5, ]

goal_tab <- xtabs(frequency ~ goal + GCM, data = goal_gcm)
goal_tab <- goal_tab[rowSums(goal_tab) >= 5, ]

combo_tab <- xtabs(frequency ~ institutions_combined + GCM, data = institutions_per_paper_gcm)
combo_tab <- combo_tab[rowSums(combo_tab) >= 3, ]

# --------------------------------------
# 4. Run Chi-squared tests
# --------------------------------------
chi_inst    <- chisq.test(inst_tab, simulate.p.value = TRUE, B = 10000)
chi_country <- chisq.test(country_tab, simulate.p.value = TRUE, B = 10000)
chi_goal    <- chisq.test(goal_tab, simulate.p.value = TRUE, B = 10000)
chi_combo   <- chisq.test(combo_tab, simulate.p.value = TRUE, B = 10000)

# --------------------------------------
# 5. Save chi-squared test summaries
# --------------------------------------
capture.output(chi_inst,    file = "chi2_institution_vs_GCM.txt")
capture.output(chi_country, file = "chi2_country_vs_GCM.txt")
capture.output(chi_goal,    file = "chi2_goal_vs_GCM.txt")
capture.output(chi_combo,   file = "chi2_institutions_combined_vs_GCM.txt")

# --------------------------------------
# 6. Save standardized residual matrices
# --------------------------------------
write.csv(chi_inst$stdres,    "stdres_institution_vs_GCM.csv")
write.csv(chi_country$stdres, "stdres_country_vs_GCM.csv")
write.csv(chi_goal$stdres,    "stdres_goal_vs_GCM.csv")
write.csv(chi_combo$stdres,   "stdres_institutions_combined_vs_GCM.csv")

# --------------------------------------
# 7. Create function: convert stdres to table with p-values and significance
# --------------------------------------
process_stdres <- function(stdres_matrix, filename_prefix) {
  df <- as.data.frame(as.table(stdres_matrix))
  colnames(df) <- c("row", "GCM", "stdres")
  df$p_value <- 2 * (1 - pnorm(abs(df$stdres)))
  
  # Save full table with p-values
  write.csv(df, paste0("stdres_", filename_prefix, "_with_p.csv"), row.names = FALSE)
  
  # Filter for significant associations: |stdres| > 2
  sig_df <- df[abs(df$stdres) > 2, ]
  sig_df <- sig_df[order(-abs(sig_df$stdres)), ]
  
  # Save filtered table
  write.csv(sig_df, paste0("significant_stdres_", filename_prefix, ".csv"), row.names = FALSE)
}

# --------------------------------------
# 8. Apply the function to all four tests
# --------------------------------------
process_stdres(chi_inst$stdres,    "institution_vs_GCM")
process_stdres(chi_country$stdres, "country_vs_GCM")
process_stdres(chi_goal$stdres,    "goal_vs_GCM")
process_stdres(chi_combo$stdres,   "institutions_combined_vs_GCM")









###############################
######### GCM institution/country use vs GCM origin
###############################

# -------------------------------
# 1. Load Data
# -------------------------------

institution_df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_institution.csv")
country_df     <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_country.csv")
combo_df       <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_by_combined_institutions.csv")

# --------------------------------------
# 2. Standardize names
# --------------------------------------
institution_df[, `:=`(institution = tolower(institution), Origin_Institution = tolower(Origin_Institution))]
country_df[,     `:=`(country = tolower(country), Origin_Country = tolower(Origin_Country))]
combo_df[,       `:=`(institutions_combined = tolower(institutions_combined), Origin_Institution = tolower(Origin_Institution))]

# --------------------------------------
# 3. Create contingency tables
# --------------------------------------
inst_tab <- xtabs(frequency ~ institution + Origin_Institution, data = institution_df)
inst_tab <- inst_tab[rowSums(inst_tab) >= 3, colSums(inst_tab) >= 3]

country_tab <- xtabs(frequency ~ country + Origin_Country, data = country_df)
country_tab <- country_tab[rowSums(country_tab) >= 3, colSums(country_tab) >= 3]

combo_tab <- xtabs(frequency ~ institutions_combined + Origin_Institution, data = combo_df)
combo_tab <- combo_tab[rowSums(combo_tab) >= 3, colSums(combo_tab) >= 3]

# --------------------------------------
# 4. Run chi-squared tests
# --------------------------------------
chi_inst    <- chisq.test(inst_tab, simulate.p.value = TRUE, B = 10000)
chi_country <- chisq.test(country_tab, simulate.p.value = TRUE, B = 10000)
chi_combo   <- chisq.test(combo_tab, simulate.p.value = TRUE, B = 10000)

# --------------------------------------
# 5. Save test summaries
# --------------------------------------
capture.output(chi_inst,    file = "chi2_institution_vs_origin_institution.txt")
capture.output(chi_country, file = "chi2_country_vs_origin_country.txt")
capture.output(chi_combo,   file = "chi2_combined_vs_origin_institution.txt")

# --------------------------------------
# 6. Save stdres matrices
# --------------------------------------
write.csv(chi_inst$stdres,    "stdres_institution_vs_origin_institution.csv")
write.csv(chi_country$stdres, "stdres_country_vs_origin_country.csv")
write.csv(chi_combo$stdres,   "stdres_combined_vs_origin_institution.csv")

# --------------------------------------
# 7. Convert stdres matrix to tidy table + p-values
# --------------------------------------
process_stdres <- function(stdres_matrix, prefix) {
  df <- as.data.frame(as.table(stdres_matrix))
  colnames(df) <- c("row", "column", "stdres")
  df$p_value <- 2 * (1 - pnorm(abs(df$stdres)))
  
  # Save full table
  write.csv(df, paste0("stdres_", prefix, "_with_p.csv"), row.names = FALSE)
  
  # Save significant subset
  sig_df <- df[abs(df$stdres) > 2, ]
  sig_df <- sig_df[order(-abs(sig_df$stdres)), ]
  write.csv(sig_df, paste0("significant_stdres_", prefix, ".csv"), row.names = FALSE)
}

# --------------------------------------
# 8. Apply the function to all three comparisons
# --------------------------------------
process_stdres(chi_inst$stdres,    "institution_vs_origin_institution")
process_stdres(chi_country$stdres, "country_vs_origin_country")
process_stdres(chi_combo$stdres,   "combined_vs_origin_institution")

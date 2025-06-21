# ----------------------------
# 0. Install and Load Packages
# ----------------------------

required_packages <- c(
  "data.table", "ggplot2", "dplyr", "forcats",
  "igraph", "ggraph", "tidygraph", "ggmosaic", "stringr"
)

# Install missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))








####### GCM popularity barplot

# Load required libraries
library(data.table)
library(ggplot2)
library(dplyr)

# Load data
df <- fread("C:/Textmining GCMs 2.0/results/GCM_model_popularity.csv")

# Rename columns if needed
colnames(df) <- c("GCM", "Frequency")  # adapt this to your actual columns

# Take top 15
top_15 <- df %>%
  arrange(desc(Frequency)) %>%
  slice(1:15)

# Factor to control order
top_15$GCM <- factor(top_15$GCM, levels = rev(top_15$GCM))

# Barplot
ggplot(top_15, aes(x = GCM, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 Most Frequently Used GCM Families",
    x = "GCM families",
    y = "Frequency of Mention"
  ) +
  theme_minimal(base_size = 13)










#################################
########### Research goal top 3 GCM families

library(data.table)
library(dplyr)
library(ggplot2)
library(forcats)

# Load data
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_research_goal.csv")

# Get top 3 GCMs per research goal
top3_df <- df %>%
  group_by(goal) %>%
  slice_max(order_by = frequency, n = 3, with_ties = FALSE) %>%
  ungroup()

# Reorder GCM factor inside each facet AND isolate factor levels per facet
top3_df <- top3_df %>%
  group_by(goal) %>%
  mutate(GCM = fct_reorder(GCM, frequency)) %>%
  ungroup()

# Create a combined label (GCM + goal) to trick facet into unique y-axes
top3_df$facet_GCM <- interaction(top3_df$goal, top3_df$GCM, sep = "___")

# Plot
ggplot(top3_df, aes(x = frequency, y = GCM, fill = GCM)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ goal, scales = "free") +
  labs(
    title = "Top 3 GCMs per Research Goal",
    x = "Mention Frequency",
    y = "Global Climate Model (GCM)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  coord_flip()





#####################
### country network (>5 GCMs)


# Load libraries
library(data.table)
library(dplyr)
library(igraph)
library(ggraph)
library(tidygraph)

# Load stdres data
df <- fread("C:/Textmining GCMs 2.0/results/stdres_country_vs_origin_country_with_p.csv")
colnames(df) <- c("User_Country", "Origin_Country", "StdRes", "p_value")

# Load GCM usage data
gcm_usage <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_country.csv")

# Count GCMs per user country
user_country_gcm_count <- gcm_usage %>%
  group_by(country) %>%
  summarise(GCM_count = n_distinct(GCM), .groups = "drop") %>%
  filter(GCM_count > 5)

# Combine all countries that either use or develop a GCM
user_country_list <- union(user_country_gcm_count$country, unique(df$Origin_Country))

# Filter significant & positive associations
df_filtered <- df %>%
  filter(p_value < 0.05, StdRes > 0) %>%
  filter(User_Country %in% user_country_list)

# ----- DUPLICATE origin and user countries as separate nodes -----

# Rename user and origin countries
df_edges <- df_filtered %>%
  mutate(
    from = paste0(User_Country, " (user)"),
    to = paste0(Origin_Country, " (origin)"),
    self_use = User_Country == Origin_Country
  ) %>%
  select(from, to, self_use)

# Create node list from renamed labels
nodes <- data.frame(name = unique(c(df_edges$from, df_edges$to))) %>%
  mutate(
    type = case_when(
      str_detect(name, " \\(user\\)") ~ "User Country",
      str_detect(name, " \\(origin\\)") ~ "Origin Country",
      TRUE ~ "Other"
    )
  )

# Build and tidy graph
graph <- graph_from_data_frame(df_edges, vertices = nodes, directed = FALSE)
graph_tbl <- as_tbl_graph(graph) %>%
  activate(edges) %>%
  mutate(self_use = df_edges$self_use)

# Plot
ggraph(graph_tbl, layout = "fr") +
  geom_edge_link(color = "gray60", alpha = 0.6) +
  geom_node_point(aes(color = type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(
    values = c("User Country" = "gold", "Origin Country" = "darkgreen"),
    guide = guide_legend(title = "Country Role")
  ) +
  labs(
    title = "GCM Network: Countries as Users and Developers Significant Links (p < 0.05, stdres > 0)",
    subtitle = "Only user countries with >5 GCM families used",
    color = "Node Type"
  ) +
  theme_void()









######################################################
#### country gcm network graph

df <- fread("C:/Textmining GCMs 2.0/results/stdres_country_vs_GCM_with_p.csv")
colnames(df) <- c("User_Country", "GCM", "StdRes", "p_value")

gcm_usage <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_country.csv")
user_country_gcm_count <- gcm_usage %>%
  group_by(country) %>%
  summarise(GCM_count = n_distinct(GCM)) %>%
  filter(GCM_count > 5)

df_filtered <- df %>%
  filter(p_value < 0.05, StdRes > 0) %>%
  filter(User_Country %in% user_country_gcm_count$country)

edges <- df_filtered %>% select(from = User_Country, to = GCM)
nodes <- data.frame(name = unique(c(edges$from, edges$to))) %>%
  mutate(type = !name %in% edges$to)

graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
graph_tbl <- as_tbl_graph(graph)

ggraph(graph_tbl, layout = "fr") +
  geom_edge_link(color = "gray80", alpha = 0.6) +
  geom_node_point(aes(color = type), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("darkblue", "orange"), labels = c("GCM", "User Country")) +
  labs(
    title = "Country–GCM Network Significant Links (p < 0.05, stdres > 0)",
    subtitle = "Only user countries with > 5 GCM families used",
    color = "Node Type"
  ) +
  theme_void()




###################
## Mosaic plot Research goals
# Load packages

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggmosaic)

# Load the data
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_research_goal.csv")

# Clean column names
colnames(df) <- tolower(colnames(df))
colnames(df) <- gsub("\\s+", "_", colnames(df))

# Step 1: For each research goal, identify the top 5 GCMs
df_top5 <- df %>%
  group_by(goal) %>%
  mutate(rank = rank(-frequency, ties.method = "first")) %>%
  mutate(gcm_grouped = ifelse(rank <= 5, gcm, "Other models")) %>%
  ungroup()

# Step 2: Aggregate 'Other models' shares
df_grouped <- df_top5 %>%
  group_by(goal, gcm_grouped) %>%
  summarise(frequency = sum(frequency), .groups = "drop")

# Step 3: Mosaic plot
ggplot(data = df_grouped) +
  geom_mosaic(aes(weight = frequency, x = product(goal), fill = gcm_grouped), na.rm = TRUE) +
  labs(
    title = "Top 5 GCMs per Research Goal (Others Grouped)",
    x = "Research Goal",
    y = "Proportion",
    fill = "GCM"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))








###################
## Institutional preference of favorite model


# Load packages
library(data.table)
library(dplyr)
library(ggplot2)

# Load GCM mentions by institution
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_institution.csv")

# Step 1: Calculate total mentions per institution
df_summary <- df %>%
  group_by(institution) %>%
  mutate(total_mentions = sum(frequency)) %>%
  ungroup()

# Step 2: Filter to institutions with >5 mentions
df_filtered <- df_summary %>%
  filter(total_mentions > 5)

# Step 3: Get favorite GCM per institution
df_top <- df_filtered %>%
  group_by(institution) %>%
  slice_max(order_by = frequency, n = 1, with_ties = FALSE) %>%
  mutate(attachment_pct = 100 * frequency / total_mentions) %>%
  ungroup()

# Step 4: Plot
ggplot(df_top, aes(x = reorder(institution, -attachment_pct), y = attachment_pct)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Institutional Attachment to Favorite GCM (Only Institutions >5 Mentions)",
    x = "Institution",
    y = "Attachment (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




####################################
#######  R code: % of studies coming from most attached institution per GCM


# Load packages
library(data.table)
library(dplyr)
library(ggplot2)

# Load GCM mentions by institution
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_institution.csv")

# Step 1: Calculate total mentions per GCM
df_summary <- df %>%
  group_by(GCM) %>%
  mutate(total_mentions = sum(frequency)) %>%
  ungroup()

# Step 2: Filter to GCMs with >5 total mentions
df_filtered <- df_summary %>%
  filter(total_mentions > 5)

# Step 3: Identify most attached institution for each GCM
df_top <- df_filtered %>%
  group_by(GCM) %>%
  slice_max(order_by = frequency, n = 1, with_ties = FALSE) %>%
  mutate(reliance_pct = 100 * frequency / total_mentions) %>%
  ungroup()

# Step 4: Plot
ggplot(df_top, aes(x = reorder(GCM, -reliance_pct), y = reliance_pct)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "GCM Reliance on Top-Using Institution",
    x = "GCM",
    y = "Share of Mentions from Most Attached Institution (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#########################################
####### Percentage of studies relying on favorite model

# You already calculated: mean_reliance <- 33.6

# Load ggplot2
library(ggplot2)

# Create a simple data frame with this value
df <- data.frame(label = "Institutions", reliance = 33.6)

# Plot
ggplot(df, aes(x = label, y = reliance)) +
  geom_col(fill = "gray70", width = 0.6) +
  geom_text(aes(label = paste0(reliance, "%")), vjust = -0.5, size = 6) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Percentage of Studies Relying on the Favorite Model",
    subtitle = "(Only institutions with at least 5 papers)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )







######################
##### institutional attachment to favorite model
##### & percentage of studies relying on institutional favorite model

###################
## Institutional preference of favorite model
###################

###################
## Institutional preference of favorite model
###################

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)

# Load GCM mentions by institution
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_institution.csv")

# Step 1: Calculate total mentions per institution
df_summary <- df %>%
  group_by(institution) %>%
  mutate(total_mentions = sum(frequency)) %>%
  ungroup()

# Step 2: Filter to institutions with >5 total GCM mentions
df_filtered <- df_summary %>%
  filter(total_mentions > 5)

# Step 3: Get favorite GCM per institution and calculate attachment %
df_top <- df_filtered %>%
  group_by(institution) %>%
  slice_max(order_by = frequency, n = 1, with_ties = FALSE) %>%
  mutate(attachment_pct = 100 * frequency / total_mentions) %>%
  ungroup()

# Step 4: Prepare factor order for proper x placement
df_top <- df_top %>%
  mutate(institution = factor(institution, levels = reorder(institution, -attachment_pct) %>% levels()))

# Step 5: Plot with red average line and in-plot label
ggplot(df_top, aes(x = institution, y = attachment_pct)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 33.6, color = "red", linetype = "dashed", size = 1) +
  annotate("text", 
           x = tail(levels(df_top$institution), 1), 
           y = 35.5, 
           label = "Avg. institutional model attachment for all papers: 33.6%", 
           color = "red", 
           hjust = 1) +
  labs(
    title = "Institutional Attachment to Favorite GCM (Only Institutions >5 GCM Mentions)",
    x = "Institution",
    y = "Attachment (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



##############################
###### research goal mosaik

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(scales)  # for hue_pal()

# Load the data
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_research_goal.csv")

# Clean column names
colnames(df) <- tolower(colnames(df))
colnames(df) <- gsub("\\s+", "_", colnames(df))

# Step 1: Add GCM group column, grouping non-top-5 per goal as "Other models"
df_grouped <- df %>%
  group_by(goal) %>%
  mutate(rank = rank(-frequency, ties.method = "first")) %>%
  mutate(gcm_grouped = ifelse(rank <= 5, gcm, "Other models")) %>%
  group_by(goal, gcm_grouped) %>%
  summarise(frequency = sum(frequency), .groups = "drop")

# Step 2: Generate colors
gcm_unique <- unique(df_grouped$gcm_grouped)
# Create base color palette
base_colors <- hue_pal()(length(gcm_unique) - 2)  # leave room for GFDL and Other models
# Remove GFDL and "Other models" from base assignment
non_custom_gcms <- setdiff(gcm_unique, c("GFDL", "Other models"))
gcm_colors <- setNames(base_colors, non_custom_gcms)
# Add custom colors
gcm_colors["GFDL"] <- "black"
gcm_colors["Other models"] <- "grey70"
gcm_colors["ICON"] <- "red"
gcm_colors["MRI"] <- "chocolate4"
gcm_colors["MPI_ESM"] <- "blue4"
gcm_colors["IPSL"] <- "purple"
# Step 3: Mosaic plot
ggplot(data = df_grouped) +
  geom_mosaic(aes(weight = frequency, x = product(goal), fill = gcm_grouped), na.rm = TRUE) +
  scale_fill_manual(values = gcm_colors) +
  labs(
    title = "Top 5 GCMs per Research Goal",
    x = "Research Goal",
    y = "Proportion",
    fill = "GCM"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))








#############################################
########## Bipartite network country GCM>5

# Load required libraries
library(data.table)
library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)
library(stringr)  # for str_to_title

# Load the dataset
df <- fread("C:/Textmining GCMs 2.0/results/GCM_mentions_ranking_by_country.csv")

# Title case the country names
df[, country := str_to_title(country)]

# Filter for frequency > 5
df_filtered <- df[frequency > 5]

# Define GCM relabeling with country of origin
gcm_with_country <- c(
  "ACCESS" = "ACCESS (Australia)", "AWI" = "AWI (Germany)",
  "BCC" = "BCC (China)", "BNU" = "BNU (China)", "CAMS" = "CAMS (China)",
  "CAS" = "CAS (China)", "CIESM" = "CIESM (Italy)", "CCSM" = "CCSM (United States)",
  "CESM" = "CESM (United States)", "CFSV2" = "CFSv2 (United States)",
  "CMCC" = "CMCC (Italy)", "CNRM" = "CNRM (France)", "CanESM" = "CanESM (Canada)",
  "E3SM" = "E3SM (United States)", "EC-Earth" = "EC-Earth (Europe (Consortium))",
  "ECMWF-IFS" = "ECMWF-IFS (Europe (Consortium))", "FGOALS" = "FGOALS (China)",
  "FIO" = "FIO (China)", "GFDL" = "GFDL (United States)", "GEOS" = "GEOS (United States)",
  "GISS" = "GISS (United States)", "HadGEM" = "HadGEM (United Kingdom)",
  "HiRAM" = "HiRAM (United States)", "ICON" = "ICON (Germany)", "IITM" = "IITM (India)",
  "INM" = "INM (Russia)", "IPSL" = "IPSL (France)", "KACE" = "KACE (South Korea)",
  "KIOST" = "KIOST (South Korea)", "MCM-UA" = "MCM-UA (Mexico)", "MIROC" = "MIROC (Japan)",
  "MPI-ESM" = "MPI-ESM (Germany)", "MRI" = "MRI (Japan)", "NESM" = "NESM (China)",
  "NICAM" = "NICAM (Japan)", "NorESM" = "NorESM (Norway)", "SAM0" = "SAM0 (South Korea)",
  "TaiESM" = "TaiESM (Taiwan)", "UKESM" = "UKESM (United Kingdom)"
)

# Relabel GCMs
df_filtered$GCM <- ifelse(df_filtered$GCM %in% names(gcm_with_country),
                          gcm_with_country[df_filtered$GCM],
                          df_filtered$GCM)

# Create edge list
edges <- df_filtered[, .(country, GCM)]

# Create bipartite graph
nodes <- unique(c(df_filtered$country, df_filtered$GCM))
node_type <- c(rep(TRUE, length(unique(df_filtered$country))),
               rep(FALSE, length(unique(df_filtered$GCM))))

g <- graph_from_data_frame(d = edges, vertices = data.frame(name = nodes, type = node_type), directed = FALSE)

# Convert to tidygraph for ggraph plotting
g_tbl <- as_tbl_graph(g)

# Plot the bipartite network
ggraph(g_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = as.factor(type)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = c("forestgreen", "steelblue"), labels = c("GCM", "Country")) +
  theme_void() +
  labs(title = "Country–GCM network", subtitle = "Filtered by frequency > 5", color = "Node Type")


# ============================================================================
# Script 54: Master Player List - Updated Cohort Organization
# ============================================================================
# 
# Purpose: Create comprehensive master list with reorganized cohorts:
#   - 72 complete cases (pre/post Statcast metrics)
#   - Historical cohort (2019-2024 surgeries - known outcomes)
#   - True 2026 watchlist (2025 surgeries only - projections)
#
# Prerequisites: Run Script 53 first to reorganize cohorts
# Date: December 14, 2025
# ============================================================================

library(tidyverse)
library(lubridate)

cat("\n=== MASTER PLAYER LIST (UPDATED COHORTS) ===\n\n")

# ----------------------------------------------------------------------------
# 1. Load all data sources
# ----------------------------------------------------------------------------

cat("Loading data sources...\n")

# 1. Complete cases with metrics
complete_cases <- read_csv("data/ANALYSIS_complete_cases_with_metrics.csv",
                          show_col_types = FALSE)

# 2. Expanded historical cohort (2019-2024)
historical_cohort <- read_csv("data/FINAL_COHORT_2019_2024.csv",
                             show_col_types = FALSE)

# 3. True 2026 Watchlist (2025 surgeries only)
watchlist_2026 <- read_csv("data/WATCHLIST_2026_true_projections.csv",
                          show_col_types = FALSE)

cat("✓ Complete cases:", nrow(complete_cases), "\n")
cat("✓ Historical cohort (2019-2024):", nrow(historical_cohort), "\n")
cat("✓ True 2026 Watchlist (2025 only):", nrow(watchlist_2026), "\n\n")

# ----------------------------------------------------------------------------
# 2. Create master list with all players
# ----------------------------------------------------------------------------

cat("Creating master list...\n")

# Combine all sources
master_list <- bind_rows(
  complete_cases,
  historical_cohort,
  watchlist_2026
) %>%
  # Deduplicate by key_mlbam (keep most complete record)
  group_by(key_mlbam) %>%
  summarise(
    # Handle column name conflicts from joins
    player_name = coalesce(first(na.omit(player_name)), 
                          first(na.omit(c(player_name.x, player_name.y)))),
    surgery_date = first(na.omit(surgery_date)),
    pre_surgery_ip = first(na.omit(pre_surgery_ip)),
    role_classification = first(na.omit(role_classification)),
    body_type = first(na.omit(body_type)),
    height_inches = first(na.omit(height_inches)),
    weight = first(na.omit(weight)),
    bats = first(na.omit(bats)),
    throws = first(na.omit(throws)),
    
    # Recovery metrics (if available)
    avg_velo_pre = first(na.omit(avg_velo_pre)),
    avg_velo_post = first(na.omit(avg_velo_post)),
    velo_change = first(na.omit(velo_change)),
    velo_recovery_pct = first(na.omit(velo_recovery_pct)),
    whiff_pct_pre = first(na.omit(whiff_pct_pre)),
    whiff_pct_post = first(na.omit(whiff_pct_post)),
    whiff_change = first(na.omit(whiff_change)),
    elite_recovery = first(na.omit(elite_recovery)),
    
    .groups = "drop"
  ) %>%
  # Add data availability flags
  mutate(
    has_complete_data = key_mlbam %in% complete_cases$key_mlbam,
    in_historical_cohort = key_mlbam %in% historical_cohort$key_mlbam,
    in_2026_watchlist = key_mlbam %in% watchlist_2026$key_mlbam,
    
    # Calculate surgery timing
    surgery_year = year(surgery_date),
    months_since = as.numeric(difftime(ymd("2025-12-14"), 
                                       surgery_date, 
                                       units = "days")) / 30.44,
    
    # Assign cohort labels (updated organization)
    cohort = case_when(
      surgery_year >= 2019 & surgery_year <= 2024 ~ "Historical (2019-2024)",
      surgery_year == 2025 ~ "True 2026 Watchlist",
      TRUE ~ "Unknown"
    ),
    
    # Return status
    return_status = case_when(
      months_since >= 18 & !is.na(avg_velo_post) ~ "Returned",
      months_since >= 18 & is.na(avg_velo_post) ~ "Expected return, no data",
      months_since < 18 ~ "Still recovering",
      TRUE ~ "Unknown"
    )
  ) %>%
  arrange(desc(months_since), player_name)

cat("✓ Master list created:", nrow(master_list), "unique players\n\n")

# ----------------------------------------------------------------------------
# 3. Summary statistics
# ----------------------------------------------------------------------------

cat("=== SUMMARY BY COHORT ===\n\n")

master_list %>%
  count(cohort, sort = TRUE) %>%
  print()

cat("\n=== DATA AVAILABILITY ===\n\n")

data_summary <- master_list %>%
  summarise(
    total_players = n(),
    with_complete_data = sum(has_complete_data, na.rm = TRUE),
    in_historical = sum(in_historical_cohort, na.rm = TRUE),
    in_watchlist = sum(in_2026_watchlist, na.rm = TRUE),
    with_body_type = sum(!is.na(body_type)),
    with_role = sum(!is.na(role_classification))
  )

print(data_summary)

cat("\n=== RETURN STATUS ===\n\n")

master_list %>%
  count(return_status, sort = TRUE) %>%
  print()

# ----------------------------------------------------------------------------
# 4. Save master lists
# ----------------------------------------------------------------------------

cat("\nSaving master lists...\n")

# 1. Complete master list
write_csv(master_list,
          "output/MASTER_player_list_updated.csv")
cat("✓ MASTER_player_list_updated.csv\n")

# 2. Complete cases only (with pre/post metrics)
master_list %>%
  filter(has_complete_data) %>%
  write_csv("output/MASTER_complete_cases_only.csv")
cat("✓ MASTER_complete_cases_only.csv\n")

# 3. Historical cohort only (2019-2024, known outcomes)
master_list %>%
  filter(in_historical_cohort) %>%
  write_csv("output/MASTER_historical_2019_2024.csv")
cat("✓ MASTER_historical_2019_2024.csv\n")

# 4. True watchlist only (2025 surgeries, projections)
master_list %>%
  filter(in_2026_watchlist) %>%
  write_csv("output/MASTER_watchlist_2025_only.csv")
cat("✓ MASTER_watchlist_2025_only.csv\n\n")

# ----------------------------------------------------------------------------
# 5. Notable players showcase
# ----------------------------------------------------------------------------

cat("=== TOP 10 ELITE RECOVERIES (Historical Cohort) ===\n\n")

master_list %>%
  filter(elite_recovery == TRUE, in_historical_cohort) %>%
  arrange(desc(velo_recovery_pct)) %>%
  select(player_name, surgery_date, role_classification, 
         velo_recovery_pct, whiff_change) %>%
  head(10) %>%
  print(n = 10)

cat("\n=== TOP 20 PROSPECTS (2025 Watchlist by Pre-Surgery IP) ===\n\n")

master_list %>%
  filter(in_2026_watchlist) %>%
  arrange(desc(pre_surgery_ip)) %>%
  select(player_name, surgery_date, role_classification, 
         pre_surgery_ip, body_type) %>%
  head(20) %>%
  print(n = 20)

cat("\n=== RECENT 2024 RETURNEES (Now in Historical Cohort) ===\n\n")

master_list %>%
  filter(surgery_year == 2024, return_status == "Returned") %>%
  arrange(surgery_date) %>%
  select(player_name, surgery_date, role_classification, 
         months_since, velo_recovery_pct) %>%
  head(15) %>%
  print(n = 15)

cat("\n=== NOTABLE NAMES ===\n\n")

notable_names <- c("Jacob deGrom", "Gerrit Cole", "Shane Bieber",
                  "Sandy Alcantara", "Dustin May", "Tyler Glasnow",
                  "Luis Severino", "Chris Sale", "Noah Syndergaard",
                  "Nathan Eovaldi", "Shohei Ohtani", "Spencer Strider")

notable <- master_list %>%
  filter(player_name %in% notable_names) %>%
  arrange(surgery_date) %>%
  select(player_name, surgery_date, cohort, role_classification, 
         return_status, has_complete_data)

if(nrow(notable) > 0) {
  print(notable, n = Inf)
} else {
  cat("(None found in current dataset)\n")
}

# ----------------------------------------------------------------------------
# 6. Player search examples
# ----------------------------------------------------------------------------

cat("\n=== PLAYER SEARCH EXAMPLES ===\n\n")

cat("To search for a specific player:\n")
cat("  master_list %>% filter(str_detect(player_name, 'deGrom'))\n\n")

cat("To see all relievers in 2025 watchlist:\n")
cat("  master_list %>% filter(role_classification == 'Reliever', in_2026_watchlist)\n\n")

cat("To see all Finesse body types:\n")
cat("  master_list %>% filter(body_type == 'Finesse')\n\n")

cat("To see elite recoveries:\n")
cat("  master_list %>% filter(elite_recovery == TRUE)\n\n")

# ----------------------------------------------------------------------------
# 7. Summary by role and body type
# ----------------------------------------------------------------------------

cat("=== ROLE DISTRIBUTION ===\n\n")

master_list %>%
  count(role_classification, cohort) %>%
  pivot_wider(names_from = cohort, values_from = n, values_fill = 0) %>%
  print()

cat("\n=== BODY TYPE DISTRIBUTION ===\n\n")

master_list %>%
  filter(!is.na(body_type)) %>%
  count(body_type, cohort) %>%
  pivot_wider(names_from = cohort, values_from = n, values_fill = 0) %>%
  print()

cat("\n=== MASTER PLAYER LIST COMPLETE ===\n")
cat("\nKey improvements from reorganization:\n")
cat("  - Historical cohort now includes 2019-2024 (known outcomes)\n")
cat("  - Watchlist only includes 2025 surgeries (true projections)\n")
cat("  - Separates observable data from projections\n")
cat("  - Enables cleaner model training and validation\n")

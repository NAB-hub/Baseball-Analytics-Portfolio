# ============================================================================
# Script 53: Reorganize Cohorts (2019-2024 Historical + 2025 Watchlist)
# ============================================================================
# 
# Purpose: Split watchlist by surgery year to create clean separation:
#   - Expand Historical Cohort: 2019-2024 surgeries (known outcomes)
#   - True 2026 Watchlist: 2025 surgeries only (projections)
#
# This resolves the model mixing issue where 2024 returnees like deGrom
# were being treated as projections when they're now observable data.
#
# Date: December 14, 2025
# ============================================================================

library(tidyverse)
library(lubridate)

cat("\n=== COHORT REORGANIZATION ===\n")
cat("Moving 2024 surgery patients from watchlist to historical cohort\n")
cat("Keeping only 2025 surgery patients as true 2026 projections\n\n")

# ----------------------------------------------------------------------------
# 1. Load existing data
# ----------------------------------------------------------------------------

cat("Loading existing cohorts...\n")

# Historical cohort (2019-2023)
historical_original <- read_csv("data/FINAL_COHORT_2019_2023.csv", 
                                show_col_types = FALSE)

# Watchlist (2024-2025 mixed)
watchlist_original <- read_csv("data/WATCHLIST_2026_roles_classified.csv",
                               show_col_types = FALSE)

cat("✓ Historical cohort (2019-2023):", nrow(historical_original), "players\n")
cat("✓ Watchlist (2024-2025 mixed):", nrow(watchlist_original), "players\n\n")

# ----------------------------------------------------------------------------
# 2. Split watchlist by surgery year
# ----------------------------------------------------------------------------

cat("Splitting watchlist by surgery year...\n")

watchlist_original <- watchlist_original %>%
  mutate(surgery_year = year(surgery_date))

# Count by year
year_counts <- watchlist_original %>%
  count(surgery_year, name = "n_players")

cat("\nWatchlist breakdown:\n")
print(year_counts)

# Split into 2024 (add to historical) and 2025 (keep as watchlist)
patients_2024 <- watchlist_original %>%
  filter(surgery_year == 2024)

patients_2025 <- watchlist_original %>%
  filter(surgery_year == 2025)

cat("\n✓ 2024 surgery patients:", nrow(patients_2024), "(move to historical)\n")
cat("✓ 2025 surgery patients:", nrow(patients_2025), "(keep as watchlist)\n\n")

# ----------------------------------------------------------------------------
# 3. Create expanded historical cohort (2019-2024)
# ----------------------------------------------------------------------------

cat("Creating expanded historical cohort (2019-2024)...\n")

# Ensure consistent column structure before combining
# Get all unique columns
all_cols_hist <- names(historical_original)
all_cols_2024 <- names(patients_2024)
all_cols <- unique(c(all_cols_hist, all_cols_2024))

# Add missing columns as NA to both datasets
for(col in all_cols) {
  if(!(col %in% names(historical_original))) {
    historical_original[[col]] <- NA
  }
  if(!(col %in% names(patients_2024))) {
    patients_2024[[col]] <- NA
  }
}

# Combine datasets
historical_expanded <- bind_rows(
  historical_original %>% select(all_of(all_cols)),
  patients_2024 %>% select(all_of(all_cols))
) %>%
  distinct(key_mlbam, .keep_all = TRUE) %>%
  arrange(surgery_date)

cat("✓ Expanded historical cohort:", nrow(historical_expanded), "players\n")
cat("  - 2019 surgeries:", sum(year(historical_expanded$surgery_date) == 2019, na.rm = TRUE), "\n")
cat("  - 2020 surgeries:", sum(year(historical_expanded$surgery_date) == 2020, na.rm = TRUE), "\n")
cat("  - 2021 surgeries:", sum(year(historical_expanded$surgery_date) == 2021, na.rm = TRUE), "\n")
cat("  - 2022 surgeries:", sum(year(historical_expanded$surgery_date) == 2022, na.rm = TRUE), "\n")
cat("  - 2023 surgeries:", sum(year(historical_expanded$surgery_date) == 2023, na.rm = TRUE), "\n")
cat("  - 2024 surgeries:", sum(year(historical_expanded$surgery_date) == 2024, na.rm = TRUE), "\n\n")

# ----------------------------------------------------------------------------
# 4. Create true 2026 watchlist (2025 only)
# ----------------------------------------------------------------------------

cat("Creating true 2026 watchlist (2025 surgeries only)...\n")

watchlist_2025_only <- patients_2025 %>%
  mutate(
    months_since_surgery = as.numeric(difftime(ymd("2025-12-14"), 
                                               surgery_date, 
                                               units = "days")) / 30.44,
    expected_return = surgery_date + months(18),
    time_to_return_months = as.numeric(difftime(expected_return,
                                                ymd("2025-12-14"),
                                                units = "days")) / 30.44
  ) %>%
  arrange(surgery_date)

cat("✓ True 2026 watchlist:", nrow(watchlist_2025_only), "players\n")
cat("  - All had surgery in 2025\n")
cat("  - Currently", round(mean(watchlist_2025_only$months_since_surgery, na.rm = TRUE), 1), 
    "months post-surgery on average\n")
cat("  - Expected to return in 2026-2027\n\n")

# ----------------------------------------------------------------------------
# 5. Save reorganized cohorts
# ----------------------------------------------------------------------------

cat("Saving reorganized cohorts...\n")

# Historical cohort (2019-2024)
write_csv(historical_expanded,
          "data/FINAL_COHORT_2019_2024.csv")
cat("✓ Saved: FINAL_COHORT_2019_2024.csv\n")

# True watchlist (2025 only)
write_csv(watchlist_2025_only,
          "data/WATCHLIST_2026_true_projections.csv")
cat("✓ Saved: WATCHLIST_2026_true_projections.csv\n\n")

# ----------------------------------------------------------------------------
# 6. Create summary comparison
# ----------------------------------------------------------------------------

cat("=== REORGANIZATION SUMMARY ===\n\n")

cat("BEFORE:\n")
cat("  Historical (2019-2023):", nrow(historical_original), "players\n")
cat("  Watchlist (2024-2025):", nrow(watchlist_original), "players\n")
cat("  Total:", nrow(historical_original) + nrow(watchlist_original), "players\n\n")

cat("AFTER:\n")
cat("  Historical (2019-2024):", nrow(historical_expanded), "players\n")
cat("  Watchlist (2025 only):", nrow(watchlist_2025_only), "players\n")
cat("  Total:", nrow(historical_expanded) + nrow(watchlist_2025_only), "players\n\n")

cat("RATIONALE:\n")
cat("  - 2024 surgery patients have 12-24 months of recovery data (observable)\n")
cat("  - These should be part of historical cohort for model training\n")
cat("  - Only 2025 patients are true projections for 2026 performance\n")
cat("  - This prevents mixing known outcomes with projections\n\n")

# ----------------------------------------------------------------------------
# 7. Notable players in each cohort
# ----------------------------------------------------------------------------

cat("=== NOTABLE PLAYERS BY COHORT ===\n\n")

# 2024 players moving to historical
notable_2024 <- patients_2024 %>%
  filter(player_name %in% c("Jacob deGrom", "Gerrit Cole", "Shane Bieber",
                            "Sandy Alcantara", "Dustin May", "Tyler Glasnow"))

if(nrow(notable_2024) > 0) {
  cat("Notable 2024 patients (now historical):\n")
  notable_2024 %>%
    select(player_name, surgery_date, role_classification) %>%
    arrange(player_name) %>%
    print(n = Inf)
  cat("\n")
}

# 2025 patients staying in watchlist
notable_2025 <- patients_2025 %>%
  arrange(desc(pre_surgery_ip)) %>%
  head(10)

cat("Top 10 players in true 2026 watchlist (by pre-surgery IP):\n")
notable_2025 %>%
  select(player_name, surgery_date, role_classification, pre_surgery_ip) %>%
  print(n = 10)

cat("\n=== COHORT REORGANIZATION COMPLETE ===\n")
cat("Next steps:\n")
cat("1. Update Script 52 (master player list) with new cohort definitions\n")
cat("2. Re-run archetype analysis on expanded historical cohort (2019-2024)\n")
cat("3. Apply projection model only to 2025 watchlist\n")

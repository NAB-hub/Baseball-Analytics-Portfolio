# =============================================================================
# 33_prefilter_hitters.R
# Pre-filter position players before scraping stats
# Based on Age, Level, and development curve
# =============================================================================

library(dplyr)

cat("Pre-filtering position players for Rule 5 viability...\n")

# Load classified position players
hitters <- read.csv("output/position_players_classified.csv", stringsAsFactors = FALSE)

cat(sprintf("Starting with: %d position players\n\n", nrow(hitters)))

# === FILTER 1: Level Threshold ===
# Must have played AA or AAA (A+ too low for immediate MLB impact)
hitters_filtered <- hitters %>%
  filter(PROJ.LEVEL %in% c("AA", "AAA") | Max.Level %in% c("AA", "AAA"))

cat(sprintf("After Level filter (AA/AAA): %d players (-%d)\n", 
            nrow(hitters_filtered), 
            nrow(hitters) - nrow(hitters_filtered)))

# === FILTER 2: Age-appropriate development ===
# Age thresholds by level (similar to pitchers):
# AAA: 23-27 ideal (too young = raw, too old = org depth)
# AA: 21-25 ideal
# Penalize: 26+ at AA, 28+ at AAA

hitters_filtered <- hitters_filtered %>%
  mutate(
    age_level_flag = case_when(
      PROJ.LEVEL == "AAA" & AGE >= 28 ~ "OLD_AAA",
      PROJ.LEVEL == "AA" & AGE >= 26 ~ "OLD_AA",
      PROJ.LEVEL == "AAA" & AGE < 23 ~ "YOUNG_AAA",
      PROJ.LEVEL == "AA" & AGE < 21 ~ "YOUNG_AA",
      TRUE ~ "AGE_APPROPRIATE"
    )
  ) %>%
  # Remove extreme outliers (but keep borderline cases)
  filter(!age_level_flag %in% c("OLD_AAA", "OLD_AA"))

cat(sprintf("After Age filter (remove 26+ at AA, 28+ at AAA): %d players (-%d)\n", 
            nrow(hitters_filtered), 
            nrow(hitters) - nrow(hitters_filtered)))

# === FILTER 3: Remove catchers with no secondary position ===
# Pure catchers (C only) are rarely Rule 5 targets unless elite
# Keep C/1B, C/OF (utility value)
hitters_filtered <- hitters_filtered %>%
  filter(!(position_bucket == "C" & num_positions == 1 & AGE < 24))

cat(sprintf("After Catcher filter (keep older C or multi-position C): %d players (-%d)\n", 
            nrow(hitters_filtered), 
            nrow(hitters) - nrow(hitters_filtered)))

# === Summary by position bucket ===
cat("\n📊 Remaining players by position:\n")
position_summary <- hitters_filtered %>%
  group_by(position_bucket) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(position_summary)

# === Age distribution ===
cat("\n📊 Age distribution:\n")
age_summary <- hitters_filtered %>%
  summarise(
    min_age = min(AGE, na.rm = TRUE),
    median_age = median(AGE, na.rm = TRUE),
    max_age = max(AGE, na.rm = TRUE)
  )
print(age_summary)

# === Level distribution ===
cat("\n📊 Level distribution:\n")
level_summary <- hitters_filtered %>%
  group_by(PROJ.LEVEL) %>%
  summarise(count = n())
print(level_summary)

# Save pre-filtered list
write.csv(hitters_filtered, "output/hitters_prefiltered.csv", row.names = FALSE)

cat(sprintf("\n✅ Pre-filtering complete!\n"))
cat(sprintf("   📉 Reduced from %d → %d players (%.1f%% reduction)\n", 
            nrow(hitters), 
            nrow(hitters_filtered),
            (1 - nrow(hitters_filtered)/nrow(hitters)) * 100))
cat("   📁 Output: output/hitters_prefiltered.csv\n")
cat("\n💡 Next step: Scrape stats for these %d players\n", nrow(hitters_filtered))

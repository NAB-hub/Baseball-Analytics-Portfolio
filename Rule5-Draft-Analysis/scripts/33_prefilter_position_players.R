# =============================================================================
# 33_prefilter_position_players.R
# Pre-filter position players before scraping stats
# Remove players too far from MLB-ready
# =============================================================================

library(dplyr)

cat("Pre-filtering position players for Rule 5 viability...\n")

# Load classified position players
hitters <- read.csv("output/position_players_classified.csv", stringsAsFactors = FALSE)

cat(sprintf("Starting with: %d Rule 5 eligible position players\n", nrow(hitters)))

# FILTER 1: Remove Single-A players (too far from MLB)
hitters_filtered <- hitters %>%
  filter(PROJ.LEVEL %in% c("A+", "AA", "AAA"))

cat(sprintf("\n✅ FILTER 1 - Level (A+/AA/AAA only): %d players remain (%d removed)\n", 
    nrow(hitters_filtered), nrow(hitters) - nrow(hitters_filtered)))

# Show level distribution after filter
level_dist <- hitters_filtered %>%
  group_by(PROJ.LEVEL) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nLevel distribution:\n")
print(level_dist)

# Show position bucket distribution
position_dist <- hitters_filtered %>%
  group_by(position_bucket) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\nPosition bucket distribution:\n")
print(position_dist)

# Save filtered list
write.csv(hitters_filtered, "output/position_players_prefiltered.csv", row.names = FALSE)

cat(sprintf("\n✅ Pre-filter complete: %d players ready for next filter stage\n", nrow(hitters_filtered)))
cat("   📁 Saved to: output/position_players_prefiltered.csv\n")

# =============================================================================
# 33_filter_position_players.R
# Pre-filter position players to narrow down candidates before scraping stats
# Step 1: Remove Single A players (too far from MLB-ready)
# =============================================================================

library(dplyr)

cat("Filtering position players by development level...\n")

# Load classified position players
hitters <- read.csv("output/position_players_classified.csv", stringsAsFactors = FALSE)

cat(sprintf("Starting with %d Rule 5 eligible position players\n", nrow(hitters)))

# Check level distribution before filtering
cat("\nLevel distribution (before filter):\n")
level_counts_before <- hitters %>%
  group_by(PROJ.LEVEL) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(level_counts_before)

# FILTER 1: Remove Single A players (A, A-, A+)
# Keep only: AA, AAA (MLB-ready or close)
hitters_filtered <- hitters %>%
  filter(!PROJ.LEVEL %in% c("A", "A-", "A+"))

cat(sprintf("\n✂️  Removed %d Single A players\n", nrow(hitters) - nrow(hitters_filtered)))
cat(sprintf("   Remaining: %d players\n", nrow(hitters_filtered)))

# Check level distribution after filtering
cat("\nLevel distribution (after filter):\n")
level_counts_after <- hitters_filtered %>%
  group_by(PROJ.LEVEL) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(level_counts_after)

# Check position distribution after filtering
cat("\nPosition distribution (after filter):\n")
pos_counts_after <- hitters_filtered %>%
  group_by(position_bucket) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
print(pos_counts_after)

# Save filtered list
write.csv(hitters_filtered, "output/position_players_filtered.csv", row.names = FALSE)

cat("\n✅ Filtering complete!\n")
cat(sprintf("   📁 Output: output/position_players_filtered.csv\n"))
cat(sprintf("   🎯 Reduced from %d to %d players (%.1f%% reduction)\n", 
            nrow(hitters), nrow(hitters_filtered), 
            (1 - nrow(hitters_filtered)/nrow(hitters)) * 100))

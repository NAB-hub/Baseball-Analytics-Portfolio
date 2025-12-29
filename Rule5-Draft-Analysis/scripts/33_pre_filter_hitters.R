# =============================================================================
# 33_pre_filter_hitters.R
# Pre-filter position players before scraping stats
# Remove players too far from MLB-ready
# =============================================================================

library(dplyr)

cat("Pre-filtering position players to reduce scraping workload...\n")

# Load classified position players
hitters <- read.csv("output/position_players_classified.csv", stringsAsFactors = FALSE)

cat(sprintf("Starting with: %d position players\n", nrow(hitters)))

# Filter 1: AA or AAA only (remove A+ and below)
hitters_filtered <- hitters %>%
  filter(Max.Level %in% c("AA", "AAA"))

cat(sprintf("After AA/AAA filter: %d players (removed %d)\n", 
            nrow(hitters_filtered), 
            nrow(hitters) - nrow(hitters_filtered)))

# Show position bucket breakdown after filter
bucket_counts <- hitters_filtered %>%
  group_by(position_bucket) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

cat("\n📊 Position Distribution (AA/AAA only):\n")
print(bucket_counts)

# Show age distribution
age_summary <- hitters_filtered %>%
  summarise(
    min_age = min(AGE, na.rm = TRUE),
    avg_age = mean(AGE, na.rm = TRUE),
    max_age = max(AGE, na.rm = TRUE),
    total = n()
  )

cat("\n📈 Age Distribution:\n")
cat(sprintf("   Min: %.1f | Avg: %.1f | Max: %.1f\n", 
            age_summary$min_age, age_summary$avg_age, age_summary$max_age))

# Show level breakdown
level_counts <- hitters_filtered %>%
  group_by(Max.Level) %>%
  summarise(count = n())

cat("\n📍 Level Breakdown:\n")
print(level_counts)

# Save filtered list
write.csv(hitters_filtered, "output/position_players_filtered.csv", row.names = FALSE)

cat(sprintf("\n✅ Pre-filtering complete!\n"))
cat(sprintf("   📁 Output: output/position_players_filtered.csv\n"))
cat(sprintf("   🎯 Ready to scrape stats for %d players (down from %d)\n", 
            nrow(hitters_filtered), nrow(hitters)))

# 35_scrape_r5_benchmarks.R
# MANUAL DOWNLOAD INSTRUCTIONS for Rule 5 Benchmark Players
# We use the SAME approach as pitcher data: manually download from FanGraphs player pages

library(dplyr)

cat("📊 Rule 5 Benchmark Data Collection Instructions\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

# Load filtered position players
hitters <- read.csv("output/position_players_filtered.csv", stringsAsFactors = FALSE)

# Identify R5 returned players
r5_returned <- hitters %>%
  filter(grepl("Rule 5|R5 return", HOW.ACQUIRED, ignore.case = TRUE)) %>%
  select(PLAYER, Team, Age, position_bucket, playerId, HOW.ACQUIRED) %>%
  distinct() %>%
  arrange(position_bucket, PLAYER)

cat("🔄 Found", nrow(r5_returned), "Rule 5 returned players\n\n")

# Save list with URLs for manual download
r5_returned <- r5_returned %>%
  mutate(fangraphs_url = paste0("https://www.fangraphs.com/players/", playerId, "/stats?position=OF"))

write.csv(r5_returned, "output/r5_benchmark_players.csv", row.names = FALSE)

cat("✅ Benchmark player list saved!\n")
cat("   📁 File: output/r5_benchmark_players.csv\n")
cat("   🔗 Contains FanGraphs URLs for each player\n\n")

cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  MANUAL DOWNLOAD INSTRUCTIONS (Same as pitcher approach)\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

cat("OPTION 1 - Individual Player Downloads:\n")
cat("  1. Open output/r5_benchmark_players.csv\n")
cat("  2. Visit each player's FanGraphs URL\n")
cat("  3. Click 'Export Data' on their MiLB stats table\n")
cat("  4. Save all CSVs to Downloads folder\n")
cat("  5. Combine them into one file\n\n")

cat("OPTION 2 - FanGraphs Custom Report (FASTER):\n")
cat("  1. Go to FanGraphs.com MiLB Leaderboards\n")
cat("  2. Filter by these", nrow(r5_returned), "player IDs:\n\n")

# Print playerIds in batches for copy/paste
player_ids <- r5_returned$playerId
cat("     Player IDs to filter:\n")
cat("     ", paste(player_ids, collapse = ", "), "\n\n")

cat("  3. Set date range: 2021-2025\n")
cat("  4. Export to CSV with columns:\n")
cat("     - Season, Team, Level, Age, G, PA, AB, H, 2B, 3B, HR, R, RBI, BB, SO, SB, CS\n")
cat("     - AVG, OBP, SLG, wOBA, wRC+, ISO, BABIP, BB%, K%, Hard%\n")
cat("  5. Save as: r5_benchmark_hitting_stats_2021_2025.csv\n")
cat("  6. Place in: data/ folder\n\n")

cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

cat("📋 R5 Benchmark Players by Position:\n")
print(r5_returned %>% count(position_bucket) %>% arrange(desc(n)))

cat("\n🎯 Key Players to Watch:\n")
pintar <- r5_returned %>% filter(PLAYER == "Andrew Pintar")
if (nrow(pintar) > 0) {
  cat("  • Andrew Pintar (OF) - Traded to MIA, 4th OF candidate\n")
  cat("    ", pintar$fangraphs_url, "\n\n")
}

cat("💪 LETS FUCKING GO! 1 more mile to the finish line!\n")
cat("   Download the hitting stats and we'll build the rankings!\n")

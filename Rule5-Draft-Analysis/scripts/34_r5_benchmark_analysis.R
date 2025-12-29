# 34_r5_benchmark_analysis.R
# Analyze players who were Rule 5 drafted but returned
# These establish minimum benchmarks for draftable talent

library(dplyr)
library(tidyr)

cat("📊 Analyzing Rule 5 Return Benchmarks...\n\n")

# Load filtered position players
hitters <- read.csv("output/position_players_filtered.csv", stringsAsFactors = FALSE)

# Find all R5 returned players
r5_returned <- hitters %>%
  filter(grepl("Rule 5|R5 return", HOW.ACQUIRED, ignore.case = TRUE)) %>%
  select(PLAYER, Team, Age, Max.Level, POS, position_bucket, HOW.ACQUIRED, playerId) %>%
  arrange(position_bucket, PLAYER)

cat("🔄 Rule 5 Returned Players (", nrow(r5_returned), " total):\n\n", sep = "")
print(r5_returned, n = 50)

# Position distribution of R5 returns
cat("\n📍 Position Distribution:\n")
r5_positions <- r5_returned %>%
  count(position_bucket, name = "r5_returns") %>%
  arrange(desc(r5_returns))
print(r5_positions)

# Age distribution
cat("\n📈 Age Statistics:\n")
cat("  Min:", round(min(r5_returned$Age, na.rm = TRUE), 1), "\n")
cat("  Avg:", round(mean(r5_returned$Age, na.rm = TRUE), 1), "\n")
cat("  Max:", round(max(r5_returned$Age, na.rm = TRUE), 1), "\n")

# Level distribution
cat("\n📍 Level Distribution:\n")
r5_levels <- r5_returned %>%
  count(Max.Level) %>%
  arrange(desc(n))
print(r5_levels)

# Highlight key players mentioned by user
cat("\n🎯 Key Draft Candidates:\n\n")

pintar <- hitters %>% filter(PLAYER == "Andrew Pintar")
if (nrow(pintar) > 0) {
  cat("✅ Andrew Pintar (4th OF candidate):\n")
  cat("   Team:", pintar$Team, "\n")
  cat("   Age:", pintar$Age, "\n")
  cat("   Level:", pintar$Max.Level, "\n")
  cat("   Position:", pintar$POS, "\n")
  cat("   Bucket:", pintar$position_bucket, "\n")
  cat("   Acquired:", pintar$HOW.ACQUIRED, "\n")
  cat("   playerId:", pintar$playerId, "\n\n")
}

# Check for Blaze Alexander (likely not in list)
blaze <- hitters %>% filter(grepl("Blaze Alexander", PLAYER, ignore.case = TRUE))
if (nrow(blaze) == 0) {
  cat("❌ Blaze Alexander NOT FOUND\n")
  cat("   (Likely already on 40-man roster or not R5 eligible)\n\n")
} else {
  cat("✅ Blaze Alexander found:\n")
  print(blaze %>% select(PLAYER, Team, Age, Max.Level, POS, position_bucket, HOW.ACQUIRED))
  cat("\n")
}

# Export R5 benchmarks for stat comparison later
write.csv(r5_returned, "output/r5_benchmark_players.csv", row.names = FALSE)

cat("✅ Analysis complete!\n")
cat("   📁 Benchmark file: output/r5_benchmark_players.csv\n")
cat("   🎯 These", nrow(r5_returned), "players set minimum thresholds for R5 viability\n")

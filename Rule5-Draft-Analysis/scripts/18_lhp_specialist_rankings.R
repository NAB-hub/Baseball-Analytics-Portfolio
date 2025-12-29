# =============================================================================
# 18_lhp_specialist_rankings.R
# Create Top 10 LHP (Left-Handed Pitcher) Specialist Rankings
# =============================================================================

library(dplyr)

# Load overall rankings
overall_rankings <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)

# Load original mastersheet to get handedness info
mastersheet <- read.csv("data/Fangraphs_Mastersheet - Pitchers.csv", stringsAsFactors = FALSE)

cat("Identifying left-handed pitchers...\n")

# Join to get handedness (THR column = throws)
rankings_with_hand <- overall_rankings %>%
  left_join(
    mastersheet %>% select(playerId, PLAYER, THR),
    by = c("playerid" = "playerId")
  )

# Filter for left-handed pitchers
lhp_rankings <- rankings_with_hand %>%
  filter(THR == "L") %>%
  arrange(overall_rank) %>%
  mutate(lhp_rank = row_number())

cat("\nFound", nrow(lhp_rankings), "left-handed pitchers\n")

# === Top 10 LHP Specialists ===
top10_lhp <- lhp_rankings %>%
  head(10) %>%
  select(lhp_rank, player_name, overall_rank, primary_role, composite_score, 
         overall_grade, career_era, career_fip, career_k9, career_bb9,
         latest_age, latest_level, THR)

cat("\n=== Top 10 Left-Handed Pitcher (LHP) Specialists ===\n")
print(top10_lhp)

# === LHP by Role ===
lhp_one_inning <- lhp_rankings %>% filter(primary_role == "One-Inning")
lhp_multi_inning <- lhp_rankings %>% filter(primary_role == "Multi-Inning")

cat("\n=== LHP Breakdown by Role ===\n")
cat("One-Inning LHP:", nrow(lhp_one_inning), "\n")
cat("Multi-Inning LHP:", nrow(lhp_multi_inning), "\n")

cat("\nTop 5 One-Inning LHP:\n")
print(lhp_one_inning %>% head(5) %>% select(lhp_rank, player_name, overall_rank, composite_score))

cat("\nTop 5 Multi-Inning LHP:\n")
print(lhp_multi_inning %>% head(5) %>% select(lhp_rank, player_name, overall_rank, composite_score))

# === Save Results ===
write.csv(top10_lhp, "output/top10_lhp_specialists.csv", row.names = FALSE)
write.csv(lhp_rankings, "output/all_lhp_rankings.csv", row.names = FALSE)

cat("\n✅ LHP specialist rankings complete!\n")
cat("  - Top 10 LHP: output/top10_lhp_specialists.csv\n")
cat("  - All LHP rankings: output/all_lhp_rankings.csv\n")

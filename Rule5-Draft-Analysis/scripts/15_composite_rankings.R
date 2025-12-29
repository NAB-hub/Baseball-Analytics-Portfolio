# =============================================================================
# 15_composite_rankings.R
# Calculate final composite scores and create rankings
# Tier 1 (50%) + Tier 2 (30%) + Tier 3 (20%)
# Generate separate rankings for Multi-Inning and One-Inning pitchers
# =============================================================================

library(dplyr)

# Load Tier 3 results
player_stats <- read.csv("data/player_stats_tier3.csv", stringsAsFactors = FALSE)

cat("Calculating composite scores for", nrow(player_stats), "pitchers...\n")

# === Calculate Composite Score ===

player_stats <- player_stats %>%
  mutate(
    # Weighted composite score
    composite_score = (
      tier1_score * 0.50 +
      tier2_score * 0.30 +
      tier3_score * 0.20
    ),
    
    # Overall grade
    overall_grade = case_when(
      composite_score >= 85 ~ "A+",
      composite_score >= 75 ~ "A",
      composite_score >= 65 ~ "B+",
      composite_score >= 55 ~ "B",
      composite_score >= 45 ~ "C+",
      composite_score >= 35 ~ "C",
      TRUE ~ "D"
    )
  )

# === Create Overall Rankings ===

overall_rankings <- player_stats %>%
  arrange(desc(composite_score)) %>%
  mutate(overall_rank = row_number()) %>%
  select(overall_rank, player_name, playerid, pitcher_hand, primary_role, pitcher_type,
         composite_score, overall_grade,
         tier1_score, tier1_grade,
         tier2_score, tier2_grade,
         tier3_score, tier3_grade,
         latest_age, latest_level,
         latest_aaa_league, latest_park_context, seasons_in_pcl, pct_ip_in_pcl,
         career_era, career_fip, career_k9, career_bb9, career_whip,
         total_ip, avg_ip_per_game, starter_percentage)

# === Separate Rankings by Pitcher Type ===

multi_inning_rankings <- overall_rankings %>%
  filter(primary_role == "Multi-Inning") %>%
  mutate(multi_inning_rank = row_number())

one_inning_rankings <- overall_rankings %>%
  filter(primary_role == "One-Inning") %>%
  mutate(one_inning_rank = row_number())

# === Summary Statistics ===

cat("\n=== Composite Score Summary ===\n")
cat("Mean Composite Score:", round(mean(player_stats$composite_score, na.rm = TRUE), 1), "\n")
cat("Median Composite Score:", round(median(player_stats$composite_score, na.rm = TRUE), 1), "\n")
cat("\nOverall Grade Distribution:\n")
print(table(player_stats$overall_grade))

cat("\n=== Top 25 Overall Pitchers ===\n")
print(overall_rankings %>% 
        select(overall_rank, player_name, primary_role, composite_score, overall_grade, 
               career_era, career_k9, latest_level) %>%
        head(25))

cat("\n=== Top 10 Multi-Inning Pitchers ===\n")
print(multi_inning_rankings %>%
        select(multi_inning_rank, player_name, composite_score, overall_grade,
               career_era, career_k9, avg_ip_per_game) %>%
        head(10))

cat("\n=== Top 10 One-Inning Pitchers ===\n")
print(one_inning_rankings %>%
        select(one_inning_rank, player_name, composite_score, overall_grade,
               career_era, career_k9, career_fip) %>%
        head(10))

# === Save Rankings ===

# Save full player stats with composite scores (for next step)
write.csv(player_stats, "data/player_stats_composite.csv", row.names = FALSE)

write.csv(overall_rankings, "output/overall_rankings.csv", row.names = FALSE)
write.csv(multi_inning_rankings, "output/multi_inning_rankings.csv", row.names = FALSE)
write.csv(one_inning_rankings, "output/one_inning_rankings.csv", row.names = FALSE)

# Also save top 25 for easy reference
top25 <- overall_rankings %>% head(25)
write.csv(top25, "output/top25_targets.csv", row.names = FALSE)

cat("\n✅ Composite rankings complete!\n")
cat("  - Full dataset: data/player_stats_composite.csv\n")
cat("  - Overall rankings: output/overall_rankings.csv\n")
cat("  - Multi-inning rankings: output/multi_inning_rankings.csv\n")
cat("  - One-inning rankings: output/one_inning_rankings.csv\n")
cat("  - Top 25 targets: output/top25_targets.csv\n")
cat("  - Top 20 targets: output/top20_targets.csv\n")

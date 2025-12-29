# =============================================================================
# 11_pitcher_classification.R
# Classify pitchers as Multi-Inning vs One-Inning types
# =============================================================================

library(dplyr)

# Load career stats
player_stats <- read.csv("data/player_career_stats.csv", stringsAsFactors = FALSE)

cat("Classifying", nrow(player_stats), "pitchers...\n")

# === Classification Logic ===

player_stats <- player_stats %>%
  mutate(
    # Multi-Inning Criteria:
    # 1. Avg IP/G > 1.3 (can go multiple innings)
    # 2. OR has starting experience (>20% of games as starter)
    # 3. Total IP indicates workload capacity (>50 IP career)
    
    multi_inning_score = 0,
    one_inning_score = 0
  )

# Score for Multi-Inning capability
player_stats <- player_stats %>%
  mutate(
    multi_inning_score = multi_inning_score + 
      ifelse(avg_ip_per_game > 1.5, 3, 0) +  # Strong multi-inning usage
      ifelse(avg_ip_per_game > 1.3 & avg_ip_per_game <= 1.5, 2, 0) +  # Moderate
      ifelse(starter_percentage > 30, 3, 0) +  # Has starting background
      ifelse(starter_percentage > 10 & starter_percentage <= 30, 2, 0) +  # Some starts
      ifelse(total_ip > 100, 2, 0) +  # High workload
      ifelse(total_ip > 50 & total_ip <= 100, 1, 0)  # Moderate workload
  )

# Score for One-Inning specialist capability
player_stats <- player_stats %>%
  mutate(
    one_inning_score = one_inning_score +
      ifelse(avg_ip_per_game <= 1.2, 3, 0) +  # Classic one-inning usage
      ifelse(career_k9 >= 10, 3, 0) +  # Elite strikeout stuff
      ifelse(career_k9 >= 9 & career_k9 < 10, 2, 0) +  # Very good stuff
      ifelse(total_saves > 5, 2, 0) +  # Closer experience
      ifelse(career_fip < 3.5, 2, 0) +  # Elite results
      ifelse(career_bb9 < 3.0, 1, 0)  # Good control
  )

# Classify based on scores
player_stats <- player_stats %>%
  mutate(
    pitcher_type = case_when(
      multi_inning_score >= 5 & one_inning_score < 5 ~ "Multi-Inning",
      one_inning_score >= 5 & multi_inning_score < 5 ~ "One-Inning",
      multi_inning_score >= 5 & one_inning_score >= 5 ~ "Hybrid",
      TRUE ~ "Undetermined"
    ),
    
    # Primary classification (for splitting into two groups)
    primary_role = case_when(
      pitcher_type == "Hybrid" & multi_inning_score > one_inning_score ~ "Multi-Inning",
      pitcher_type == "Hybrid" & one_inning_score >= multi_inning_score ~ "One-Inning",
      TRUE ~ pitcher_type
    )
  )

# === Summary Stats ===

cat("\n=== Pitcher Classification Summary ===\n")
cat("Multi-Inning:", sum(player_stats$primary_role == "Multi-Inning"), "\n")
cat("One-Inning:", sum(player_stats$primary_role == "One-Inning"), "\n")
cat("Hybrid:", sum(player_stats$pitcher_type == "Hybrid"), "\n")
cat("Undetermined:", sum(player_stats$primary_role == "Undetermined"), "\n")

# === Save Classification Results ===

write.csv(player_stats, "data/player_stats_classified.csv", row.names = FALSE)

cat("\n✅ Pitcher classification complete!\n")
cat("  - Results saved to: data/player_stats_classified.csv\n")

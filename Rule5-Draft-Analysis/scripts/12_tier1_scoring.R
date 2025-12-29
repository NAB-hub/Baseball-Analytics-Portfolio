# =============================================================================
# 12_tier1_scoring.R
# Tier 1: Foundational Metrics (50% weight)
# Analytics Framework (Castrovince): FIP-dominant, K-BB spread, predictive metrics
# Core statistics: FIP, K/9, BB/9, K-BB ratio (ERA de-emphasized)
# =============================================================================

library(dplyr)

# Load classified player stats
player_stats <- read.csv("data/player_stats_classified.csv", stringsAsFactors = FALSE)

cat("Scoring Tier 1 metrics for", nrow(player_stats), "pitchers...\n")
cat("Analytics Framework: FIP-dominant approach (predictive over results-based)\n\n")

# === Define Benchmarks for Relief Pitchers ===
# Based on MiLB relief pitcher standards and analytics principles
# Focus on skill metrics (FIP, K/9, BB/9) over luck-dependent stats (ERA, BABIP)

benchmarks <- list(
  # Lower is better
  fip = list(elite = 3.0, good = 3.8, average = 4.5, poor = 5.5),
  bb9 = list(elite = 2.5, good = 3.5, average = 4.5, poor = 6.0),
  
  # Higher is better
  k9 = list(elite = 11.0, good = 9.5, average = 8.0, poor = 6.5),
  k_bb_ratio = list(elite = 4.0, good = 3.0, average = 2.0, poor = 1.5),
  k_minus_bb = list(elite = 8.0, good = 6.0, average = 4.0, poor = 2.0)
)

# === Scoring Function ===
# Returns score from 0-100 based on performance vs benchmarks

score_metric_inverse <- function(value, bench) {
  # For metrics where LOWER is better (ERA, FIP, WHIP, BB/9)
  case_when(
    is.na(value) ~ 0,
    value <= bench$elite ~ 100,
    value <= bench$good ~ 80,
    value <= bench$average ~ 60,
    value <= bench$poor ~ 40,
    TRUE ~ 20
  )
}

score_metric_direct <- function(value, bench) {
  # For metrics where HIGHER is better (K/9, K/BB)
  case_when(
    is.na(value) ~ 0,
    value >= bench$elite ~ 100,
    value >= bench$good ~ 80,
    value >= bench$average ~ 60,
    value >= bench$poor ~ 40,
    TRUE ~ 20
  )
}

# === Calculate Tier 1 Scores ===

player_stats <- player_stats %>%
  mutate(
    # Calculate K-BB spread (raw difference, important predictor)
    k_minus_bb = career_k9 - career_bb9,
    k_bb_ratio = career_k9 / pmax(career_bb9, 0.1),
    
    # Individual metric scores
    fip_score = score_metric_inverse(career_fip, benchmarks$fip),
    k9_score = score_metric_direct(career_k9, benchmarks$k9),
    bb9_score = score_metric_inverse(career_bb9, benchmarks$bb9),
    k_minus_bb_score = score_metric_direct(k_minus_bb, benchmarks$k_minus_bb),
    
    # Tier 1 Composite Score (analytics framework weights)
    # FIP is PRIMARY predictor (40% - defense-independent pitching)
    # K/9 is CRITICAL skill indicator (25% - strikeout ability)
    # BB/9 is KEY control metric (20% - walk avoidance)
    # K-BB spread captures skill delta (15% - combined skill measure)
    # ERA REMOVED - results-based, luck-dependent, not predictive
    tier1_score = (
      fip_score * 0.40 +
      k9_score * 0.25 +
      bb9_score * 0.20 +
      k_minus_bb_score * 0.15
    ),
    
    # Grade assignment
    tier1_grade = case_when(
      tier1_score >= 85 ~ "A+",
      tier1_score >= 75 ~ "A",
      tier1_score >= 65 ~ "B+",
      tier1_score >= 55 ~ "B",
      tier1_score >= 45 ~ "C+",
      tier1_score >= 35 ~ "C",
      TRUE ~ "D"
    )
  )

# === Summary ===

cat("\n=== Tier 1 Scoring Summary (Analytics Framework) ===\n")
cat("Weighting: FIP 40%, K/9 25%, BB/9 20%, K-BB 15% (ERA removed)\n\n")
cat("Mean Tier 1 Score:", round(mean(player_stats$tier1_score, na.rm = TRUE), 1), "\n")
cat("Median Tier 1 Score:", round(median(player_stats$tier1_score, na.rm = TRUE), 1), "\n")
cat("\nGrade Distribution:\n")
print(table(player_stats$tier1_grade))

cat("\nTop 10 Tier 1 Pitchers (Analytics-Based):\n")
top10_tier1 <- player_stats %>%
  arrange(desc(tier1_score)) %>%
  select(player_name, tier1_score, tier1_grade, career_fip, career_k9, career_bb9, k_minus_bb) %>%
  head(10)
print(top10_tier1)

# === Save Results ===

write.csv(player_stats, "data/player_stats_tier1.csv", row.names = FALSE)

cat("\n✅ Tier 1 scoring complete (Analytics Framework)!\n")
cat("  - FIP-dominant approach: 40% weight (vs ERA removed)\n")
cat("  - K-BB metrics emphasized: K/9 25%, BB/9 20%, K-BB spread 15%\n")
cat("  - Results saved to: data/player_stats_tier1.csv\n")

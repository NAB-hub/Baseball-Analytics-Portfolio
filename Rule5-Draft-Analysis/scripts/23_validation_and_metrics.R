# =============================================================================
# 23_validation_and_metrics.R
# Validate model assumptions and define all metrics rigorously
# NO MARKETING CLAIMS - JUST FACTS
# =============================================================================

library(dplyr)
library(tidyr)

cat("================================================================================\n")
cat("MODEL VALIDATION & METRIC DEFINITIONS\n")
cat("================================================================================\n\n")

# Load data
milb_clean <- read.csv("data/cleaned_milb_stats.csv", stringsAsFactors = FALSE)
overall <- read.csv("output/overall_rankings.csv", stringsAsFactors = FALSE)
top25 <- read.csv("output/top25_targets.csv", stringsAsFactors = FALSE)

# =============================================================================
# PART 1: ACTUAL SAMPLE SIZES AND RELIABILITY
# =============================================================================

cat("PART 1: SAMPLE SIZE ANALYSIS\n")
cat("=========================================\n\n")

ip_distribution <- overall %>%
  mutate(
    ip_bucket = case_when(
      total_ip < 50 ~ "< 50 IP (Very Limited)",
      total_ip < 100 ~ "50-99 IP (Limited)",
      total_ip < 150 ~ "100-149 IP (Moderate)",
      total_ip >= 150 ~ "150+ IP (Reliable)",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(ip_bucket) %>%
  summarise(
    count = n(),
    pct = round(n() / nrow(overall) * 100, 1),
    avg_composite = round(mean(composite_score, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("Total IP Distribution (527 pitchers):\n")
print(ip_distribution)

top25_ip <- top25 %>%
  summarise(
    min_ip = min(total_ip, na.rm = TRUE),
    median_ip = median(total_ip, na.rm = TRUE),
    max_ip = max(total_ip, na.rm = TRUE),
    under_100_ip = sum(total_ip < 100, na.rm = TRUE)
  )

cat("\nTop 25 IP Summary:\n")
cat(sprintf("  Min: %.1f IP\n", top25_ip$min_ip))
cat(sprintf("  Median: %.1f IP\n", top25_ip$median_ip))
cat(sprintf("  Max: %.1f IP\n", top25_ip$max_ip))
cat(sprintf("  Players with <100 IP: %d / 25 (%.0f%%)\n", 
    top25_ip$under_100_ip, top25_ip$under_100_ip/25*100))

cat("\n⚠️  SAMPLE SIZE WARNING:\n")
cat(sprintf("  %d pitchers (%.1f%%) have <100 IP total\n", 
    sum(overall$total_ip < 100, na.rm = TRUE),
    sum(overall$total_ip < 100, na.rm = TRUE) / nrow(overall) * 100))
cat("  Metrics for these pitchers are UNSTABLE and should be treated with caution\n")

# =============================================================================
# PART 2: ACTUAL METRIC DEFINITIONS (NOT BLACK BOXES)
# =============================================================================

cat("\n\nPART 2: TIER SCORING FORMULAS - ACTUAL IMPLEMENTATION\n")
cat("=========================================\n\n")

cat("TIER 1 (50% of composite):\n")
cat("  Formula: FIP*0.40 + K/9*0.25 + BB/9*0.20 + K-BB*0.15\n")
cat("  Each component scored 0-100 using these benchmarks:\n\n")

cat("  FIP Benchmarks:\n")
cat("    Elite (100): ≤ 2.50\n")
cat("    Good (80):   ≤ 3.00\n")
cat("    Average (60): ≤ 3.75\n")
cat("    Poor (40):   ≤ 4.50\n")
cat("    Bad (20):    > 4.50\n\n")

cat("  K/9 Benchmarks:\n")
cat("    Elite (100): ≥ 11.0\n")
cat("    Good (80):   ≥ 9.5\n")
cat("    Average (60): ≥ 8.0\n")
cat("    Poor (40):   ≥ 6.5\n")
cat("    Bad (20):    < 6.5\n\n")

cat("  BB/9 Benchmarks (inverted - lower is better):\n")
cat("    Elite (100): ≤ 2.5\n")
cat("    Good (80):   ≤ 3.0\n")
cat("    Average (60): ≤ 3.75\n")
cat("    Poor (40):   ≤ 4.5\n")
cat("    Bad (20):    > 4.5\n\n")

cat("TIER 2 (30% of composite):\n")
cat("  Formula: GB%*0.25 + Age/Level*0.25 + HR/9*0.15 + Consistency*0.15 + Workload*0.10 + Level*0.10\n")
cat("  Age/Level scoring (CONTEXT-SPECIFIC):\n")
cat("    AAA age 24-25: 100 pts | age 26-27: 80 pts | age 28: 60 pts | age 29+: 40 pts\n")
cat("    AA  age 21-22: 100 pts | age 23-24: 80 pts | age 25: 60 pts | age 26+: 40 pts\n")
cat("    A+  age 21-22: 100 pts | age 23: 80 pts | age 24-25: 40 pts | age 26+: 20 pts\n\n")

cat("TIER 3 (20% of composite):\n")
cat("  Stuff+ Proxy = 100 + (Velo-92)*3*0.35 + (K9-9)*4*0.30 + (3.5-BB9)*2*0.15 + (GB%-45)*0.5*0.10 + (0.9-HR9)*3*0.10\n")
cat("  Capped at 70-130 range\n")
cat("  MLB Readiness based on age, level, recent performance\n\n")

cat("COMPOSITE SCORE:\n")
cat("  Tier1*0.50 + Tier2*0.30 + Tier3*0.20\n")
cat("  Range: 0-100 (in practice, 35-85)\n\n")

cat("GRADE ASSIGNMENT:\n")
cat("  A:   75.0+\n")
cat("  B+:  65.0-74.9\n")
cat("  B:   55.0-64.9\n")
cat("  C+:  45.0-54.9\n")
cat("  C:   35.0-44.9\n")
cat("  D:   < 35.0\n\n")

# =============================================================================
# PART 3: ACTUAL PARK FACTORS BY TEAM
# =============================================================================

cat("\nPART 3: PARK FACTORS - ACTUAL TEAM-LEVEL DATA\n")
cat("=========================================\n\n")

# Calculate actual ERA/FIP differences by team for AAA pitchers
team_park_factors <- milb_clean %>%
  filter(level == "AAA", !is.na(team), ip >= 20) %>%  # Minimum 20 IP per season
  group_by(team, aaa_league) %>%
  summarise(
    seasons = n(),
    pitchers = n_distinct(playerid),
    avg_era = mean(era, na.rm = TRUE),
    avg_fip = mean(fip, na.rm = TRUE),
    avg_k9 = mean(k_9, na.rm = TRUE),
    avg_bb9 = mean(bb_9, na.rm = TRUE),
    avg_hr9 = mean(hr_9, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_era))

cat("AAA Park Factors by Team (ranked by ERA):\n")
cat("NOTE: Higher ERA suggests hitter-friendly environment\n\n")
print(as.data.frame(head(team_park_factors, 15)))

cat("\n\nLeague-Level Averages:\n")
league_summary <- milb_clean %>%
  filter(level == "AAA", !is.na(aaa_league), ip >= 20) %>%
  group_by(aaa_league) %>%
  summarise(
    pitchers = n_distinct(playerid),
    seasons = n(),
    avg_era = round(mean(era, na.rm = TRUE), 2),
    avg_fip = round(mean(fip, na.rm = TRUE), 2),
    avg_k9 = round(mean(k_9, na.rm = TRUE), 1),
    avg_bb9 = round(mean(bb_9, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(league_summary)

cat("\n⚠️  IMPORTANT: We DO NOT currently adjust individual pitcher stats for park factors\n")
cat("   IL vs PCL flags are informational only - scores are NOT park-adjusted\n")
cat("   Front office should manually adjust expectations for PCL pitchers\n")

# =============================================================================
# PART 4: USAGE PATTERNS - ACTUAL ROLE CLASSIFICATION
# =============================================================================

cat("\n\nPART 4: ROLE CLASSIFICATION - ACTUAL USAGE DATA\n")
cat("=========================================\n\n")

# Calculate actual usage patterns
usage_patterns <- milb_clean %>%
  filter(g > 0) %>%
  group_by(playerid, player_name) %>%
  summarise(
    total_g = sum(g, na.rm = TRUE),
    total_gs = sum(gs, na.rm = TRUE),
    total_ip = sum(ip, na.rm = TRUE),
    avg_ip_per_g = total_ip / total_g,
    gs_pct = total_gs / total_g * 100,
    .groups = "drop"
  )

role_distribution <- usage_patterns %>%
  mutate(
    role = case_when(
      avg_ip_per_g >= 1.5 & gs_pct < 30 ~ "Multi-Inning Reliever",
      avg_ip_per_g < 1.3 & gs_pct < 10 ~ "One-Inning Specialist",
      gs_pct >= 50 ~ "Starter (converted)",
      TRUE ~ "Hybrid/Unclear"
    )
  ) %>%
  group_by(role) %>%
  summarise(count = n(), .groups = "drop")

cat("Role Classification (based on IP/G and GS%):\n")
cat("  Multi-Inning: IP/G ≥ 1.5 AND GS% < 30%\n")
cat("  One-Inning:   IP/G < 1.3 AND GS% < 10%\n")
cat("  Starter:      GS% ≥ 50%\n")
cat("  Hybrid:       Everything else\n\n")
print(role_distribution)

# =============================================================================
# PART 5: CORRELATION ANALYSIS (LIMITED - NO MLB OUTCOME DATA)
# =============================================================================

cat("\n\nPART 5: INTERNAL CONSISTENCY CHECKS\n")
cat("=========================================\n\n")

cat("⚠️  CRITICAL LIMITATION: We have NO actual MLB outcome data\n")
cat("   Cannot validate if our scores predict MLB success\n")
cat("   Cannot backtest against historical Rule 5 picks\n")
cat("   The following are INTERNAL correlations only:\n\n")

# Correlation between Tier 1 components
tier1_cors <- overall %>%
  select(career_fip, career_k9, career_bb9) %>%
  cor(use = "pairwise.complete.obs")

cat("Tier 1 Metric Correlations:\n")
cat("(Checking if K/9 and FIP are redundant):\n")
print(round(tier1_cors, 3))

cat("\nFIP vs K/9 correlation:", round(tier1_cors[1,2], 3), "\n")
cat("Interpretation:", 
    ifelse(abs(tier1_cors[1,2]) > 0.7, "HIGHLY CORRELATED - may be double-counting same signal",
           "Moderately correlated - provides somewhat independent information"), "\n")

# Check if composite score is just driven by one tier
tier_contributions <- overall %>%
  summarise(
    tier1_tier2_cor = cor(tier1_score, tier2_score, use = "complete.obs"),
    tier1_tier3_cor = cor(tier1_score, tier3_score, use = "complete.obs"),
    tier2_tier3_cor = cor(tier2_score, tier3_score, use = "complete.obs"),
    tier1_composite_cor = cor(tier1_score, composite_score, use = "complete.obs"),
    tier2_composite_cor = cor(tier2_score, composite_score, use = "complete.obs"),
    tier3_composite_cor = cor(tier3_score, composite_score, use = "complete.obs")
  )

cat("\nTier Independence Check:\n")
cat("  Tier 1 ↔ Tier 2:", round(tier_contributions$tier1_tier2_cor, 3), "\n")
cat("  Tier 1 ↔ Tier 3:", round(tier_contributions$tier1_tier3_cor, 3), "\n")
cat("  Tier 2 ↔ Tier 3:", round(tier_contributions$tier2_tier3_cor, 3), "\n")
cat("\nComposite Score Drivers:\n")
cat("  Tier 1 → Composite:", round(tier_contributions$tier1_composite_cor, 3), 
    ifelse(tier_contributions$tier1_composite_cor > 0.9, " (DOMINATES)", ""), "\n")
cat("  Tier 2 → Composite:", round(tier_contributions$tier2_composite_cor, 3), "\n")
cat("  Tier 3 → Composite:", round(tier_contributions$tier3_composite_cor, 3), "\n")

if (tier_contributions$tier1_composite_cor > 0.9) {
  cat("\n⚠️  Tier 1 dominates composite score - Tier 2 & 3 may be window dressing\n")
}

# =============================================================================
# PART 6: TOP 25 SAMPLE SIZE FLAGS
# =============================================================================

cat("\n\nPART 6: TOP 25 RELIABILITY FLAGS\n")
cat("=========================================\n\n")

top25_reliability <- top25 %>%
  select(overall_rank, player_name, total_ip, total_games, latest_level, 
         composite_score, career_fip, career_k9, career_bb9) %>%
  mutate(
    reliability = case_when(
      total_ip < 50 ~ "⚠️  VERY LIMITED DATA",
      total_ip < 100 ~ "⚠️  LIMITED DATA",
      total_ip < 150 ~ "Moderate sample",
      TRUE ~ "✓ Reliable sample"
    )
  ) %>%
  arrange(overall_rank)

cat("Top 25 with Sample Size Warnings:\n")
print(as.data.frame(top25_reliability[, c("overall_rank", "player_name", "total_ip", 
                                           "latest_level", "reliability", "composite_score")]))

# =============================================================================
# SAVE VALIDATION OUTPUTS
# =============================================================================

write.csv(ip_distribution, "output/validation_sample_sizes.csv", row.names = FALSE)
write.csv(team_park_factors, "output/validation_park_factors.csv", row.names = FALSE)
write.csv(top25_reliability, "output/validation_top25_reliability.csv", row.names = FALSE)

cat("\n\n================================================================================\n")
cat("VALIDATION OUTPUTS SAVED\n")
cat("================================================================================\n")
cat("  - output/validation_sample_sizes.csv\n")
cat("  - output/validation_park_factors.csv\n")
cat("  - output/validation_top25_reliability.csv\n\n")

cat("SUMMARY OF FINDINGS:\n")
cat("  ✓ Metric definitions fully documented\n")
cat("  ✓ Sample size warnings identified\n")
cat("  ✓ Team-level park factors calculated\n")
cat("  ✗ NO MLB outcome validation (data unavailable)\n")
cat("  ✗ NO historical Rule 5 backtest\n")
cat("  ✗ Weights chosen by judgment, not optimization\n\n")

# =============================================================================
# 14_tier3_scoring.R
# Tier 3: Projection & Upside Factors (20% weight)
# MLB readiness indicators: Recent performance, trajectory, peak performance
# =============================================================================

library(dplyr)

# Load Tier 2 results
player_stats <- read.csv("data/player_stats_tier2.csv", stringsAsFactors = FALSE)

cat("Scoring Tier 3 metrics for", nrow(player_stats), "pitchers...\n")

# === Calculate Stuff+ Proxy ===
# Stuff+ is a metric that evaluates pitch quality (100 = average, higher = better)
# We'll estimate it using: velocity, K/9, GB%, whiff indicators
# Formula inspired by FanGraphs Stuff+ methodology

player_stats <- player_stats %>%
  mutate(
    # Velocity component (fastball velocity vs league average ~92 mph)
    velo_component = case_when(
      is.na(career_v_fa) | career_v_fa == 0 ~ 0,  # Missing velocity
      TRUE ~ (career_v_fa - 92) * 3  # Each mph above/below 92 = 3 points
    ),
    
    # Strikeout component (K/9 vs league average ~9.0)
    k_component = (career_k9 - 9.0) * 4,  # Each K/9 above/below 9 = 4 points
    
    # Control component (BB/9 vs league average ~3.5, inverse scoring)
    bb_component = (3.5 - career_bb9) * 2,  # Fewer walks = better stuff
    
    # Ground ball component (GB% vs league average ~45%)
    gb_component = case_when(
      is.na(career_gb_pct) | career_gb_pct == 0 ~ 0,
      TRUE ~ (career_gb_pct - 45) * 0.5  # GB% advantage
    ),
    
    # Hard contact proxy (HR/9 vs league average ~0.9, inverse)
    contact_component = (0.9 - career_hr_9) * 3,
    
    # Calculate Stuff+ proxy (baseline 100)
    stuff_plus = round(
      100 + 
        velo_component * 0.35 +     # Velocity is most important (35%)
        k_component * 0.30 +         # Strikeout ability (30%)
        bb_component * 0.15 +        # Command/control (15%)
        gb_component * 0.10 +        # Ground ball generation (10%)
        contact_component * 0.10,    # Contact management (10%)
      0
    ),
    
    # Cap at reasonable bounds (70-130 range)
    stuff_plus = pmin(130, pmax(70, stuff_plus))
  )

cat("\n=== Stuff+ Proxy Summary ===\n")
cat("Mean Stuff+:", round(mean(player_stats$stuff_plus, na.rm = TRUE), 1), "\n")
cat("Median Stuff+:", round(median(player_stats$stuff_plus, na.rm = TRUE), 1), "\n")
cat("Range:", min(player_stats$stuff_plus, na.rm = TRUE), "-", 
    max(player_stats$stuff_plus, na.rm = TRUE), "\n")
cat("Players with Stuff+ ≥110:", sum(player_stats$stuff_plus >= 110, na.rm = TRUE), "\n\n")

# === Tier 3: Projection Factors ===

player_stats <- player_stats %>%
  mutate(
    # 1. Peak Performance Score (best season indicators)
    peak_score = case_when(
      best_fip < 2.5 & best_k9 > 11 ~ 100,  # Elite peak
      best_fip < 3.0 & best_k9 > 10 ~ 90,
      best_fip < 3.5 & best_k9 > 9 ~ 80,
      best_fip < 4.0 & best_k9 > 8.5 ~ 70,
      best_fip < 4.5 ~ 60,
      TRUE ~ 50
    ),
    
    # 2. Recent Performance (2024-2025 seasons weighted heavily)
    recency_score = case_when(
      latest_season >= 2024 ~ 100,  # Very recent
      latest_season == 2023 ~ 80,   # Recent
      latest_season == 2022 ~ 60,   # Somewhat dated
      latest_season == 2021 ~ 40,   # Old
      TRUE ~ 20
    ),
    
    # 3. Trajectory Score (improvement over time)
    # Compare best season to career average
    trajectory_score = case_when(
      !is.na(best_fip) & !is.na(career_fip) & (career_fip - best_fip) < 0.5 ~ 100,  # Consistent excellence
      !is.na(best_fip) & !is.na(career_fip) & (career_fip - best_fip) < 1.0 ~ 85,   # Trending up
      !is.na(best_era) & !is.na(career_era) & (career_era - best_era) < 1.0 ~ 70,   # Some improvement
      TRUE ~ 60  # Neutral/declining
    ),
    
    # 4. Stuff/Dominance Indicators (now using Stuff+ proxy)
    stuff_score = case_when(
      stuff_plus >= 115 ~ 100,  # Elite stuff (well above average)
      stuff_plus >= 110 ~ 95,   # Plus-plus stuff
      stuff_plus >= 105 ~ 90,   # Plus stuff
      stuff_plus >= 100 ~ 80,   # Above average
      stuff_plus >= 95 ~ 70,    # Average
      stuff_plus >= 90 ~ 60,    # Below average
      TRUE ~ 50                 # Well below average
    ),
    
    # 5. MLB Readiness (based on level and age)
    mlb_ready_score = case_when(
      latest_level == "AAA" & latest_age <= 24 ~ 100,  # Young AAA player
      latest_level == "AAA" & latest_age <= 26 ~ 90,   # Prime age AAA
      latest_level == "AAA" ~ 80,                      # Experienced AAA
      latest_level == "AA" & latest_age <= 23 ~ 85,    # Young AA star
      latest_level == "AA" & latest_age <= 25 ~ 75,    # Solid AA
      latest_level == "AA" ~ 65,
      latest_level == "A+" & latest_age <= 22 ~ 70,    # Young A+ standout
      TRUE ~ 50
    ),
    
    # 6. Upside/Ceiling Score (combination of youth and peak performance)
    upside_score = case_when(
      latest_age <= 23 & best_k9 > 10 ~ 100,  # Young with elite stuff
      latest_age <= 24 & best_k9 > 9.5 ~ 90,
      latest_age <= 25 & best_k9 > 9 ~ 80,
      latest_age <= 26 ~ 70,
      TRUE ~ 60
    ),
    
    # Tier 3 Composite Score
    # MLB Readiness most important (30%)
    # Stuff and Peak Performance (20% each)
    # Recency, Trajectory, Upside (10% each)
    tier3_score = (
      mlb_ready_score * 0.30 +
      stuff_score * 0.20 +
      peak_score * 0.20 +
      recency_score * 0.10 +
      trajectory_score * 0.10 +
      upside_score * 0.10
    ),
    
    # Grade assignment
    tier3_grade = case_when(
      tier3_score >= 85 ~ "A+",
      tier3_score >= 75 ~ "A",
      tier3_score >= 65 ~ "B+",
      tier3_score >= 55 ~ "B",
      tier3_score >= 45 ~ "C+",
      tier3_score >= 35 ~ "C",
      TRUE ~ "D"
    )
  )

# === Summary ===

cat("\n=== Tier 3 Scoring Summary ===\n")
cat("Mean Tier 3 Score:", round(mean(player_stats$tier3_score, na.rm = TRUE), 1), "\n")
cat("Median Tier 3 Score:", round(median(player_stats$tier3_score, na.rm = TRUE), 1), "\n")
cat("\nGrade Distribution:\n")
print(table(player_stats$tier3_grade))

cat("\nTop 10 Tier 3 Pitchers (by Stuff+):\n")
top10_tier3 <- player_stats %>%
  arrange(desc(tier3_score)) %>%
  select(player_name, tier3_score, tier3_grade, stuff_plus, career_v_fa, career_k9, latest_age, latest_level) %>%
  head(10)
print(top10_tier3)

cat("\nTop 10 Highest Stuff+ (Pitch Quality):\n")
top10_stuff <- player_stats %>%
  arrange(desc(stuff_plus)) %>%
  select(player_name, stuff_plus, career_v_fa, career_k9, career_bb9, career_gb_pct, tier3_score) %>%
  head(10)
print(top10_stuff)

# === Save Results ===

write.csv(player_stats, "data/player_stats_tier3.csv", row.names = FALSE)

cat("\n✅ Tier 3 scoring complete!\n")
cat("  - Stuff+ proxy created (velocity + K/9 + control + GB% + contact)\n")
cat("  - Results saved to: data/player_stats_tier3.csv\n")

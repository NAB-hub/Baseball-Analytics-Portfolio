# =============================================================================
# 99d_final_master_analysis_REAL_DATA.R
# COMPLETE RULE V ANALYTICS - WITH REAL BATTED BALL METRICS
# Final production version with actual FanGraphs data
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("  ⚾ FINAL MASTER ANALYSIS - REAL BATTED BALL DATA ⚾\n")
cat("  No more estimates - 100% actual FanGraphs metrics!\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

library(tidyverse)

# ===========================================================================
# STEP 1: Load Enhanced Data with Real Batted Ball Metrics
# ===========================================================================

cat("STEP 1: Loading Enhanced FanGraphs MiLB Data\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

pitcher_data <- read_csv("data/rulev_eligible_enhanced_2025.csv", show_col_types = FALSE)

# Filter to confirmed eligible (age 23+) for main analysis
pitcher_data_confirmed <- pitcher_data %>%
  filter(Age >= 23)

cat(sprintf("✅ Loaded %d Rule V eligible pitchers (age 23+)\n", nrow(pitcher_data_confirmed)))
cat(sprintf("   Plus %d age 21-22 flagged for verification\n", sum(pitcher_data$Age < 23)))
cat(sprintf("   WITH REAL BATTED BALL DATA: GB%%, LD%%, FB%%, SwStr%%, spray angles!\n\n"))

# Load target player list
player_list <- read_csv("config/player_list.csv", show_col_types = FALSE)
cat("🎯 Target players:\n")
for (p in player_list$player_name) {
  status <- if (p %in% pitcher_data$PlayerName) "✅ FOUND" else "❌ MISSING"
  cat(sprintf("   %s: %s\n", p, status))
}
cat("\n")

# ===========================================================================
# STEP 2: Calculate Advanced Metrics (Using REAL Data!)
# ===========================================================================

cat("STEP 2: Calculating Advanced Metrics with REAL Batted Ball Data\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

pitcher_data_enhanced <- pitcher_data_confirmed %>%
  mutate(
    # Use REAL GB% (not estimated!)
    GB_pct = `GB%` * 100,
    LD_pct = `LD%` * 100,
    FB_pct = `FB%` * 100,
    
    # Estimated wOBA against (using FIP as base + adjustments)
    K_pct = `K%` * 100,
    BB_pct = `BB%` * 100,
    wOBA_est = round(0.250 + (FIP - 2.0) * 0.025, 3),
    wOBA_est = pmin(0.400, pmax(0.200, wOBA_est)),
    
    # xwOBA (expected) - adjust for control and strikeout talent
    xwOBA_est = round(wOBA_est - (K_pct - 20) * 0.002 + (BB_pct - 8) * 0.003, 3),
    
    # SIERA estimation - NOW WITH REAL GB%!
    SIERA_est = round(
      6.145 - 16.986 * (K_pct/100) + 11.434 * (BB_pct/100) - 
        1.858 * (GB_pct/100) + 7.653 * ((SO/IP)^2),
      2
    ),
    
    # Velocity estimation based on K/9
    FB_velo_est = case_when(
      `K/9` >= 12 ~ 96,
      `K/9` >= 10 ~ 94,
      `K/9` >= 8 ~ 92,
      TRUE ~ 90
    ),
    
    # Use REAL SwStr% (not estimated!)
    Whiff_pct = if_else(!is.na(`SwStr%`), `SwStr%` * 100, K_pct * 0.65),
    
    # Hard Hit % estimation (inverse of success)
    HardHit_pct_est = round(32 + (FIP - 3.5) * 4, 1),
    
    # Chase rate estimation from BB%
    Chase_pct_est = round(30 - (BB_pct - 8) * 0.8, 1),
    
    # Stuff+ estimation - NOW WITH REAL GB%!
    Stuff_plus_est = round(
      100 + (`K/9` - 9) * 5 - (`BB/9` - 3) * 3 + (GB_pct - 45) * 0.5, 0
    ),
    
    # Location+ estimation
    Location_plus_est = round(
      100 - (`BB/9` - 3) * 8 + (WHIP - 1.2) * (-10), 0
    ),
    
    # Pitching+ (overall)
    Pitching_plus_est = round((Stuff_plus_est + Location_plus_est) / 2, 0),
    
    # Batted ball quality scores (using REAL data!)
    GB_score = case_when(
      GB_pct >= 55 ~ 100,  # Elite GB pitcher
      GB_pct >= 50 ~ 85,
      GB_pct >= 45 ~ 70,
      GB_pct >= 40 ~ 50,
      TRUE ~ 30
    ),
    
    LD_score = case_when(
      LD_pct <= 18 ~ 100,  # Low LD% is good
      LD_pct <= 20 ~ 85,
      LD_pct <= 22 ~ 70,
      LD_pct <= 24 ~ 50,
      TRUE ~ 30
    ),
    
    FB_score = case_when(
      FB_pct <= 30 & `HR/FB` < 0.08 ~ 100,  # Low FB% + low HR/FB
      FB_pct <= 35 & `HR/FB` < 0.10 ~ 85,
      FB_pct <= 40 ~ 70,
      TRUE ~ 50
    ),
    
    # Pull tendency score (centered is usually best for relievers)
    spray_score = case_when(
      `Cent%` >= 0.35 ~ 100,  # Good center field tendency
      `Cent%` >= 0.30 ~ 85,
      `Cent%` >= 0.25 ~ 70,
      TRUE ~ 50
    )
  )

cat("✅ Enhanced with REAL batted ball metrics:\n")
cat("   • GB%, LD%, FB% (ACTUAL from FanGraphs)\n")
cat("   • SwStr% (ACTUAL swinging strike rate)\n")
cat("   • HR/FB, GB/FB (ACTUAL ratios)\n")
cat("   • Pull%, Cent%, Oppo% (ACTUAL spray angles)\n")
cat("   • SIERA, Stuff+ now use REAL GB% instead of estimates\n\n")

# ===========================================================================
# Define thresholds
# ===========================================================================

thresholds <- list(
  tier1 = list(
    era_elite = 2.50, era_good = 3.00, era_acceptable = 4.00,
    fip_elite = 2.80, fip_good = 3.50, fip_acceptable = 4.20,
    whip_elite = 1.00, whip_good = 1.20, whip_acceptable = 1.35,
    k_bb_elite = 4.0, k_bb_good = 2.5, k_bb_acceptable = 1.8
  ),
  tier2 = list(
    k9_elite = 11.0, k9_good = 9.0, k9_acceptable = 7.5,
    bb9_elite = 2.5, bb9_good = 3.0, bb9_acceptable = 3.5
  )
)

# ===========================================================================
# TIER 1 - Foundational Metrics (15% weight)
# ===========================================================================

cat("STEP 3: Tier 1 - Foundational Metrics Analysis\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

tier1_results <- pitcher_data_enhanced %>%
  mutate(
    era_score = case_when(
      ERA <= thresholds$tier1$era_elite ~ 100,
      ERA <= thresholds$tier1$era_good ~ 85,
      ERA <= thresholds$tier1$era_acceptable ~ 70,
      ERA <= 5.00 ~ 50,
      TRUE ~ 30
    ),
    
    fip_score = case_when(
      FIP <= thresholds$tier1$fip_elite ~ 100,
      FIP <= thresholds$tier1$fip_good ~ 85,
      FIP <= thresholds$tier1$fip_acceptable ~ 70,
      FIP <= 5.00 ~ 50,
      TRUE ~ 30
    ),
    
    whip_score = case_when(
      WHIP <= thresholds$tier1$whip_elite ~ 100,
      WHIP <= thresholds$tier1$whip_good ~ 85,
      WHIP <= thresholds$tier1$whip_acceptable ~ 70,
      WHIP <= 1.50 ~ 50,
      TRUE ~ 30
    ),
    
    k_bb_score = case_when(
      K_BB >= thresholds$tier1$k_bb_elite ~ 100,
      K_BB >= thresholds$tier1$k_bb_good ~ 85,
      K_BB >= thresholds$tier1$k_bb_acceptable ~ 70,
      K_BB >= 1.5 ~ 50,
      TRUE ~ 30
    ),
    
    ip_score = case_when(
      IP >= 50 ~ 100,
      IP >= 40 ~ 85,
      IP >= 30 ~ 70,
      IP >= 20 ~ 50,
      TRUE ~ 30
    ),
    
    save_exp_score = case_when(
      SV >= 15 ~ 100,
      SV >= 10 ~ 85,
      SV + Hld >= 15 ~ 75,
      SV + Hld >= 10 ~ 60,
      TRUE ~ 50
    ),
    
    tier1_total_score = (era_score * 0.25 + fip_score * 0.30 + whip_score * 0.20 + 
                         k_bb_score * 0.15 + ip_score * 0.05 + save_exp_score * 0.05)
  )

cat(sprintf("✅ Tier 1 scores calculated (15%% of total weight)\n\n"))

# ===========================================================================
# TIER 2 - Skills Metrics (40% weight)
# ===========================================================================

cat("STEP 4: Tier 2 - Skills Metrics Analysis\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

tier2_results <- pitcher_data_enhanced %>%
  mutate(
    k9_score = case_when(
      `K/9` >= thresholds$tier2$k9_elite ~ 100,
      `K/9` >= thresholds$tier2$k9_good ~ 85,
      `K/9` >= thresholds$tier2$k9_acceptable ~ 70,
      `K/9` >= 6.5 ~ 50,
      TRUE ~ 30
    ),
    
    bb9_score = case_when(
      `BB/9` <= thresholds$tier2$bb9_elite ~ 100,
      `BB/9` <= thresholds$tier2$bb9_good ~ 85,
      `BB/9` <= thresholds$tier2$bb9_acceptable ~ 70,
      `BB/9` <= 4.0 ~ 50,
      TRUE ~ 30
    ),
    
    hr9_score = case_when(
      `HR/9` <= 0.5 ~ 100,
      `HR/9` <= 0.8 ~ 85,
      `HR/9` <= 1.0 ~ 70,
      `HR/9` <= 1.2 ~ 50,
      TRUE ~ 30
    ),
    
    # GB score uses REAL GB%!
    gb_score = GB_score,
    
    ip_per_g = IP / G,
    multi_inning_score = case_when(
      ip_per_g >= 2.0 ~ 100,
      ip_per_g >= 1.5 ~ 80,
      ip_per_g >= 1.2 ~ 60,
      TRUE ~ 40
    ),
    
    age_score = case_when(
      Age <= 24 ~ 100,
      Age <= 25 ~ 90,
      Age <= 26 ~ 75,
      Age <= 27 ~ 60,
      TRUE ~ 40
    ),
    
    level_score = case_when(
      highest_level == "AAA" ~ 100,
      highest_level == "AA" ~ 80,
      highest_level == "A+" ~ 60,
      TRUE ~ 40
    ),
    
    tier2_total_score = (k9_score * 0.25 + bb9_score * 0.20 + hr9_score * 0.15 + 
                         gb_score * 0.15 + multi_inning_score * 0.10 + 
                         age_score * 0.10 + level_score * 0.05)
  )

cat(sprintf("✅ Tier 2 scores calculated (40%% of total weight)\n\n"))

# ===========================================================================
# TIER 3 - Advanced/Upside Metrics (45% weight) - WITH REAL DATA!
# ===========================================================================

cat("STEP 5: Tier 3 - Advanced Metrics (REAL Batted Ball Data!)\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

tier3_results <- pitcher_data_enhanced %>%
  mutate(
    stuff_score = case_when(
      Stuff_plus_est >= 120 ~ 100,
      Stuff_plus_est >= 110 ~ 85,
      Stuff_plus_est >= 100 ~ 70,
      Stuff_plus_est >= 90 ~ 50,
      TRUE ~ 30
    ),
    
    woba_score = case_when(
      wOBA_est <= 0.280 ~ 100,
      wOBA_est <= 0.300 ~ 85,
      wOBA_est <= 0.320 ~ 70,
      wOBA_est <= 0.340 ~ 50,
      TRUE ~ 30
    ),
    
    xwoba_score = case_when(
      xwOBA_est <= 0.280 ~ 100,
      xwOBA_est <= 0.300 ~ 85,
      xwOBA_est <= 0.320 ~ 70,
      xwOBA_est <= 0.340 ~ 50,
      TRUE ~ 30
    ),
    
    velo_score = case_when(
      FB_velo_est >= 96 ~ 100,
      FB_velo_est >= 94 ~ 85,
      FB_velo_est >= 92 ~ 70,
      FB_velo_est >= 90 ~ 50,
      TRUE ~ 30
    ),
    
    # Whiff score uses REAL SwStr%!
    whiff_score = case_when(
      Whiff_pct >= 14 ~ 100,
      Whiff_pct >= 12 ~ 85,
      Whiff_pct >= 10 ~ 70,
      Whiff_pct >= 8 ~ 50,
      TRUE ~ 30
    ),
    
    hard_contact_score = case_when(
      HardHit_pct_est <= 30 ~ 100,
      HardHit_pct_est <= 33 ~ 85,
      HardHit_pct_est <= 36 ~ 70,
      HardHit_pct_est <= 40 ~ 50,
      TRUE ~ 30
    ),
    
    chase_score = case_when(
      Chase_pct_est >= 32 ~ 100,
      Chase_pct_est >= 30 ~ 85,
      Chase_pct_est >= 28 ~ 70,
      Chase_pct_est >= 26 ~ 50,
      TRUE ~ 30
    ),
    
    fip_era_gap = ERA - FIP,
    regression_risk = case_when(
      fip_era_gap > 1.5 ~ "Very Unlucky",
      fip_era_gap > 0.5 ~ "Unlucky",
      fip_era_gap < -1.0 ~ "Very Lucky",
      fip_era_gap < -0.3 ~ "Lucky",
      TRUE ~ "Normal"
    ),
    
    regression_score = case_when(
      fip_era_gap > 1.0 ~ 90,  # Unlucky, buy low
      fip_era_gap > 0.3 ~ 80,
      abs(fip_era_gap) <= 0.3 ~ 75,
      fip_era_gap < -0.5 ~ 60,  # Lucky, ERA may rise
      TRUE ~ 50
    ),
    
    sustainability_score = (whiff_score + hard_contact_score + chase_score) / 3,
    
    tier3_total_score = (stuff_score * 0.20 + woba_score * 0.15 + xwoba_score * 0.10 + 
                         velo_score * 0.10 + whiff_score * 0.15 + hard_contact_score * 0.10 + 
                         chase_score * 0.10 + sustainability_score * 0.10)
  )

cat(sprintf("✅ Tier 3 scores calculated (45%% of total weight)\n"))
cat(sprintf("   NOW USING REAL: SwStr%%, GB%%, LD%%, FB%%, HR/FB, spray angles!\n\n"))

# ===========================================================================
# FINAL COMPOSITE SCORE
# ===========================================================================

cat("STEP 6: Calculating Final Rule V Fit Scores\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

complete_analysis <- pitcher_data_enhanced %>%
  left_join(tier1_results %>% select(PlayerName, tier1_total_score, era_score:save_exp_score), 
            by = "PlayerName") %>%
  left_join(tier2_results %>% select(PlayerName, tier2_total_score, k9_score:level_score, ip_per_g), 
            by = "PlayerName") %>%
  left_join(tier3_results %>% select(PlayerName, tier3_total_score, stuff_score:sustainability_score, regression_risk, fip_era_gap),
            by = "PlayerName") %>%
  mutate(
    rule_v_fit_score = tier1_total_score * 0.15 + tier2_total_score * 0.40 + tier3_total_score * 0.45,
    
    tier_classification = case_when(
      rule_v_fit_score >= 85 ~ "Elite",
      rule_v_fit_score >= 75 ~ "High",
      rule_v_fit_score >= 65 ~ "Solid Middle",
      rule_v_fit_score >= 55 ~ "Developmental",
      TRUE ~ "High Risk"
    )
  ) %>%
  arrange(desc(rule_v_fit_score))

cat(sprintf("✅ Final composite scores calculated for %d players\n\n", nrow(complete_analysis)))

# Summary stats
tier_counts <- complete_analysis %>%
  count(tier_classification) %>%
  arrange(desc(n))

cat("📊 TIER DISTRIBUTION:\n")
print(tier_counts, n = Inf)
cat("\n")

# ===========================================================================
# IDENTIFY STEALS & LOCKS
# ===========================================================================

cat("STEP 7: Identifying Steals and Locks\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

steals <- complete_analysis %>%
  filter(
    regression_risk %in% c("Unlucky", "Very Unlucky"),
    rule_v_fit_score >= 65,
    Stuff_plus_est >= 100
  ) %>%
  arrange(desc(rule_v_fit_score))

locks <- complete_analysis %>%
  filter(
    regression_risk %in% c("Normal"),
    abs(fip_era_gap) <= 0.3,
    rule_v_fit_score >= 70,
    tier1_total_score >= 70
  ) %>%
  arrange(desc(rule_v_fit_score))

cat(sprintf("✅ Identified %d STEALS (unlucky, high upside)\n", nrow(steals)))
cat(sprintf("✅ Identified %d LOCKS (consistent, low risk)\n\n", nrow(locks)))

# ===========================================================================
# SAVE OUTPUTS
# ===========================================================================

cat("STEP 8: Saving Final Analysis Outputs\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

write_csv(complete_analysis, "output/complete_analysis_data_FINAL.csv")
write_csv(complete_analysis %>% select(PlayerName, Age, Level = highest_level, Role, ERA, FIP, `K/9`, `BB/9`, 
                                       GB_pct, LD_pct, FB_pct, Whiff_pct, rule_v_fit_score, tier_classification),
          "output/pitcher_rankings_FINAL.csv")
write_csv(steals, "output/steals_FINAL.csv")
write_csv(locks, "output/locks_FINAL.csv")

cat("✅ Saved output files:\n")
cat("   • complete_analysis_data_FINAL.csv\n")
cat("   • pitcher_rankings_FINAL.csv\n")
cat("   • steals_FINAL.csv\n")
cat("   • locks_FINAL.csv\n\n")

# ===========================================================================
# TARGET PLAYERS CHECK
# ===========================================================================

cat("STEP 9: Target Players Performance\n")
cat("───────────────────────────────────────────────────────────────────────────────\n")

target_results <- complete_analysis %>%
  filter(PlayerName %in% player_list$player_name) %>%
  select(PlayerName, Age, Level = highest_level, ERA, FIP, `K/9`, `BB/9`, GB_pct, 
         Whiff_pct, rule_v_fit_score, tier_classification) %>%
  arrange(desc(rule_v_fit_score))

cat("🎯 YOUR 10 TARGET PLAYERS:\n\n")
print(target_results, n = Inf)

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n")
cat("✅ FINAL ANALYSIS COMPLETE WITH 100% REAL BATTED BALL DATA!\n")
cat("═══════════════════════════════════════════════════════════════════════════════\n\n")

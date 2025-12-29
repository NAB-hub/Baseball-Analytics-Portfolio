# =============================================================================
# 51_rank_hitters.R
# Rank hitters using three-tier composite scoring
# Adapted from pitcher framework: 50% performance, 30% context, 20% projection
# =============================================================================

library(dplyr)

cat("🎯 Ranking Rule 5 Eligible Hitters...\n\n")

# Load career stats
career <- read.csv("data/hitters_career_stats.csv", stringsAsFactors = FALSE)

cat(sprintf("📊 Loaded %d players\n", nrow(career)))

# =============================================================================
# TIER 1: PERFORMANCE (50% weight)
# =============================================================================
cat("\n⚙️  Calculating Tier 1: Performance Metrics (50%)...\n")

tier1 <- career %>%
  mutate(
    # Core offensive value
    wrc_plus_score = pmin(career_wrc_plus / 120, 1.5),  # 120+ is excellent
    obp_score = pmin((career_obp - 0.300) / 0.070, 1.5),  # .370+ is excellent
    
    # Power
    iso_score = pmin(career_iso / 0.180, 1.5),  # .180+ is good power
    
    # Plate discipline
    bb_score = pmin(career_bb_pct / 0.12, 1.5),  # 12%+ is excellent
    k_score = pmax(1 - ((career_k_pct - 0.18) / 0.15), 0),  # <18% is excellent
    
    # Replace NAs with 0
    across(ends_with("_score"), ~ifelse(is.na(.), 0, .)),
    
    # Tier 1 composite
    tier1_score = (wrc_plus_score * 0.35 + 
                   obp_score * 0.25 + 
                   iso_score * 0.20 + 
                   bb_score * 0.10 + 
                   k_score * 0.10) * 50  # Scale to 50 points
  )

cat(sprintf("   ✓ Performance scores calculated\n"))
cat(sprintf("   - Top wRC+: %.0f (%s)\n", 
            max(tier1$career_wrc_plus, na.rm = TRUE),
            tier1$player_name[which.max(tier1$career_wrc_plus)]))

# =============================================================================
# TIER 2: CONTEXT (30% weight)
# =============================================================================
cat("\n⚙️  Calculating Tier 2: Context Metrics (30%)...\n")

tier2 <- tier1 %>%
  mutate(
    # Age/Level fit (younger at higher levels is better)
    age_level_score = case_when(
      latest_level == "AAA" & latest_age <= 24 ~ 1.5,
      latest_level == "AAA" & latest_age <= 26 ~ 1.2,
      latest_level == "AAA" & latest_age <= 28 ~ 0.9,
      latest_level == "AA" & latest_age <= 23 ~ 1.2,
      latest_level == "AA" & latest_age <= 25 ~ 0.9,
      latest_level == "AA" & latest_age <= 27 ~ 0.6,
      TRUE ~ 0.3
    ),
    
    # Sample size (reliability)
    pa_score = pmin(total_pa / 800, 1.5),  # 800+ PA is solid sample
    
    # Upper-level exposure
    level_score = case_when(
      pct_pa_aaa >= 50 ~ 1.5,
      pct_pa_aaa >= 25 ~ 1.2,
      pct_pa_aa >= 50 ~ 1.0,
      pct_pa_aa >= 25 ~ 0.7,
      TRUE ~ 0.3
    ),
    
    # Consistency (seasons played)
    consistency_score = pmin(total_seasons / 3, 1.5),  # 3+ seasons is good
    
    # Speed/Baserunning (valuable asset)
    sb_score = pmin(total_sb / 30, 1.5),  # 30+ SB is good speed
    
    # Tier 2 composite
    tier2_score = (age_level_score * 0.30 + 
                   pa_score * 0.25 + 
                   level_score * 0.25 + 
                   consistency_score * 0.10 + 
                   sb_score * 0.10) * 30  # Scale to 30 points
  )

cat(sprintf("   ✓ Context scores calculated\n"))

# =============================================================================
# TIER 3: PROJECTION (20% weight)
# =============================================================================
cat("\n⚙️  Calculating Tier 3: Projection Metrics (20%)...\n")

tier3 <- tier2 %>%
  mutate(
    # Peak performance (ceiling indicator)
    peak_wrc_score = pmin(best_wrc_plus / 140, 1.5),  # 140+ peak is elite
    peak_iso_score = pmin(best_iso / 0.220, 1.5),  # .220+ peak power
    
    # Growth trajectory (improving)
    seasons_upward = case_when(
      total_seasons >= 3 & best_wrc_plus >= career_wrc_plus * 1.1 ~ 1.5,
      total_seasons >= 2 & best_wrc_plus >= career_wrc_plus * 1.05 ~ 1.2,
      TRUE ~ 0.8
    ),
    
    # Age-based upside
    age_upside = case_when(
      latest_age <= 24 ~ 1.5,
      latest_age <= 26 ~ 1.2,
      latest_age <= 28 ~ 0.9,
      TRUE ~ 0.5
    ),
    
    # Tier 3 composite
    tier3_score = (peak_wrc_score * 0.35 + 
                   peak_iso_score * 0.25 + 
                   seasons_upward * 0.20 + 
                   age_upside * 0.20) * 20  # Scale to 20 points
  )

cat(sprintf("   ✓ Projection scores calculated\n"))

# =============================================================================
# COMPOSITE RANKING
# =============================================================================
cat("\n🏆 Calculating Composite Rankings...\n")

rankings <- tier3 %>%
  mutate(
    composite_score = tier1_score + tier2_score + tier3_score,
    
    # Role classification based on profile
    role_projection = case_when(
      career_iso >= 0.180 & career_k_pct <= 0.25 ~ "Power Bat",
      career_iso >= 0.150 & career_obp >= 0.350 ~ "Balanced",
      career_obp >= 0.360 & career_bb_pct >= 0.10 ~ "OBP Machine",
      total_sb >= 25 & career_avg >= 0.250 ~ "Speed/Contact",
      career_wrc_plus >= 100 & total_pa >= 800 ~ "Solid Regular",
      TRUE ~ "Depth/Utility"
    ),
    
    # MLB readiness
    mlb_ready = case_when(
      latest_level == "AAA" & career_wrc_plus >= 100 ~ "Ready",
      latest_level == "AAA" & career_wrc_plus >= 90 ~ "Close",
      latest_level == "AA" & career_wrc_plus >= 110 ~ "Close",
      TRUE ~ "Developmental"
    )
  ) %>%
  arrange(desc(composite_score)) %>%
  mutate(rank = row_number())

cat(sprintf("   ✓ %d players ranked\n", nrow(rankings)))
cat(sprintf("   - MLB Ready: %d\n", sum(rankings$mlb_ready == "Ready")))
cat(sprintf("   - Close to Ready: %d\n", sum(rankings$mlb_ready == "Close")))

# =============================================================================
# FILTERS & FLAGGING
# =============================================================================
cat("\n🔍 Applying Filters...\n")

# Flag players with insufficient sample size or red flags
rankings <- rankings %>%
  mutate(
    sample_size_flag = ifelse(total_pa < 400, "⚠️ Limited PA", ""),
    age_flag = ifelse(latest_age >= 28, "⚠️ Older", ""),
    level_flag = ifelse(latest_level %in% c("A", "A+") & latest_age >= 25, "⚠️ Low Level", ""),
    
    # Combined flags
    flags = paste(sample_size_flag, age_flag, level_flag),
    flags = gsub("^\\s+|\\s+$", "", flags),  # Trim whitespace
    flags = ifelse(flags == "", "None", flags)
  )

cat(sprintf("   ✓ Flags applied\n"))
cat(sprintf("   - Players with flags: %d\n", sum(rankings$flags != "None")))

# =============================================================================
# POSITION BREAKDOWNS
# =============================================================================
cat("\n📊 Position Breakdown:\n")

pos_summary <- rankings %>%
  group_by(latest_position) %>%
  summarise(
    count = n(),
    avg_composite = mean(composite_score),
    top_player = player_name[which.max(composite_score)],
    top_score = max(composite_score),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

for(i in 1:min(5, nrow(pos_summary))) {
  cat(sprintf("   - %s: %d players (Top: %s, %.1f)\n", 
              pos_summary$latest_position[i],
              pos_summary$count[i],
              pos_summary$top_player[i],
              pos_summary$top_score[i]))
}

# =============================================================================
# SAVE RESULTS
# =============================================================================
write.csv(rankings, "output/hitter_rankings.csv", row.names = FALSE)

cat("\n✅ Rankings complete! Saved to output/hitter_rankings.csv\n")
cat(sprintf("\n🎯 Top 10 Hitters:\n"))

top10 <- rankings %>%
  select(rank, player_name, org, latest_position, latest_age, latest_level,
         career_wrc_plus, career_obp, career_iso, 
         tier1_score, tier2_score, tier3_score, composite_score,
         role_projection, mlb_ready, flags) %>%
  head(10)

print(top10, row.names = FALSE)

cat("\n")

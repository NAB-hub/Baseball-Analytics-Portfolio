# =============================================================================
# 51b_rank_hitters_v2.R
# REVISED Hitter Ranking - Learning from 2023-2024 Failed Rule 5 Picks
# 
# Key Changes:
# 1. Minimum threshold: 110 wRC+ at AAA (or 120+ at AA)
# 2. Heavy positional value multiplier (1B-only needs 130+ wRC+)
# 3. Age penalty past 26 (Bowman/Taylor/Cantrelle lesson)
# 4. Recent performance weighted 60% (not career stats)
# 5. MLB-readiness over upside/potential
# =============================================================================

library(dplyr)

cat("🎯 REVISED Hitter Ranking (Learning from Failed R5 Picks)...\n\n")

# Load data
career <- read.csv("data/hitters_career_stats.csv", stringsAsFactors = FALSE)
seasons <- read.csv("data/hitters_cleaned_seasons.csv", stringsAsFactors = FALSE)

cat(sprintf("📊 Loaded %d players\n", nrow(career)))

# =============================================================================
# STEP 1: CALCULATE RECENT PERFORMANCE (2025 weighted heavily)
# =============================================================================
cat("\n⚙️  Calculating Recent Performance (2025 = 60%, 2024 = 30%, Career = 10%)...\n")

# Get 2025 stats with LEVEL-SPECIFIC sample check
recent_2025_by_level <- seasons %>%
  filter(Season == 2025) %>%
  group_by(playerId, Level) %>%
  summarise(
    wrc_level = weighted.mean(wRC_plus, PA, na.rm = TRUE),
    iso_level = weighted.mean(ISO, PA, na.rm = TRUE),
    obp_level = weighted.mean(OBP, PA, na.rm = TRUE),
    k_pct_level = weighted.mean(K_pct, PA, na.rm = TRUE),
    pa_level = sum(PA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Only keep samples with 100+ PA at that specific level
  filter(pa_level >= 100)

# Aggregate 2025 across qualifying levels
recent_2025 <- recent_2025_by_level %>%
  group_by(playerId) %>%
  summarise(
    wrc_2025 = weighted.mean(wrc_level, pa_level, na.rm = TRUE),
    iso_2025 = weighted.mean(iso_level, pa_level, na.rm = TRUE),
    obp_2025 = weighted.mean(obp_level, pa_level, na.rm = TRUE),
    k_pct_2025 = weighted.mean(k_pct_level, pa_level, na.rm = TRUE),
    pa_2025 = sum(pa_level, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Must have 150+ total PA across qualifying levels
  filter(pa_2025 >= 150)

recent_2024 <- seasons %>%
  filter(Season == 2024) %>%
  group_by(playerId) %>%
  summarise(
    wrc_2024 = weighted.mean(wRC_plus, PA, na.rm = TRUE),
    iso_2024 = weighted.mean(ISO, PA, na.rm = TRUE),
    pa_2024 = sum(PA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pa_2024 >= 150)

# Merge recent performance
career_recent <- career %>%
  left_join(recent_2025, by = "playerId") %>%
  left_join(recent_2024, by = "playerId") %>%
  mutate(
    # Recency-weighted stats (2025 = 60%, 2024 = 30%, Career = 10%)
    # Only use 2025 if they have 150+ PA with 100+ at each level
    recent_wrc = case_when(
      !is.na(wrc_2025) & pa_2025 >= 150 ~ wrc_2025 * 0.6 + coalesce(wrc_2024, career_wrc_plus) * 0.3 + career_wrc_plus * 0.1,
      !is.na(wrc_2024) & pa_2024 >= 150 ~ wrc_2024 * 0.5 + career_wrc_plus * 0.5,
      TRUE ~ career_wrc_plus
    ),
    recent_iso = case_when(
      !is.na(iso_2025) & pa_2025 >= 150 ~ iso_2025 * 0.6 + coalesce(iso_2024, career_iso) * 0.3 + career_iso * 0.1,
      !is.na(iso_2024) & pa_2024 >= 150 ~ iso_2024 * 0.5 + career_iso * 0.5,
      TRUE ~ career_iso
    ),
    recent_obp = coalesce(obp_2025, career_obp),
    recent_k_pct = coalesce(k_pct_2025, career_k_pct)
  )

cat(sprintf("   ✓ Recent performance calculated\n"))

# =============================================================================
# STEP 2: HARD FILTERS (Learn from failures)
# =============================================================================
cat("\n🚨 Applying Hard Filters (Lessons from Failed Picks)...\n")

filtered <- career_recent %>%
  mutate(
    # Minimum performance thresholds by level
    meets_threshold = case_when(
      latest_level == "AAA" & recent_wrc >= 110 ~ TRUE,
      latest_level == "AAA" & recent_wrc >= 100 & latest_position %in% c("C", "SS", "CF") ~ TRUE,  # Premium positions get slight break
      latest_level == "AA" & recent_wrc >= 120 & latest_age <= 24 ~ TRUE,
      latest_level == "AA" & recent_wrc >= 130 & latest_age <= 26 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Age filters (Bowman/Taylor/Cantrelle were 25-26 and failed)
    age_acceptable = case_when(
      latest_age <= 25 ~ TRUE,
      latest_age <= 27 & recent_wrc >= 120 ~ TRUE,  # Elite hitters get pass
      TRUE ~ FALSE
    ),
    
    # Sample size minimum
    sample_acceptable = total_pa >= 400,
    
    # Combined filter
    passes_filters = meets_threshold & age_acceptable & sample_acceptable
  )

cat(sprintf("   - Started with: %d players\n", nrow(filtered)))
cat(sprintf("   - Meets performance threshold: %d\n", sum(filtered$meets_threshold)))
cat(sprintf("   - Age acceptable: %d\n", sum(filtered$age_acceptable)))
cat(sprintf("   - Sample size OK: %d\n", sum(filtered$sample_acceptable)))
cat(sprintf("   ✓ Pass all filters: %d players\n", sum(filtered$passes_filters)))

# Focus only on qualified players
qualified <- filtered %>% filter(passes_filters)

cat(sprintf("\n📉 Filtered from %d → %d qualified players\n", nrow(career), nrow(qualified)))

# =============================================================================
# STEP 3: POSITIONAL VALUE MULTIPLIER (Carson Taylor lesson)
# =============================================================================
cat("\n⚙️  Calculating Positional Value...\n")

qualified <- qualified %>%
  mutate(
    position_value = case_when(
      # Premium positions (scarce, high value)
      latest_position == "C" ~ 1.20,
      latest_position == "SS" ~ 1.15,
      latest_position == "CF" ~ 1.10,
      
      # Middle infield / multi-position
      latest_position == "2B" ~ 1.05,
      latest_position %in% c("3B", "UTL") ~ 1.08,
      
      # Corner OF
      latest_position == "OF" ~ 1.00,
      
      # Corner infield (needs elite bat)
      latest_position == "1B" & recent_wrc >= 130 ~ 1.00,
      latest_position == "1B" & recent_wrc < 130 ~ 0.85,  # PENALTY for weak-hitting 1B
      
      TRUE ~ 0.95
    ),
    
    # Extra bonus for true utility
    utility_bonus = ifelse(latest_position == "UTL", 1.05, 1.00)
  )

cat(sprintf("   ✓ Positional values assigned\n"))

# =============================================================================
# STEP 4: SIMPLIFIED PERFORMANCE SCORE (50 points)
# =============================================================================
cat("\n⚙️  Tier 1: Performance Score (50 points)...\n")

tier1 <- qualified %>%
  mutate(
    # Core metrics with realistic scaling
    wrc_score = pmin((recent_wrc - 100) / 30, 1.5),  # 130+ wRC is elite
    iso_score = pmin(recent_iso / 0.180, 1.5),       # .180+ ISO is good power
    k_score = pmax(1.2 - (recent_k_pct / 0.20), 0),  # K% penalty (20% is average)
    obp_score = pmin((recent_obp - 0.320) / 0.060, 1.5),  # .380+ OBP is elite
    
    # Performance composite
    tier1_score = (wrc_score * 0.45 + 
                   iso_score * 0.25 + 
                   obp_score * 0.15 + 
                   k_score * 0.15) * 50
  )

cat(sprintf("   ✓ Performance scores calculated\n"))

# =============================================================================
# STEP 5: MLB READINESS SCORE (30 points)
# =============================================================================
cat("\n⚙️  Tier 2: MLB Readiness (30 points)...\n")

tier2 <- tier1 %>%
  mutate(
    # AAA performance heavily weighted
    aaa_score = case_when(
      latest_level == "AAA" & pct_pa_aaa >= 50 ~ 1.5,
      latest_level == "AAA" & pct_pa_aaa >= 25 ~ 1.2,
      latest_level == "AA" & pct_pa_aa >= 75 ~ 0.8,
      latest_level == "AA" & has_aaa ~ 0.9,
      TRUE ~ 0.3
    ),
    
    # Age/level fit (ready NOW, not potential)
    age_level_score = case_when(
      latest_level == "AAA" & latest_age <= 24 ~ 1.5,
      latest_level == "AAA" & latest_age <= 26 ~ 1.3,
      latest_level == "AAA" & latest_age <= 27 ~ 1.0,
      latest_level == "AA" & latest_age <= 23 ~ 1.1,
      latest_level == "AA" & latest_age <= 25 ~ 0.9,
      TRUE ~ 0.5
    ),
    
    # Recent form (2025 performance)
    form_score = case_when(
      !is.na(wrc_2025) & wrc_2025 >= 120 ~ 1.5,
      !is.na(wrc_2025) & wrc_2025 >= 100 ~ 1.2,
      !is.na(wrc_2025) & wrc_2025 >= 90 ~ 0.9,
      !is.na(wrc_2024) & wrc_2024 >= 110 ~ 1.0,
      TRUE ~ 0.7
    ),
    
    # Sample size reliability
    reliability_score = pmin(total_pa / 800, 1.5),
    
    # Tier 2 composite
    tier2_score = (aaa_score * 0.35 + 
                   age_level_score * 0.25 + 
                   form_score * 0.25 + 
                   reliability_score * 0.15) * 30
  )

cat(sprintf("   ✓ Readiness scores calculated\n"))

# =============================================================================
# STEP 6: UPSIDE/CEILING SCORE (20 points)
# =============================================================================
cat("\n⚙️  Tier 3: Upside/Ceiling (20 points)...\n")

tier3 <- tier2 %>%
  mutate(
    # Peak performance ceiling
    peak_score = pmin((best_wrc_plus - 100) / 50, 1.5),  # 150+ peak is elite
    peak_power_score = pmin(best_iso / 0.220, 1.5),      # .220+ peak power
    
    # Growth/trajectory (improving?)
    trajectory_score = case_when(
      !is.na(wrc_2025) & wrc_2025 > career_wrc_plus * 1.1 ~ 1.5,  # Improving
      !is.na(wrc_2025) & wrc_2025 > career_wrc_plus ~ 1.2,
      TRUE ~ 0.8
    ),
    
    # Control years (younger = more control)
    control_score = case_when(
      latest_age <= 24 ~ 1.5,
      latest_age <= 26 ~ 1.2,
      latest_age <= 27 ~ 0.9,
      TRUE ~ 0.5
    ),
    
    # Tier 3 composite
    tier3_score = (peak_score * 0.30 + 
                   peak_power_score * 0.25 + 
                   trajectory_score * 0.25 + 
                   control_score * 0.20) * 20
  )

cat(sprintf("   ✓ Upside scores calculated\n"))

# =============================================================================
# STEP 7: COMPOSITE RANKING WITH POSITIONAL ADJUSTMENT
# =============================================================================
cat("\n🏆 Calculating Final Composite Rankings...\n")

rankings <- tier3 %>%
  mutate(
    # Base composite
    base_score = tier1_score + tier2_score + tier3_score,
    
    # Apply positional multiplier
    composite_score = base_score * position_value * utility_bonus,
    
    # Role classification
    role_projection = case_when(
      recent_iso >= 0.200 & recent_k_pct <= 0.25 ~ "Power Bat",
      recent_iso >= 0.160 & recent_wrc >= 115 ~ "Balanced Hitter",
      recent_obp >= 0.360 & career_bb_pct >= 0.10 ~ "OBP/Contact",
      total_sb >= 20 & recent_wrc >= 100 ~ "Speed Tool",
      latest_position %in% c("C", "SS", "CF", "UTL") ~ "Glove-First UTL",
      TRUE ~ "Bench Bat"
    ),
    
    # MLB readiness classification
    mlb_ready = case_when(
      latest_level == "AAA" & recent_wrc >= 115 ~ "MLB Ready",
      latest_level == "AAA" & recent_wrc >= 105 ~ "Near Ready",
      latest_level == "AA" & recent_wrc >= 130 & latest_age <= 24 ~ "High Upside",
      TRUE ~ "Developmental"
    )
  ) %>%
  arrange(desc(composite_score)) %>%
  mutate(rank = row_number())

cat(sprintf("   ✓ %d players ranked\n", nrow(rankings)))

# =============================================================================
# STEP 8: FLAG POTENTIAL ISSUES
# =============================================================================
cat("\n🔍 Flagging Concerns...\n")

rankings <- rankings %>%
  mutate(
    # Red flags
    flag_age = ifelse(latest_age >= 27, "⚠️ Age 27+", ""),
    flag_level = ifelse(latest_level %in% c("A", "A+"), "⚠️ Low Level", ""),
    flag_sample = ifelse(total_pa < 600, "⚠️ Limited PA", ""),
    flag_position = ifelse(latest_position == "1B" & recent_wrc < 130, "⚠️ Weak 1B Bat", ""),
    flag_decline = ifelse(!is.na(wrc_2025) & wrc_2025 < career_wrc_plus * 0.85, "⚠️ Declining", ""),
    
    # Combine flags
    all_flags = paste(flag_age, flag_level, flag_sample, flag_position, flag_decline),
    all_flags = gsub("\\s+", " ", all_flags),
    all_flags = trimws(all_flags),
    flags = ifelse(all_flags == "", "None", all_flags)
  )

cat(sprintf("   ✓ Flags applied\n"))

# =============================================================================
# STEP 9: CHECK FAILED R5 PICKS
# =============================================================================
cat("\n📋 Checking Failed 2023-2024 Rule 5 Picks...\n")

failed_r5 <- c("Carson Taylor", "Cooper Bowman", "Hayden Cantrelle", "Clay Dungan", 
               "Garrett Spain", "Jose Torres", "Lizandro Espinoza", "Luis Caicuto", 
               "Miguel Ugueto")

failed_check <- rankings %>%
  filter(player_name %in% failed_r5) %>%
  select(rank, player_name, composite_score, mlb_ready, flags)

if(nrow(failed_check) > 0) {
  cat("\n⚠️  Failed R5 picks in our rankings:\n")
  print(failed_check, row.names = FALSE)
} else {
  cat("   ✓ No failed picks in top ranks (good!)\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================
write.csv(rankings, "output/hitter_rankings_v2.csv", row.names = FALSE)

cat("\n✅ Rankings complete! Saved to output/hitter_rankings_v2.csv\n")

# =============================================================================
# DISPLAY TOP 15
# =============================================================================
cat(sprintf("\n🎯 Top 15 Rule 5 Hitter Candidates:\n\n"))

top15 <- rankings %>%
  select(rank, player_name, org, latest_position, latest_age, latest_level,
         recent_wrc, recent_iso, composite_score, position_value,
         role_projection, mlb_ready, flags) %>%
  head(15)

print(top15, row.names = FALSE)

cat("\n📊 Summary Statistics:\n")
cat(sprintf("   - Total qualified: %d\n", nrow(rankings)))
cat(sprintf("   - MLB Ready: %d\n", sum(rankings$mlb_ready == "MLB Ready")))
cat(sprintf("   - Near Ready: %d\n", sum(rankings$mlb_ready == "Near Ready")))
cat(sprintf("   - Average wRC+ (top 15): %.0f\n", mean(top15$recent_wrc)))
cat(sprintf("   - Average age (top 15): %.1f\n", mean(top15$latest_age)))
cat(sprintf("   - Premium positions (top 15): %d\n", sum(top15$latest_position %in% c("C", "SS", "CF", "2B", "UTL"))))

cat("\n")
